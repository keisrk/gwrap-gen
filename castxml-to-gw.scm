(use-modules (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 receive)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-9)
             (sxml simple)
             (sxml match)
             (sxml fold)
             ((sxml xpath)
              #:prefix x:))

(define (dedup x)
  (match x
         (() #nil)
         ((l . ls) (cons l (filter (lambda (y) (not (equal? l y))) (dedup ls))))))

(define (attr-find key x)
  (match
   x
   (() #f)
   ((and ((k v) . kvs) (? (lambda (_) (equal? key k)))) v)
   (((_ _) . kvs) (attr-find key kvs))))

;; Init
(define-record-type <layout-init>
  (make-layout-init file-base file-id id-table fn-table en-table)
  layout-init?
  (file-base layout-init-fb set-layout-init-fb!)
  (file-id layout-init-fid set-layout-init-fid!)
  (id-table layout-init-idtbl set-layout-init-idtbl!)
  (fn-table layout-init-fntbl set-layout-init-fntbl!)
  (en-table layout-init-entbl set-layout-init-entbl!))

;; <layout-init> -> id -> record -> unit
(define (set-idtbl! layout-init id record)
  (set-layout-init-idtbl! layout-init
                     (acons id record (layout-init-idtbl layout-init))))

;; <layout-init> -> id -> typeinfo
;; typeinfo ::= <string> ;; , i.e., FundamentalType 
;;            | ('pointer typeinfo)
;;            | ('mchars)
;;            | ('defined     (@ (name name) (file fileid)) typeinfo)
;;            | ('enumeration (@ (name name) (file fileid)))
;;            | ('struct      (@ (name name) (file fileid)) typeinfo ...)
;;            | ('field | 'array | 'union | 'function-type ...)

(define (get-idtbl layout-init id)
  (let ((sxml (assoc-ref (layout-init-idtbl layout-init) id)))
    (sxml-match
     sxml
     [(FundamentalType (@ . ,attr)) (assoc-ref standard-tbl (attr-find 'name attr))]
     [(PointerType (@ . ,attr)) (let* ((type (attr-find 'type attr))
                                       (pointee (get-idtbl layout-init type)))
                                  (if (equal? pointee "char") (list 'mchars)
                                      (list 'pointer pointee)))]
     [(Typedef (@ . ,attr)) (let ((name (attr-find 'name attr))
                                  (file (attr-find 'file attr))
                                  (type (get-idtbl layout-init (attr-find 'type attr))))
                               (list 'defined
                                     (list '@ (list 'name name) (list 'file file))
                               type))]
     [(Enumeration (@ . ,attr)) (let* ((name-or-empty (attr-find 'name attr))
                                       (name (if (equal? name-or-empty "") "int"
                                                 name-or-empty))
                                       (file (attr-find 'file attr)))
                                  (list 'enumeration
                                   (list '@ (list 'name name) (list 'file file))))]
     [(Struct (@ . ,attr)) (let* ((name-or-empty (attr-find 'name attr))
                                 (name (if (equal? name-or-empty "") "anonymous-struct"
                                           name-or-empty))
                                 (file (attr-find 'file attr))
                                 (members (attr-find 'members attr)))
                              (list 'struct
                                    (list '@ (list 'name name) (list 'file file))
                              members))]
     [(CvQualifiedType (@ . ,attr)) (get-idtbl layout-init (attr-find 'type attr))]
     [(Field (@ . ,attr)) (let ((name (attr-find 'name attr))
                                (file (attr-find 'file attr))
                                (context (attr-find 'context attr))
                                (type (get-idtbl layout-init (attr-find 'type attr))))
                             (list 'field
                                   (list '@ (list 'name name) (list 'file file) (list 'context context))
                             type))]
     [(ArrayType (@ . ,attr)) (let ((type (get-idtbl layout-init (attr-find 'type attr))))
                                (cons 'array type))]
     [(Union (@ . ,attr)) (let ((name (attr-find 'name attr))
                                (file (attr-find 'file attr))
                                (members (attr-find 'members attr)))
                             (list 'union
                                   (list '@ (list 'name name) (list 'file file))
                             members))]
     [(FunctionType (@ . ,attr)) (let ((returns (attr-find 'returns attr)))
                            (list 'function-type
                                   (list '@ (list 'returns returns))))]
     [,otherwise #f])))

(define (c-type t)
  (sxml-match
   t
   ["bool" "int"]
   ["unsigned-int" "unsigned int"]
   [,s (guard (string? s)) s]
   [(defined (@ . ,attr) ,s) (guard (string? s)) s]
   [(defined (@ . ,attr) . ,args) (attr-find 'name attr)]
   [(mchars) "char *"]
   [(pointer (defined (@ . ,attr) (enumeration ,args) . ,_)) "int *"]
   [(pointer (defined (@ . ,attr) (struct ,args) . ,_)) "void *"]
   [(pointer (defined (@ . ,attr) . ,args)) (string-append (attr-find 'name attr) " *")]
   [(pointer ,s) (guard (string? s)) (string-append (c-type s) " *")]
   [(pointer ,otherwise) (c-type '(pointer "void"))]
   [,othewise #f]))

(define (scm-type t)
  (sxml-match
   t
   [,s (guard (string? s)) (string-downcase s)]
   [(defined (@ . ,attr) ,s) (guard (string? s)) s]
   [(defined (@ . ,attr) (enumeration (@ . , enum-attr) . ,enum-args) . ,rest)
    (string-downcase (attr-find 'name enum-attr))]
   [(defined (@ . ,attr) . ,args)
    (let ((name (string-downcase (attr-find 'name attr))))
      (format #f "<~a>" name))]
   [(mchars) t]
   [(pointer (defined (@ . ,attr) . ,args))
    (let ((name (string-append (string-downcase (attr-find 'name attr)) "-ptr")))
      (format #f "<~a>" name))]
   [(pointer ,s) (guard (string? s))
    (let ((name (string-append (string-downcase s) "-ptr")))
      (format #f "<~a>" name))]
   [(pointer ,otherwise) (scm-type '(pointer "void"))]
   [,othewise #f]))

(define standard-tbl
  '(
    ("__int128"                 . "int")         
    ("unsigned __int128"        . "unsigned-int")
    ("long unsigned int"        . "unsigned-int")
    ("int"                      . "int")
    ("double"                   . "double")
    ("long int"                 . "int")
    ("long long int"            . "int")
    ("float"                    . "float")
    ("long double"              . "double")
    ("long long unsigned int"   . "unsigned-int")
    ("unsigned char"            . "char")
    ("short unsigned int"       . "unsigned-int")
    ("unsigned int"             . "unsigned-int")
    ("signed char"              . "char")
    ("short int"                . "int")
    ("void"                     . "void")
    ("_Bool"                    . "bool")
    ("char"                     . "char")
    ("size_t"             . "unsigned-int")
    ))

;; <layout-init> -> id -> record -> unit
(define (set-fntbl! layout-init id record)
  (set-layout-init-fntbl! layout-init
                     (acons id record (layout-init-fntbl layout-init))))

(define (get-funcinfo layout-init x)
  (sxml-match
   x
   [(Function (@ . ,attr) . ,args)
    (let* ((name (attr-find 'name attr))
           (file (attr-find 'file attr))
           (rtn (attr-find 'returns attr))
           (returns (get-idtbl layout-init rtn))
           (arg-list ((x:filter (x:node-typeof? 'Argument)) args))
           (arguments (map (lambda (x) (get-funcinfo layout-init x)) arg-list)))
      (append
       (list 'function
             (list '@ (list 'name name)
                      (list 'file file)
                      (list 'returns returns)))
       arguments))]
    [(Argument (@ . ,attr) . ,args) (let* ((name-or-empty (attr-find 'name attr))
                                           (name (if (not name-or-empty) "argument" name-or-empty))
                                           (t (attr-find 'type attr))
                                           (type (get-idtbl layout-init t)))
                                     (list 'argument
                                           (list '@ (list 'name name)
                                                    (list 'type type))))]
     [, otherwise #f]))

(define (get-fntbl layout-init id)
  (let ((sxml (assoc-ref (layout-init-fntbl layout-init) id)))
    (get-funcinfo layout-init sxml)))

(define (set-entbl! layout-init id record)
  (set-layout-init-entbl! layout-init
                     (acons id record (layout-init-entbl layout-init))))

(define (get-enuminfo layout-init x)
  (sxml-match
   x
   [(Enumeration (@ . ,attr) . ,args) (let* ((id (attr-find 'id attr))
                                             (name-or-empty (attr-find 'name attr))
                                             (name (if (equal? name-or-empty "") (string-append "anonymous" id)
                                                       name-or-empty))
                                             (c-name (if (equal? name-or-empty "") "int"
                                                       name-or-empty))
                                             (file (attr-find 'file attr))
                                             (en-values ((x:filter (x:node-typeof? 'EnumValue)) args))
                                             (values (map (lambda (x) (get-enuminfo layout-init x)) en-values)))
                                        (append (list 'enumeration
                                                      (list '@ (list 'name name)
                                                               (list 'c-name c-name)
                                                               (list 'file file)))
                                                      values))]
   [(EnumValue (@ . ,attr)) (let ((name (attr-find 'name attr))
                                  (value (attr-find 'init attr)))
                              (cons name value))]))

(define (get-entbl layout-init id)
  (let ((sxml (assoc-ref (layout-init-entbl layout-init) id)))
    (get-enuminfo layout-init sxml)))

(define (idtbl-post-handler tag params layout-init klayout-init kids)
  (let* ((attr (car params))
         (id (attr-find 'id attr))
         (record (list tag (cons '@ attr)))
         (node (append (list tag (cons '@ attr)) kids)))
    (set-idtbl! layout-init id record)
    (values klayout-init node)))

(define (fntbl-post-handler tag params layout-init klayout-init kids)
  (let* ((attr (car params))
         (id (attr-find 'id attr))
         (node (append (list tag (cons '@ attr)) kids)))
    (set-fntbl! layout-init id node)
    (values klayout-init node)))

(define (entbl-post-handler tag params layout-init klayout-init kids)
  (let* ((attr (car params))
         (id (attr-find 'id attr))
         (record (list tag (cons '@ attr)))
         (node (append (list tag (cons '@ attr)) kids)))
    (set-idtbl! layout-init id record)
    (set-entbl! layout-init id node)
    (values klayout-init node)))

(define (file-post-handler tag params layout-init klayout-init kids)
  (let* ((attr (car params))
         (file-name (string-append (layout-init-fb layout-init) ".h"))
         (file-path (attr-find 'name attr))
         (file-id (attr-find 'id attr)))
    (if (string-contains file-path file-name)
        (set-layout-init-fid! layout-init file-id))
    (values klayout-init #f)))

(define bindings-init
  `((FundamentalType
     (post . ,idtbl-post-handler))
    (Enumeration
     (post . ,entbl-post-handler))
    (PointerType
     (post . ,idtbl-post-handler))
    (Typedef
     (post . ,idtbl-post-handler))
    (Struct
     (post . ,idtbl-post-handler))
    (Field
     (post . ,idtbl-post-handler))
    (Union
     (post . ,idtbl-post-handler))
    (ArrayType
     (post . ,idtbl-post-handler))
    (FunctionType
     (post . ,idtbl-post-handler))
    (CvQualifiedType
     (post . ,idtbl-post-handler))
    (Function
     (post . ,fntbl-post-handler))
    (File
     (post . ,file-post-handler))
     ))

;; Conversion
(define-record-type <layout-conv>
  (make-layout-conv fileid typeinfo funcinfo enuminfo)
  layout-conv?
  (fileid layout-conv-fileid set-layout-conv-fileid!)
  (typeinfo layout-conv-typeinfo set-layout-conv-typeinfo!)
  (funcinfo layout-conv-funcinfo set-layout-conv-funcinfo!)
  (enuminfo layout-conv-enuminfo set-layout-conv-enuminfo!))

;; <layout-conv> -> <layout-init> -> unit
(define (set-fileid! layout-conv layout-init)
  (set-layout-conv-fileid! layout-conv (layout-init-fid layout-init)))

(define (set-typeinfo! layout-conv layout-init)
  (for-each (lambda (rc)
              (let* ((id (car rc))
                     (typeinfo (get-idtbl layout-init id)))
                (set-layout-conv-typeinfo!
                 layout-conv
                 (acons id typeinfo (layout-conv-typeinfo layout-conv)))))
            (layout-init-idtbl layout-init)))

(define (set-funcinfo! layout-conv layout-init)
  (for-each (lambda (rc)
              (let* ((id (car rc))
                     (funcinfo (get-fntbl layout-init id)))
                (set-layout-conv-funcinfo!
                 layout-conv
                 (acons id funcinfo (layout-conv-funcinfo layout-conv)))))
            (layout-init-fntbl layout-init)))

(define (set-enuminfo! layout-conv layout-init)
  (for-each (lambda (rc)
              (let* ((id (car rc))
                     (enuminfo (get-entbl layout-init id)))
                (set-layout-conv-enuminfo!
                 layout-conv
                 (acons id enuminfo (layout-conv-enuminfo layout-conv)))))
            (layout-init-entbl layout-init)))

(define (miscinfo-dump layout-conv)
  (define (ms-dump fid x)
    (sxml-match
     x
     [(function (@ (name ,name) (file ,file) (returns ,returns)) . ,args)
      (if (equal? fid file)
          (let* ((arguments-clsr (lambda (kv) (ms-dump fid kv)))
                 (arguments (filter-map arguments-clsr args))
                 (p-rtn (sxml-match
                         returns
                         [(pointer (defined (@ (file , file)) . ,_)) returns]
                         [(pointer ,s) (guard (string? s)) returns]
                         [(pointer ,_) '(pointer "void")]
                         [,otherwise #f])))
            (case p-rtn
              ((#f) (if (null? arguments) #f arguments))
              (else (if (null? arguments) (list p-rtn) (cons p-rtn arguments)))))
          #f)]
     [(argument (@ (name ,name) (type ,type)))
      (sxml-match
       type
       [(pointer (defined (@ (file , file)) . ,_)) type]
       [(pointer ,s) (guard (string? s)) type]
       [(pointer ,_) '(pointer "void")]
       [,otherwise #f])]
     [,otherwise #f]))
  (map sxp-wrap-as-wct
       (dedup
        (fold append #nil
              (filter-map (lambda (x) (ms-dump (layout-conv-fileid layout-conv) (cdr x)))
                          (layout-conv-funcinfo layout-conv))))))
  
(define (load-conv layout-init)
  (let ((layout-conv (make-layout-conv #f #nil #nil #nil)))
    (set-fileid! layout-conv layout-init)
    (set-typeinfo! layout-conv layout-init)
    (set-funcinfo! layout-conv layout-init)
    (set-enuminfo! layout-conv layout-init)
    layout-conv))

;; G-Wrap code generation
(define (sxp-wrap-as-wct type)
  (let* ((scm-name (match (scm-type type)
                          (('mchars) '(list 'mchars 'caller-owned))
                          ((? string? s) `(string->symbol ,s))))
         (c-name (c-type type)))
    (list 'wrap-as-wct! 'ws
          '#:name scm-name
          '#:c-type-name c-name
          '#:c-const-type-name (string-append "const " c-name)
          '#:description "Automatically generated.")))

(define (sxp-wrap-function name rtn c-name args)
  (let ((returns (match (scm-type rtn)
                        ((? string? s) `(string->symbol ,s))
                        (('mchars) '(list (string->symbol "mchars")
                                          (string->symbol "caller-owned")))))
        (arguments (match args
                          (() '(list (list (string->symbol "void")
                                           (string->symbol "x"))))
                          (_ `(list ,@args)))))
    (list 'wrap-function! 'ws
          '#:name `(string->symbol ,name)
          '#:returns returns
          '#:c-name c-name
          '#:arguments arguments
          '#:description "Automatically generated.")))

(define (sxp-wrap-argument name type)
  (let ((arg-name (if (equal? "" name) "x" name))
        (type-name (match (scm-type type)
                          ((? string? s) `(string->symbol ,s))
                          (('mchars) '(list (string->symbol "mchars")
                                            (string->symbol "caller-owned"))))))
    `(list ,type-name (string->symbol ,arg-name))))

(define (sxp-wrap-enumeration name c-type-name values)
  (list 'wrap-enum! 'ws
        '#:name `(string->symbol ,(scm-type name))
        '#:c-type-name c-type-name
        '#:values `(list ,@values)
        '#:description "Automatically generated."))

(define (sxp-wrap-enum-value name value)
  `(cons (string->symbol ,name) ,value))

;; Dump <layout-conv> -> sxp
(define (typeinfo-dump layout-conv)
  (define (tp-dump fid x)
    (sxml-match
     x
     [(defined (@ (name ,name) (file ,file)) (struct . ,_) . ,args) #f]
     [(defined (@ (name ,name) (file ,file)) (enumeration . ,_) . ,args) #f]
     [(defined (@ (name ,name) (file ,file)) . ,args)
      (if (equal? fid file)
          (sxp-wrap-as-wct x) #f)]
     [,otherwise #f]))
  (dedup
   (filter-map (lambda (x) (tp-dump (layout-conv-fileid layout-conv) (cdr x)))
               (layout-conv-typeinfo layout-conv))))

(define (funcinfo-dump layout-conv)
  (define (fn-dump fid x)
    (sxml-match
     x
     [(function (@ (name ,name) (file ,file) (returns ,returns)) . ,args)
      (if (equal? fid file)
          (let* ((arguments-clsr (lambda (kv) (fn-dump fid kv)))
                 (arguments (map arguments-clsr args)))
            (sxp-wrap-function name returns name arguments))
          #f)]
     [(argument (@ (name ,name) (type ,type))) (sxp-wrap-argument name type)]
     [,otherwise #f]))
  (dedup
   (filter-map (lambda (x) (fn-dump (layout-conv-fileid layout-conv) (cdr x)))
               (layout-conv-funcinfo layout-conv))))

(define (enuminfo-dump layout-conv)
  (define (en-dump fid x)
    (sxml-match
     x
     [(enumeration (@ (name ,name) (c-name ,c-name) (file ,file)) . ,args)
      (if (equal? fid file)
          (let* ((values-clsr (lambda (kv) (sxp-wrap-enum-value (car kv) (cdr kv))))
                 (values (map values-clsr args)))
            (sxp-wrap-enumeration name c-name values))
          #f)]
     [,otherwise #f]))
  (dedup
   (filter-map (lambda (x) (en-dump (layout-conv-fileid layout-conv) (cdr x)))
               (layout-conv-enuminfo layout-conv))))

;; G-Wrap code generation
(define (ws-prelude)
;;    `(define-module (,(string->symbol mod-name) gw ,(string->symbol (format #f "~a-spec" mod-name)))
;;       #:use-module (oop goops)                   
;;       #:use-module (g-wrap)
;;       #:use-module (g-wrap c-codegen)
;;       #:use-module (g-wrap c-types)
;;       #:use-module (g-wrap enumeration)
;;       #:use-module (g-wrap guile)
;;       #:use-module (g-wrap guile ws standard)
;;       #:export (,(string->symbol (format #f "<~a-wrapset>" ws-name)))))

  `(use-modules (oop goops)
                (g-wrap)
                (g-wrap c-codegen)
                (g-wrap c-types)
                (g-wrap enumeration)
                (g-wrap guile)
                (g-wrap guile ws standard)))




  
(define (ws-class name)
  `(define-class ,(string->symbol (format #f "<~a-wrapset>" name)) (<gw-guile-wrapset>)
         #:id (string->symbol ,(string-append name "-wrapset"))
         #:dependencies '(standard)))

(define (ws-decl name header-path)
  `(define-method (global-declarations-cg
                   (ws ,(string->symbol (format #f "<~a-wrapset>" name))))
     (list
      (next-method)
      ,(format #f "#include <~a>\n" header-path))))
 
(define (ws-init name t-info m-info e-info f-info)
  `(define-method (initialize
                   (ws ,(string->symbol (format #f "<~a-wrapset>" name))) initargs)
     (next-method
      ws
      (append (list '#:module (list (string->symbol ,(format #f "lib~a" name))))
              initargs))
     ,@t-info
     ,@m-info
     ,@e-info
     ,@f-info
     ))

;; Defaults
(define (pass-post-handler tag params lo klo kids)
  (let ((attr (car params)))
    (if (null? attr)
        (values klo (cons tag kids))
        (values klo (append (list tag (cons '@ attr)) kids)))))
(define (text-handler text params lo) (values lo text))
(define bindings
  `((*default* . ,pass-post-handler)
    (*text* . ,text-handler)))
(define *default-params* #nil)
(define *default-stylesheet* #nil)

(define (gwrap-gen file-base xml-path header-path)
  (let* ((layout-init (make-layout-init file-base "" #nil #nil #nil))
         (sxml (call-with-input-file xml-path
                 (lambda (port)
                   (xml->sxml port)))))
    (receive (tree lo)
             (fold-layout sxml (append bindings-init bindings) *default-params* layout-init *default-stylesheet*)
             (let ((layout-conv (load-conv lo)))
               (pretty-print (ws-prelude))
               (pretty-print (ws-class file-base))
               (pretty-print (ws-decl file-base header-path))
               (pretty-print (ws-init file-base
                                      (typeinfo-dump layout-conv)
                                      (miscinfo-dump layout-conv)
                                      (enuminfo-dump layout-conv)
                                      (funcinfo-dump layout-conv)))
))))

(define (main args)
  (match args
         ((this-file file-base xml-path header-path) (gwrap-gen file-base xml-path header-path))))
