(use-modules (ice-9 match))

(define (main args)
  (match
   args
   ((this-file file-base prefix lib-link)
    (load (string-append prefix "-spec.scm"))
    (generate-wrapset 'guile
                      (string->symbol (string-append file-base "-wrapset"))
                      (string-append file-base "-wrapset"))
    (compile-wrapset file-base lib-link))))

(define (compile-wrapset file-base lib-link)
  (let ((gcc
         (format
          #f
          "gcc -Wno-undef -shared -o libgw-guile-~a-wrapset.so -fPIC ~a-wrapset.c `pkg-config --cflags --libs guile-2.0` ~a -lgwrap-core-runtime -lgwrap-guile-runtime" file-base file-base lib-link)))
    (display gcc)(newline)
    (system gcc)))

