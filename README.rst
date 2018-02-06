castxml --castxml-gccxml /usr/include/exempi-2.0/exempi/xmp.h 
guile -e main -s castxml-to-gw.scm xmp xmp.xml exempi > exempi-spec.scm
guile -e main -s call-gwrap.scm xmp exempi
