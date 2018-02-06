castxml --castxml-gccxml /usr/include/snappy-c.h
guile -e main -s ../castxml-to-gw.scm snappy-c snappy-c.xml snappy-c.h > snappy-spec.scm
guile -e main -s ../call-gwrap.scm snappy-c test/snappy "-lsnappy"
