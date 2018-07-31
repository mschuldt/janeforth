
: TEST4 print-stack-trace ;

: TEST3 TEST4 1 2 + . cr TEST4 ;

: TEST2 TEST3 TEST3 ;

: TEST TEST2 ;
