
: TEST4 print-stack-trace throw ;

: TEST3 0 TEST4 26 TEST4 ;

: TEST2
	['] TEST3 catch
	?dup if ." TEST3 threw exception " . cr then
	TEST3
;

: TEST TEST2 ;
