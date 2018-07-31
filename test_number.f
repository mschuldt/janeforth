
: TEST
	123                                . cr
	[ hex -7F ] literal      decimal   . cr
	[ hex 7FF77FF7 ] literal hex       . cr
	[ hex -7FF77FF7 ] literal 2 base ! . cr
	[ 2 base ! 1111111111101110111111111110111 ] literal hex . cr
;

decimal ( restore immediate-mode base )
