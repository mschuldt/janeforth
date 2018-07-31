
: 2drop inline drop inline drop ;code

: c@++ inline dup inline 1+ inline swap inline c@ ;code

: TEST
	." 2drop: "	1 2 3 4 2drop . . cr

	s" testing" drop
	c@++ emit cr
	c@++ emit cr
	c@++ emit cr
	c@++ emit cr
	c@++ emit cr
	c@++ emit cr
	c@++ emit cr
	drop
;
