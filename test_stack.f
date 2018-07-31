
: TEST
	depth . cr

	42 dup . . cr
	23 drop depth . cr
	1 2 swap . . cr
	1 2 over . . . cr
	1 2 3 -rot . . . cr
	1 2 3 rot . . . cr
	1 2 3 4 2drop . . cr
	1 2 3 4 2dup . . . . . . cr
	1 2 3 4 2swap . . . . cr

	depth . cr
;
