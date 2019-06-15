\ -*- forth -*-
\	A sometimes minimal FORTH compiler for Linux / i386 systems. -*- asm -*-

: / /mod swap drop ;
: mod /mod drop ;

: '\n' 10 ;
: bl   32 ; \ BL (BLank) is a standard FORTH word for space.

: cr '\n' emit ;

: space bl emit ;

: negate 0 swap - ;

: true  1 ;
: false 0 ;
: not   0= ;

: literal immediate
	['] lit ,		\ compile LIT
	,		\ compile the literal itself (from the stack)
	;

: ':'
	[		\ go into immediate mode (temporarily)
	char :		\ push the number 58 (ASCII code of colon) on the parameter stack
	]		\ go back to compile mode
	literal		\ compile LIT 58 as the definition of ':' word
;

: ';' [ char ; ] literal ;
: '(' [ char ( ] literal ;
: ')' [ char ) ] literal ;
: '"' [ char " ] literal ;
: 'A' [ char A ] literal ;
: '0' [ char 0 ] literal ;
: '-' [ char - ] literal ;
: '.' [ char . ] literal ;

: postpone immediate
	word		\ get the next word
	find		\ find it in the dictionary
	>cfa		\ get its codeword
	,		\ and compile that
;

: [char] immediate char postpone literal ;

: recurse immediate
	latest @	\ LATEST points to the word being compiled at the moment
	>cfa		\ get the codeword
	,		\ compile it
;

: recursive immediate latest @ reveal ;


\ CONTROL STRUCTURES --------------------------------------------

: if immediate
	['] 0branch ,	\ compile 0BRANCH
	here @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
;

: then immediate
	dup
	here @ swap -	\ calculate the offset from the address saved on the stack
	swap !		\ store the offset in the back-filled location
;

: else immediate
	['] branch ,	\ definite branch to just over the false-part
	here @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
	swap		\ now back-fill the original (IF) offset
	dup		\ same as for THEN word above
	here @ swap -
	swap !
;

: begin immediate
	here @		\ save location on the stack
;

: until immediate
	['] 0branch ,	\ compile 0BRANCH
	here @ -	\ calculate the offset from the address saved on the stack
	,		\ compile the offset here
;

: again immediate
	['] branch ,	\ compile BRANCH
	here @ -	\ calculate the offset back
	,		\ compile the offset here
;

: while immediate
	['] 0branch ,	\ compile 0BRANCH
	here @		\ save location of the offset2 on the stack
	0 ,		\ compile a dummy offset2
;

: repeat immediate
	['] branch ,	\ compile BRANCH
	swap		\ get the original offset (from BEGIN)
	here @ - ,	\ and compile it after BRANCH
	dup
	here @ swap -	\ calculate the offset2
	swap !		\ and back-fill it in the original location
;

: unless immediate
	['] not ,		\ compile NOT (to reverse the test)
	postpone if	\ continue by calling the normal IF
;

: rpick 1+ 4 * rsp@ + @ ;


: ( immediate
	1		\ allowed nested parens by keeping track of depth
	begin
		key		\ read next character
		dup '(' = if	\ open paren?
			drop		\ drop the open paren
			1+		\ depth increases
		else
			')' = if	\ close paren?
				1-		\ depth decreases
			then
		then
	dup 0= until		\ continue until we reach matching close paren, depth 0
	drop		\ drop the depth counter
;

: nip ( x y -- y ) swap drop ;
: tuck ( x y -- y x y ) swap over ;
: pick ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
	1+		( add one because of 'u' on the stack )
	4 *		( multiply by the word size )
	sp@ +		( add to the stack pointer )
	@    		( and fetch )
;

: spaces	( n -- )
	begin
		dup 0>		( while n > 0 )
	while
		space		( print a space )
		1-		( until we count down to 0 )
	repeat
	drop
;

: decimal ( -- ) 10 base ! ;
: hex ( -- ) 16 base ! ;

( This is the underlying recursive definition of U. )
: u.		( u -- )
	base @ /mod	( width rem quot )
	?dup if			( if quotient <> 0 then )
		recurse		( print the quotient )
	then

	( print the remainder )
	dup 10 < if
		'0'		( decimal digits 0..9 )
	else
		10 -		( hex and beyond digits A..Z )
		'A'
	then
	+
	emit
;

( This word returns the width (in characters) of an unsigned number in the current base )
: uwidth	( u -- width )
	base @ /	( rem quot )
	?dup if		( if quotient <> 0 then )
		recurse 1+	( return 1+recursive call )
	else
		1		( return 1 )
	then
;

: u.r		( u width -- )
	swap		( width u )
	dup		( width u u )
	uwidth		( width u uwidth )
	rot		( u uwidth width )
	swap -		( u width-uwidth )
	spaces
	( ... and then call the underlying implementation of u. )
	u.
;

\	.R prints a signed number, padded to a certain width.
: .r		( n width -- )
	swap		( width n )
	dup 0< if
		negate		( width u )
		1		( save a flag to remember that it was negative | width n 1 )
		swap		( width 1 u )
		rot		( 1 u width )
		1-		( 1 u width-1 )
	else
		0		( width u 0 )
		swap		( width 0 u )
		rot		( 0 u width )
	then
	swap		( flag width u )
	dup		( flag width u u )
	uwidth		( flag width u uwidth )
	rot		( flag u uwidth width )
	swap -		( flag u width-uwidth )

	spaces		( flag u )
	swap		( u flag )

	if			( was it negative? print the - character )
		'-' emit
	then

	u.
;

: . 0 .r space ;

: u. u. space ;

: ? ( addr -- ) @ . ;

(  or define without ifs: OVER - >R - R>  U<  )
: within
	-rot		( b c a )
	over		( b c a c )
	<= if
		> if		( b c -- )
			true
		else
			false
		then
	else
		2drop		( b c -- )
		false
	then
;

: depth		( -- n )
	s0 @ sp@ -
	4-			( adjust because S0 was on the stack when we pushed DSP )
        4 /                     ( divide by the cell size to get item count )
;

: .s		( -- )
        [ char < ] literal emit depth 0 .r [ char > ] literal emit space ( print stack depth)
	s0 @		( get current stack pointer )
	begin
		4-		( move down )
		dup sp@ 4+  >
	while
		dup @ u.	( print the stack element )
		space
	repeat
	drop
;

: aligned	( addr -- addr )
	3 + 3 invert and	( (addr+3) & ~3 )
;

: align here @ aligned here ! ;

( C, appends a byte to the current compiled word. )
: c,
	here @ c!	( store the character in the compiled image )
	1 here +!	( increment HERE pointer by 1 byte )
;

: s" immediate		( -- addr len )
	state @ if	( compiling? )
		['] litstring ,	( compile LITSTRING )
		here @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		begin
			key 		( get next character of the string )
			dup '"' <>
		while
			c,		( copy character )
		repeat
		drop		( drop the double quote character at the end )
		dup		( get the saved address of the length word )
		here @ swap -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		swap !		( and back-fill the length location )
		align		( round up to next multiple of 4 bytes for the remaining code )
	else		( immediate mode )
		here @		( get the start address of the temporary space )
		begin
			key
			dup '"' <>
		while
			over c!		( save next character )
			1+		( increment address )
		repeat
		drop		( drop the final " character )
		here @ -	( calculate the length )
		here @		( push the start address )
		swap 		( addr len )
	then
;

: ." immediate		( -- )
	state @ if	( compiling? )
		postpone s"	( read the string, and compile LITSTRING, etc. )
		['] type ,	( compile the final TYPE )
	else
		( In immediate mode, just read characters and print them until we get
		  to the ending double quote. )
		begin
			key
			dup '"' = if
				drop	( drop the double quote character )
				exit	( return from this function )
			then
			emit
		again
	then
;

: constant
	word		( get the name (the name follows CONSTANT) )
	create		( make the dictionary entry )
	docol ,		( append DOCOL (the codeword field of this word) )
	['] lit ,		( append the codeword LIT )
	,		( append the value on the top of the stack )
	['] exit ,	( append the codeword EXIT )
;

0 constant nil

: allot		( n -- addr )
	here @ swap	( here n )
	here +!		( adds n to HERE, after this the old value of HERE is still on the stack )
;


: cells ( n -- n ) 4 * ;

: variable
	1 cells allot	( allocate 1 cell of memory, push the pointer to this memory )
	word create	( make the dictionary entry (the name follows VARIABLE) )
	docol ,		( append DOCOL (the codeword field of this word) )
	['] lit ,		( append the codeword LIT )
	,		( append the pointer to the new memory )
	['] exit ,	( append the codeword EXIT )
;

: value		( n -- )
	word create	( make the dictionary entry (the name follows VALUE) )
	docol ,		( append DOCOL )
	['] lit ,		( append the codeword LIT )
	,		( append the initial value )
	['] exit ,	( append the codeword EXIT )
;

: to immediate	( n -- )
	word		( get the name of the value )
	find		( look it up in the dictionary )
	>dfa		( get a pointer to the first data field (the 'LIT') )
	4+		( increment to point at the value )
	state @ if	( compiling? )
		['] lit ,		( compile LIT )
		,		( compile the address of the value )
		['] ! ,		( compile ! )
	else		( immediate mode )
		!		( update it straightaway )
	then
;

( x +TO VAL adds x to VAL )
: +to immediate
	word		( get the name of the value )
	find		( look it up in the dictionary )
	>dfa		( get a pointer to the first data field (the 'LIT') )
	4+		( increment to point at the value )
	state @ if	( compiling? )
		['] lit ,		( compile LIT )
		,		( compile the address of the value )
		['] +! ,		( compile +! )
	else		( immediate mode )
		+!		( update it straightaway )
	then
;

(
	PRINTING THE DICTIONARY ----------------------------------------------------------------------

	ID. takes an address of a dictionary entry and prints the word's name.

	For example: LATEST @ ID. would print the name of the last word that was defined.
)
: id.
	4+		( skip over the link pointer )
	dup c@		( get the flags/length byte )
	f_lenmask and	( mask out the flags - just want the length )

	begin
		dup 0>		( length > 0? )
	while
		swap 1+		( addr len -- len addr+1 )
		dup c@		( len addr -- len addr char | get the next character)
		emit		( len addr char -- len addr | and print it)
		swap 1-		( len addr -- addr len-1    | subtract one from length )
	repeat
	2drop		( len addr -- )
;

: ?hidden
	4+		( skip over the link pointer )
	c@		( get the flags/length byte )
	f_hidden and	( mask the F_HIDDEN flag and return it (as a truth value) )
;
: ?immediate
	4+		( skip over the link pointer )
	c@		( get the flags/length byte )
	f_immed and	( mask the F_IMMED flag and return it (as a truth value) )
;

: words
	latest @	( start at LATEST dictionary entry )
	begin
		?dup		( while link pointer is not null )
	while
		dup ?hidden not if	( ignore hidden words )
			dup id.		( but if not hidden, print the word )
			space
		then
		@		( dereference the link pointer - go to previous word )
	repeat
	cr
;

: forget
	word find	( find the word, gets the dictionary entry address )
	dup @ latest !	( set LATEST to point to the previous word )
	here !		( and store HERE with the dictionary address )
;

: dump		( addr len -- )
	base @ -rot		( save the current BASE at the bottom of the stack )
	hex			( and switch to hexadecimal mode )

	begin
		?dup		( while len > 0 )
	while
		over 8 u.r	( print the address )
		space

		( print up to 16 words on this line )
		2dup		( addr len addr len )
		1- 15 and 1+	( addr len addr linelen )
		begin
			?dup		( while linelen > 0 )
		while
			swap		( addr len linelen addr )
			dup c@		( addr len linelen addr byte )
			2 .r space	( print the byte )
			1+ swap 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		repeat
		drop		( addr len )

		( print the ASCII equivalents )
		2dup 1- 15 and 1+ ( addr len addr linelen )
		begin
			?dup		( while linelen > 0)
		while
			swap		( addr len linelen addr )
			dup c@		( addr len linelen addr byte )
			dup 32 128 within if	( 32 <= c < 128? )
				emit
			else
				drop '.' emit
			then
			1+ swap 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		repeat
		drop		( addr len )
		cr

		dup 1- 15 and 1+ ( addr len linelen )
		tuck		( addr linelen len linelen )
		-		( addr linelen len-linelen )
		>r + r>		( addr+linelen len-linelen )
	repeat

	drop			( restore stack )
	base !			( restore saved BASE )
;

: case immediate
	0		( push 0 to mark the bottom of the stack )
;

: of immediate
	['] over ,	( compile OVER )
	['] = ,		( compile = )
	postpone if	( compile IF )
	['] drop ,  	( compile DROP )
;

: endof immediate
	postpone else	( ENDOF is the same as ELSE )
;

: endcase immediate
	['] drop ,	( compile DROP )

	( keep compiling THEN until we get to our zero marker )
	begin
		?dup
	while
		postpone then
	repeat
;

: cfa>
	latest @	( start at LATEST dictionary entry )
	begin
		?dup		( while link pointer is not null )
	while
		2dup swap	( cfa curr curr cfa )
		< if		( current dictionary entry < cfa? )
			nip		( leave curr dictionary entry on the stack )
			exit
		then
		@		( follow link pointer back )
	repeat
	drop		( restore stack )
	0		( sorry, nothing found )
;

: see
	word find	( find the dictionary entry to decompile )

	( Now we search again, looking for the next word in the dictionary.  This gives us
	  the length of the word that we will be decompiling.  (Well, mostly it does). )
	here @		( address of the end of the last compiled word )
	latest @	( word last curr )
	begin
		2 pick		( word last curr word )
		over		( word last curr word curr )
		<>		( word last curr word<>curr? )
	while			( word last curr )
		nip		( word curr )
		dup @		( word curr prev (which becomes: word last curr) )
	repeat

	drop		( at this point, the stack is: start-of-word end-of-word )
	swap		( end-of-word start-of-word )

	( begin the definition with : NAME [IMMEDIATE] )
	':' emit space dup id. space
	dup ?immediate if ." immediate " then

	>dfa		( get the data address, ie. points after DOCOL | end-of-word start-of-data )

	( now we start decompiling until we hit the end of the word )
	begin		( end start )
		2dup >
	while
		dup @		( end start codeword )

		case
		['] lit of		( is it LIT ? )
			4 + dup @		( get next word which is the integer constant )
			.			( and print it )
		endof
		['] litstring of		( is it LITSTRING ? )
			[ char S ] literal emit '"' emit space ( print S"<space> )
			4 + dup @		( get the length word )
			swap 4 + swap		( end start+4 length )
			2dup type		( print the string )
			'"' emit space		( finish the string with a final quote )
			+ aligned		( end start+4+len, aligned )
			4 -			( because we're about to add 4 below )
		endof
		['] 0branch of		( is it 0BRANCH ? )
			." 0branch ( "
			4 + dup @		( print the offset )
			.
			." ) "
		endof
		['] branch of		( is it BRANCH ? )
			." branch ( "
			4 + dup @		( print the offset )
			.
			." ) "
		endof
		['] ' of		( is it ' (TICK) ? )
			[ char ' ] literal emit space
			4 + dup @		( get the next codeword )
			cfa>			( and force it to be printed as a dictionary entry )
			id. space
		endof
		['] ['] of		( is it ['] ? )
			." [.] "
			4 + dup @
			cfa>
			id. space
		endof
		['] exit of		( is it EXIT? )
			( We expect the last word to be EXIT, and if it is then we don't print it
			  because EXIT is normally implied by ;.  EXIT can also appear in the middle
			  of words, and then it needs to be printed. )
			2dup			( end start end start )
			4 +			( end start end start+4 )
			<> if			( end start | we're not at the end )
				." exit "
			then
		endof
					( default case: )
			dup			( in the default case we always need to dup before using )
			cfa>			( look up the codeword to get the dictionary entry )
			id. space		( and print it )
		endcase

		4 +		( end start+4 )
	repeat

	';' emit cr

	2drop		( restore stack )
;

: :noname
	0 0 create	( create a word with no name - we need a dictionary header because ; expects it )
	here @		( current HERE value is the address of the codeword, ie. the xt )
	docol ,		( compile DOCOL (the codeword) )
	]		( go into compile mode )
;

: exception-marker
	rdrop			( drop the original parameter stack pointer )
	0			( there was no exception, this is the normal return path )
;

: catch		( xt -- exn? )
	sp@ 4+ >r		( save parameter stack pointer (+4 because of xt) on the return stack )
	['] exception-marker 4+	( push the address of the RDROP inside EXCEPTION-MARKER ... )
	>r			( ... on to the return stack so it acts like a return address )
	execute			( execute the nested function )
;

: throw		( n -- )
	?dup if			( only act if the exception code <> 0 )
		rsp@ 			( get return stack pointer )
		begin
			dup r0 4- <		( RSP < R0 )
		while
			dup @			( get the return stack entry )
			['] exception-marker 4+ = if	( found the EXCEPTION-MARKER on the return stack )
				4+			( skip the EXCEPTION-MARKER on the return stack )
				rsp!			( restore the return stack pointer )

				( Restore the parameter stack. )
				dup dup dup		( reserve some working space so the stack for this word
							  doesn't coincide with the part of the stack being restored )
				r>			( get the saved parameter stack pointer | n dsp )
				4-			( reserve space on the stack to store n )
				swap over		( dsp n dsp )
				!			( write n on the stack )
				sp! exit		( restore the parameter stack pointer, immediately exit )
			then
			4+
		repeat

		( No matching catch - print a message and restart the INTERPRETer. )
		drop

		case
		0 1- of	( ABORT )
			." ABORTED" cr
		endof
			( default case )
			." UNCAUGHT THROW "
			dup . cr
		endcase
		quit
	then
;

: abort		( -- )
	0 1- throw
;

( Print a stack trace by walking up the return stack. )
: print-stack-trace
	rsp@				( start at caller of this function )
	begin
		dup r0 4- <		( RSP < R0 )
	while
		dup @			( get the return stack entry )
		case
		['] exception-marker 4+ of	( is it the exception stack frame? )
			." catch ( DSP="
			4+ dup @ u.		( print saved stack pointer )
			." ) "
		endof
						( default case )
			dup
			cfa>			( look up the codeword to get the dictionary entry )
			?dup if			( and print it )
				2dup			( dea addr dea )
				id.			( print word from dictionary entry )
				[ char + ] literal emit
				swap >dfa 4+ - .	( print offset )
			then
		endcase
		4+			( move up the stack )
	repeat
	drop
	cr
;

\ C STRINGS

\ Z" .." is like S" ..." except that the string is
\ terminated by an ASCII NUL character.

: z" immediate
	state @ if	( compiling? )
		['] litstring ,	( compile LITSTRING )
		here @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		begin
			key 		( get next character of the string )
			dup '"' <>
		while
			here @ c!	( store the character in the compiled image )
			1 here +!	( increment HERE pointer by 1 byte )
		repeat
		0 here @ c!	( add the ASCII NUL byte )
		1 here +!
		drop		( drop the double quote character at the end )
		dup		( get the saved address of the length word )
		here @ swap -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		swap !		( and back-fill the length location )
		align		( round up to next multiple of 4 bytes for the remaining code )
		['] drop ,	( compile DROP (to drop the length) )
	else		( immediate mode )
		here @		( get the start address of the temporary space )
		begin
			key
			dup '"' <>
		while
			over c!		( save next character )
			1+		( increment address )
		repeat
		drop		( drop the final " character )
		0 swap c!	( store final ASCII NUL )
		here @		( push the start address )
	then
;

: strlen 	( str -- len )
	dup		( save start address )
	begin
		dup c@ 0<>	( zero byte found? )
	while
		1+
	repeat

	swap -		( calculate the length )
;

: cstring	( addr len -- c-addr )
	swap over	( len saddr len )
	here @ swap	( len saddr daddr len )
	cmove		( len )

	here @ +	( daddr+len )
	0 swap c!	( store terminating NUL char )

	here @ 		( push start address )
;

: argc
	s0 @ @
;

\ n ARGV gets the nth command line argument.
: argv ( n -- str u )
	1+ cells s0 @ +	( get the address of argv[n] entry )
	@		( get the address of the string )
	dup strlen	( and get its length / turn it into a FORTH string )
;

\ ENVIRON returns the address of the first environment string.
\ The list of strings ends with a NULL pointer.
: environ	( -- addr )
	argc		( number of command line parameters on the stack to skip )
	2 +		( skip command line count and NULL pointer after the command line args )
	cells		( convert to an offset )
	s0 @ +		( add to base stack address )
;

( BYE exits by calling the Linux exit(2) syscall. )
: bye		( -- )
	0		( return code (0) )
	sys_exit	( system call number )
	syscall1
;

: get-brk	( -- brkpoint )
	0 sys_brk syscall1	( call brk(0) )
;

\ UNUSED returns the number of cells remaining in the user memory (data segment).
: unused	( -- n )
	get-brk		( get end of data segment according to the kernel )
	here @		( get current position in data segment )
	-
	4 /		( returns number of cells )
;

: brk		( brkpoint -- )
	sys_brk syscall1
;

\ MORECORE increases the data segment by the specified number
\ of (4 byte) cells. The number of cells requested should
\  normally be a multiple of 1024. )
: morecore	( cells -- )
	cells get-brk + brk
;

: r/o ( -- fam ) o_rdonly ;
: r/w ( -- fam ) o_rdwr ;

: open-file	( addr u fam -- fd 0 (if successful) | c-addr u fam -- fd errno (if there was an error) )
	-rot		( fam addr u )
	cstring		( fam cstring )
	sys_open syscall2 ( open (filename, flags) )
	dup		( fd fd )
	dup 0< if	( errno? )
		negate		( fd errno )
	else
		drop 0		( fd 0 )
	then
;

: create-file	( addr u fam -- fd 0 (if successful) | c-addr u fam -- fd errno (if there was an error) )
	o_creat or
	o_trunc or
	-rot		( fam addr u )
	cstring		( fam cstring )
	420 -rot	( 0644 fam cstring )
	sys_open syscall3 ( open (filename, flags|O_TRUNC|O_CREAT, 0644) )
	dup		( fd fd )
	dup 0< if	( errno? )
		negate		( fd errno )
	else
		drop 0		( fd 0 )
	then
;

: close-file	( fd -- 0 (if successful) | fd -- errno (if there was an error) )
	sys_close syscall1
	negate
;

: read-file	( addr u fd -- u2 0 (if successful) | addr u fd -- 0 0 (if EOF) | addr u fd -- u2 errno (if error) )
	>r swap r>	( u addr fd )
	sys_read syscall3

	dup		( u2 u2 )
	dup 0< if	( errno? )
		negate		( u2 errno )
	else
		drop 0		( u2 0 )
	then
;

(
	PERROR prints a message for an errno, similar to C's perror(3) but we don't have the extensive
	list of strerror strings available, so all we can do is print the errno.
)
: perror	( errno addr u -- )
	type
	':' emit space
	." ERRNO="
	. cr
;

\ ASSEMBLER CODE

hex

( Equivalent to the NEXT macro )
: next immediate AD c, FF c, 20 c, ;

: ;code immediate
	postpone next		( end the word with NEXT macro )
	align			( machine code is assembled in bytes so isn't necessarily aligned at the end )
	latest @ dup
	hidden			( unhide the word )
	dup >dfa swap >cfa !	( change the codeword to point to the data area )
	postpone [		( go back to immediate mode )
;

( The i386 registers )
: eax immediate 0 ;
: ecx immediate 1 ;
: edx immediate 2 ;
: ebx immediate 3 ;
: esp immediate 4 ;
: ebp immediate 5 ;
: esi immediate 6 ;
: edi immediate 7 ;

( i386 stack instructions )
: push immediate 50 + c, ;
: pop immediate 58 + c, ;

( RDTSC instruction )
: rdtsc immediate 0F c, 31 c, ;

decimal

\ RDTSC is an assembler primitive which reads the Pentium timestamp
\ counter (a very fine-grained counter which counts processor clock
\ cycles).  Because the TSC is 64 bits wide we have to push it onto
\ the stack in two slots. )
: rdtsc		( -- lsb msb )
	rdtsc		( writes the result in %edx:%eax )
	eax push	( push lsb )
	edx push	( push msb )
;code

hex
: =next		( addr -- next? )
	   dup c@ AD <> if drop false exit then
	1+ dup c@ FF <> if drop false exit then
	1+     c@ 20 <> if      false exit then
	true
;
decimal

\ (INLINE) is the lowlevel inline function.
: (inline)	( cfa -- )
	@			( remember codeword points to the code )
	begin			( copy bytes until we hit NEXT macro )
		dup =next not
	while
		dup c@ c,
		1+
	repeat
	drop
;

: inline immediate
	word find		( find the word in the dictionary )
	>cfa			( codeword )

	dup @ docol = if	( check codeword <> DOCOL (ie. not a FORTH word) )
		." Cannot INLINE FORTH words" cr abort
	then

	(inline)
;

hide =next

\ NOTE: DOES> isn't possible to implement with this FORTH because
\ we don't have a separate data pointer.

: welcome
	s" TEST-MODE" find not if
		." JANEFORTH VERSION " version . cr
		unused . ." CELLS REMAINING" cr
		." OK "
	then
;

welcome
hide welcome
