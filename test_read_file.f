(
	Test READ-FILE.
	$Id: test_read_file.f,v 1.2 2007-10-22 18:53:13 rich Exp $
)

0 value FD
100 cells allot constant BUFFER

: TEST
	s" test_read_file.f.out" r/o open-file
	?dup if s" test_read_file.f.out" perror quit then

	to FD

	begin
		BUFFER 100 cells FD read-file
		?dup if s" READ-FILE" perror quit then
		dup
		BUFFER swap tell
	0= until

	FD close-file
	?dup if s" CLOSE-FILE" perror quit then
;
