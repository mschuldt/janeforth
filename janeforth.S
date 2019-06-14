//  -*- asm -*-
//  A sometimes minimal FORTH compiler for Linux / i386 systems.

	.set JANEFORTH_VERSION,1

/* NEXT macro. */
	.macro NEXT
	lodsl
	jmp *(%eax)
	.endm

/* Macros to deal with the return stack. */
	.macro PUSHRSP reg
	lea -4(%ebp),%ebp	// push reg on to return stack
	movl \reg,(%ebp)
	.endm

	.macro POPRSP reg
	mov (%ebp),\reg		// pop top of return stack to reg
	lea 4(%ebp),%ebp
	.endm

/* DOCOL - the interpreter! */
	.text
	.align 4
DOCOL:
	PUSHRSP %esi		// push %esi on to the return stack
	addl $4,%eax		// %eax points to codeword, so make
	movl %eax,%esi		// %esi point to first data word
	NEXT

/* Assembler entry point. */
	.text
	.globl _start
_start:
	cld
	mov %esp,var_S0		// Save the initial data stack pointer in FORTH variable S0.
	mov $return_stack_top,%ebp // Initialise the return stack.
	call set_up_data_segment

	mov $cold_start,%esi	// Initialise interpreter.
	NEXT			// Run interpreter!

	.section .rodata
cold_start:			// High-level code without a codeword.
	.int QUIT

/* Flags  */
	.set F_IMMED,0x80
	.set F_HIDDEN,0x20
	.set F_LENMASK,0x1f	// length mask

	// Store the chain of links.
	.set link,0

	.macro defword name, namelen, flags=0, label
	.section .rodata
	.align 4
	.globl name_\label
name_\label :
	.int link		// link
	.set link,name_\label
	.byte \flags+\namelen	// flags + length byte
	.ascii "\name"		// the name
	.align 4		// padding to next 4 byte boundary
	.globl \label
\label :
	.int DOCOL		// codeword - the interpreter
	// list of word pointers follow
	.endm

	.macro defcode name, namelen, flags=0, label
	.section .rodata
	.align 4
	.globl name_\label
name_\label :
	.int link		// link
	.set link,name_\label
	.byte \flags+\namelen	// flags + length byte
	.ascii "\name"		// the name
	.align 4		// padding to next 4 byte boundary
	.globl \label
\label :
	.int code_\label	// codeword
	.text
	//.align 4
	.globl code_\label
code_\label :			// assembler code follows
	.endm


	defcode "drop",4,,DROP
	pop %eax		// drop top of stack
	NEXT

	defcode "swap",4,,SWAP
	pop %eax		// swap top two elements on stack
	pop %ebx
	push %eax
	push %ebx
	NEXT

	defcode "dup",3,,DUP
	mov (%esp),%eax		// duplicate top of stack
	push %eax
	NEXT

	defcode "over",4,,OVER
	mov 4(%esp),%eax	// get the second element of stack
	push %eax		// and push it on top
	NEXT

	defcode "rot",3,,ROT
	pop %eax
	pop %ebx
	pop %ecx
	push %ebx
	push %eax
	push %ecx
	NEXT

	defcode "-rot",4,,NROT
	pop %eax
	pop %ebx
	pop %ecx
	push %eax
	push %ecx
	push %ebx
	NEXT

	defcode "2drop",5,,TWODROP // drop top two elements of stack
	pop %eax
	pop %eax
	NEXT

	defcode "2dup",4,,TWODUP // duplicate top two elements of stack
	mov (%esp),%eax
	mov 4(%esp),%ebx
	push %ebx
	push %eax
	NEXT

	defcode "2swap",5,,TWOSWAP // swap top two pairs of elements of stack
	pop %eax
	pop %ebx
	pop %ecx
	pop %edx
	push %ebx
	push %eax
	push %edx
	push %ecx
	NEXT

	defcode "?dup",4,,QDUP	// duplicate top of stack if non-zero
	movl (%esp),%eax
	test %eax,%eax
	jz 1f
	push %eax
1:	NEXT

	defcode "1+",2,,INCR
	incl (%esp)		// increment top of stack
	NEXT

	defcode "1-",2,,DECR
	decl (%esp)		// decrement top of stack
	NEXT

	defcode "4+",2,,INCR4
	addl $4,(%esp)		// add 4 to top of stack
	NEXT

	defcode "4-",2,,DECR4
	subl $4,(%esp)		// subtract 4 from top of stack
	NEXT

	defcode "+",1,,ADD
	pop %eax		// get top of stack
	addl %eax,(%esp)	// and add it to next word on stack
	NEXT

	defcode "-",1,,SUB
	pop %eax		// get top of stack
	subl %eax,(%esp)	// and subtract it from next word on stack
	NEXT

	defcode "*",1,,MUL
	pop %eax
	pop %ebx
	imull %ebx,%eax
	push %eax		// ignore overflow
	NEXT

	defcode "/mod",4,,DIVMOD
	xor %edx,%edx
	pop %ebx
	pop %eax
	idivl %ebx
	push %edx		// push remainder
	push %eax		// push quotient
	NEXT

	defcode "=",1,,EQU	// top two words are equal?
	pop %eax
	pop %ebx
	cmp %ebx,%eax
	sete %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "<>",2,,NEQU	// top two words are not equal?
	pop %eax
	pop %ebx
	cmp %ebx,%eax
	setne %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "<",1,,LT
	pop %eax
	pop %ebx
	cmp %eax,%ebx
	setl %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode ">",1,,GT
	pop %eax
	pop %ebx
	cmp %eax,%ebx
	setg %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "<=",2,,LE
	pop %eax
	pop %ebx
	cmp %eax,%ebx
	setle %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode ">=",2,,GE
	pop %eax
	pop %ebx
	cmp %eax,%ebx
	setge %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "0=",2,,ZEQU	// top of stack equals 0?
	pop %eax
	test %eax,%eax
	setz %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "0<>",3,,ZNEQU	// top of stack not 0?
	pop %eax
	test %eax,%eax
	setnz %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "0<",2,,ZLT	// comparisons with 0
	pop %eax
	test %eax,%eax
	setl %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "0>",2,,ZGT
	pop %eax
	test %eax,%eax
	setg %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "0<=",3,,ZLE
	pop %eax
	test %eax,%eax
	setle %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "0>=",3,,ZGE
	pop %eax
	test %eax,%eax
	setge %al
	movzbl %al,%eax
	pushl %eax
	NEXT

	defcode "and",3,,AND	// bitwise AND
	pop %eax
	andl %eax,(%esp)
	NEXT

	defcode "or",2,,OR	// bitwise OR
	pop %eax
	orl %eax,(%esp)
	NEXT

	defcode "xor",3,,XOR	// bitwise XOR
	pop %eax
	xorl %eax,(%esp)
	NEXT

	defcode "invert",6,,INVERT // this is the FORTH bitwise "NOT" function (cf. NEGATE and NOT)
	notl (%esp)
	NEXT

	defcode "2/",2,,TWODIV
        shrl $1, (%esp)
	NEXT

	defcode "2*",2,,TWOMUL
        shll $1, (%esp)
	NEXT

	defcode "lshift",6,,LSHIFT	// left shift
        movb (%esp), %cl
        pop %eax
        shll %cl, (%esp)
	NEXT

	defcode "rshift",6,,RSHIFT	// left right
        movb (%esp), %cl
        pop %eax
        shrl %cl, (%esp)
	NEXT


	defcode "exit",4,,EXIT
	POPRSP %esi		// pop return stack into %esi
	NEXT

	defcode "lit",3,,LIT
	lodsl
	push %eax		// push the literal number on to stack
	NEXT

	defcode "!",1,,STORE
	pop %ebx		// address to store at
	pop %eax		// data to store there
	mov %eax,(%ebx)		// store it
	NEXT

	defcode "@",1,,FETCH
	pop %ebx		// address to fetch
	mov (%ebx),%eax		// fetch it
	push %eax		// push value onto stack
	NEXT

	defcode "+!",2,,ADDSTORE
	pop %ebx		// address
	pop %eax		// the amount to add
	addl %eax,(%ebx)	// add it
	NEXT

	defcode "-!",2,,SUBSTORE
	pop %ebx		// address
	pop %eax		// the amount to subtract
	subl %eax,(%ebx)	// add it
	NEXT

	defcode "c!",2,,STOREBYTE
	pop %ebx		// address to store at
	pop %eax		// data to store there
	movb %al,(%ebx)		// store it
	NEXT

	defcode "c@",2,,FETCHBYTE
	pop %ebx		// address to fetch
	xor %eax,%eax
	movb (%ebx),%al		// fetch it
	push %eax		// push value onto stack
	NEXT

/* C@C! is a useful byte copy primitive. */
	defcode "c@c!",4,,CCOPY
	movl 4(%esp),%ebx	// source address
	movb (%ebx),%al		// get source character
	pop %edi		// destination address
	stosb			// copy to destination
	push %edi		// increment destination address
	incl 4(%esp)		// increment source address
	NEXT

	defcode "cmove",5,,CMOVE
	mov %esi,%edx		// preserve %esi
	pop %ecx		// length
	pop %edi		// destination address
	pop %esi		// source address
	rep movsb		// copy source to destination
	mov %edx,%esi		// restore %esi
	NEXT

	.macro defvar name, namelen, flags=0, label, initial=0
	defcode \name,\namelen,\flags,\label
	push $var_\label
	NEXT
	.data
	.align 4
var_\label :
	.int \initial
	.endm

	defvar "state",5,,STATE
	defvar "here",4,,HERE
	defvar "latest",6,,LATEST,name_SYSCALL0 // SYSCALL0 must be last in built-in dictionary
	defvar "s0",2,,S0
	defvar "base",4,,BASE,10

//#include <asm-i386/unistd.h>	// you might need this instead
#include <asm/unistd.h>

	.macro defconst name, namelen, flags=0, label, value
	defcode \name,\namelen,\flags,\label
	push $\value
	NEXT
	.endm

	defconst "version",7,,VERSION,JANEFORTH_VERSION
	defconst "r0",2,,RZ,return_stack_top
	defconst "docol",5,,__DOCOL,DOCOL
	defconst "f_immed",7,,__F_IMMED,F_IMMED
	defconst "f_hidden",8,,__F_HIDDEN,F_HIDDEN
	defconst "f_lenmask",9,,__F_LENMASK,F_LENMASK

	defconst "sys_exit",8,,SYS_EXIT,__NR_exit
	defconst "sys_open",8,,SYS_OPEN,__NR_open
	defconst "sys_close",9,,SYS_CLOSE,__NR_close
	defconst "sys_read",8,,SYS_READ,__NR_read
	defconst "sys_write",9,,SYS_WRITE,__NR_write
	defconst "sys_creat",9,,SYS_CREAT,__NR_creat
	defconst "sys_brk",7,,SYS_BRK,__NR_brk

	defconst "o_rdonly",8,,__O_RDONLY,0
	defconst "o_wronly",8,,__O_WRONLY,1
	defconst "o_rdwr",6,,__O_RDWR,2
	defconst "o_creat",7,,__O_CREAT,0100
	defconst "o_excl",6,,__O_EXCL,0200
	defconst "o_trunc",7,,__O_TRUNC,01000
	defconst "o_append",8,,__O_APPEND,02000
	defconst "o_nonblock",10,,__O_NONBLOCK,04000

	defcode ">r",2,,TOR
	pop %eax		// pop parameter stack into %eax
	PUSHRSP %eax		// push it on to the return stack
	NEXT

	defcode "r>",2,,FROMR
	POPRSP %eax		// pop return stack on to %eax
	push %eax		// and push on to parameter stack
	NEXT

	defcode "r@",2,,RSPFETCH
	push %ebp
	NEXT

	defcode "rsp!",4,,RSPSTORE
	pop %ebp
	NEXT

	defcode "rdrop",5,,RDROP
	addl $4,%ebp		// pop return stack and throw away
	NEXT

	defcode "sp@",3,,DSPFETCH
	mov %esp,%eax
	push %eax
	NEXT

	defcode "sp!",3,,DSPSTORE
	pop %esp
	NEXT

	defcode "key",3,,KEY
	call _KEY
	push %eax		// push return value on stack
	NEXT
_KEY:
	mov (currkey),%ebx
	cmp (bufftop),%ebx
	jge 1f			// exhausted the input buffer?
	xor %eax,%eax
	mov (%ebx),%al		// get next key from input buffer
	inc %ebx
	mov %ebx,(currkey)	// increment currkey
	ret

1:	// Out of input; use read(2) to fetch more input from stdin.
	xor %ebx,%ebx		// 1st param: stdin
	mov $buffer,%ecx	// 2nd param: buffer
	mov %ecx,currkey
	mov $BUFFER_SIZE,%edx	// 3rd param: max length
	mov $__NR_read,%eax	// syscall: read
	int $0x80
	test %eax,%eax		// If %eax <= 0, then exit.
	jbe 2f
	addl %eax,%ecx		// buffer+%eax = bufftop
	mov %ecx,bufftop
	jmp _KEY

2:	// Error or end of input: exit the program.
	xor %ebx,%ebx
	mov $__NR_exit,%eax	// syscall: exit
	int $0x80

	.data
	.align 4
currkey:
	.int buffer		// Current place in input buffer (next character to read).
bufftop:
	.int buffer		// Last valid data in input buffer + 1.

	defcode "emit",4,,EMIT
	pop %eax
	call _EMIT
	NEXT
_EMIT:
	mov $1,%ebx		// 1st param: stdout

	// write needs the address of the byte to write
	mov %al,emit_scratch
	mov $emit_scratch,%ecx	// 2nd param: address

	mov $1,%edx		// 3rd param: nbytes = 1

	mov $__NR_write,%eax	// write syscall
	int $0x80
	ret

	.data			// NB: easier to fit in the .data section
emit_scratch:
	.space 1		// scratch used by EMIT

	defcode "word",4,,WORD
	call _WORD
	push %edi		// push base address
	push %ecx		// push length
	NEXT

_WORD:
	/* Search for first non-blank character.  Also skip \ comments. */
1:
	call _KEY		// get next key, returned in %eax
	cmpb $'\\',%al		// start of a comment?
	je 3f			// if so, skip the comment
	cmpb $' ',%al
	jbe 1b			// if so, keep looking

	/* Search for the end of the word, storing chars as we go. */
	mov $word_buffer,%edi	// pointer to return buffer
2:
	stosb			// add character to return buffer
	call _KEY		// get next key, returned in %al
	cmpb $' ',%al		// is blank?
	ja 2b			// if not, keep looping

	/* Return the word (well, the static buffer) and length. */
	sub $word_buffer,%edi
	mov %edi,%ecx		// return length of the word
	mov $word_buffer,%edi	// return address of the word
	ret

	/* Code to skip \ comments to end of the current line. */
3:
	call _KEY
	cmpb $'\n',%al		// end of line yet?
	jne 3b
	jmp 1b

	.data			// NB: easier to fit in the .data section
	// A static buffer where WORD returns.  Subsequent calls
	// overwrite this buffer.  Maximum word length is 32 chars.
word_buffer:
	.space 32

	defcode "number",6,,NUMBER
	pop %ecx		// length of string
	pop %edi		// start address of string
	call _NUMBER
	push %eax		// parsed number
	push %ecx		// number of unparsed characters (0 = no error)
	NEXT

_NUMBER:
	xor %eax,%eax
	xor %ebx,%ebx

	test %ecx,%ecx		// trying to parse a zero-length string is an error, but will return 0.
	jz 5f

	movl var_BASE,%edx	// get BASE (in %dl)

	// Check if first character is '-'.
	movb (%edi),%bl		// %bl = first character in string
	inc %edi
	push %eax		// push 0 on stack
	cmpb $'-',%bl		// negative number?
	jnz 2f
	pop %eax
	push %ebx		// push <> 0 on stack, indicating negative
	dec %ecx
	jnz 1f
	pop %ebx		// error: string is only '-'.
	movl $1,%ecx
	ret

	// Loop reading digits.
1:	imull %edx,%eax		// %eax *= BASE
	movb (%edi),%bl		// %bl = next character in string
	inc %edi

	// Convert 0-9, A-Z to a number 0-35.
2:	subb $'0',%bl		// < '0'?
	jb 4f
	cmp $10,%bl		// <= '9'?
	jb 3f
	subb $17,%bl		// < 'A'? (17 is 'A'-'0')
	jb 4f
	addb $10,%bl

3:	cmp %dl,%bl		// >= BASE?
	jge 4f

	// OK, so add it to %eax and loop.
	add %ebx,%eax
	dec %ecx
	jnz 1b

	// Negate the result if first character was '-' (saved on the stack).
4:	pop %ebx
	test %ebx,%ebx
	jz 5f
	neg %eax

5:	ret

	defcode "find",4,,FIND
	pop %ecx		// %ecx = length
	pop %edi		// %edi = address
	call _FIND
	push %eax		// %eax = address of dictionary entry (or NULL)
	NEXT

_FIND:
	push %esi		// Save %esi so we can use it in string comparison.

	// Now we start searching backwards through the dictionary for this word.
	mov var_LATEST,%edx	// LATEST points to name header of the latest word in the dictionary
1:	test %edx,%edx		// NULL pointer?  (end of the linked list)
	je 4f

	// Compare the length expected and the length of the word.
	// Note that if the F_HIDDEN flag is set on the word, then by a bit of trickery
	// this won't pick the word (the length will appear to be wrong).
	xor %eax,%eax
	movb 4(%edx),%al	// %al = flags+length field
	andb $(F_HIDDEN|F_LENMASK),%al // %al = name length
	cmpb %cl,%al		// Length is the same?
	jne 2f

	// Compare the strings in detail.
	push %ecx		// Save the length
	push %edi		// Save the address (repe cmpsb will move this pointer)
	lea 5(%edx),%esi	// Dictionary string we are checking against.
	repe cmpsb		// Compare the strings.
	pop %edi
	pop %ecx
	jne 2f			// Not the same.

	// The strings are the same - return the header pointer in %eax
	pop %esi
	mov %edx,%eax
	ret

2:	mov (%edx),%edx		// Move back through the link field to the previous word
	jmp 1b			// .. and loop.

4:	// Not found.
	pop %esi
	xor %eax,%eax		// Return zero to indicate not found.
	ret

	defcode ">cfa",4,,TCFA
	pop %edi
	call _TCFA
	push %edi
	NEXT
_TCFA:
	xor %eax,%eax
	add $4,%edi		// Skip link pointer.
	movb (%edi),%al		// Load flags+len into %al.
	inc %edi		// Skip flags+len byte.
	andb $F_LENMASK,%al	// Just the length, not the flags.
	add %eax,%edi		// Skip the name.
	addl $3,%edi		// The codeword is 4-byte aligned.
	andl $~3,%edi
	ret

	defword ">dfa",4,,TDFA
	.int TCFA		// >CFA		(get code field address)
	.int INCR4		// 4+		(add 4 to it to get to next word)
	.int EXIT		// EXIT		(return from FORTH word)

	defcode "create",6,,CREATE

	// Get the name length and address.
	pop %ecx		// %ecx = length
	pop %ebx		// %ebx = address of name

	// Link pointer.
	movl var_HERE,%edi	// %edi is the address of the header
	movl var_LATEST,%eax	// Get link pointer
	stosl			// and store it in the header.

	// Length byte and the word itself.
	mov %cl,%al		// Get the length.
	stosb			// Store the length/flags byte.
	push %esi
	mov %ebx,%esi		// %esi = word
	rep movsb		// Copy the word
	pop %esi
	addl $3,%edi		// Align to next 4 byte boundary.
	andl $~3,%edi

	// Update LATEST and HERE.
	movl var_HERE,%eax
	movl %eax,var_LATEST
	movl %edi,var_HERE
	NEXT

	defcode ",",1,,COMMA
	pop %eax		// Code pointer to store.
	call _COMMA
	NEXT
_COMMA:
	movl var_HERE,%edi	// HERE
	stosl			// Store it.
	movl %edi,var_HERE	// Update HERE (incremented)
	ret

	defcode "[",1,F_IMMED,LBRAC
	xor %eax,%eax
	movl %eax,var_STATE	// Set STATE to 0.
	NEXT

	defcode "]",1,,RBRAC
	movl $1,var_STATE	// Set STATE to 1.
	NEXT

	defword ":",1,,COLON
	.int WORD		// Get the name of the new word
	.int CREATE		// CREATE the dictionary entry / header
	.int LIT, DOCOL, COMMA	// Append DOCOL  (the codeword).
	.int LATEST, FETCH, HIDDEN // Make the word hidden (see below for definition).
	.int RBRAC		// Go into compile mode.
	.int EXIT		// Return from the function.

	defword ";",1,F_IMMED,SEMICOLON
	.int LIT, EXIT, COMMA	// Append EXIT (so the word will return).
	.int LATEST, FETCH, REVEAL // Toggle hidden flag -- unhide the word (see below for definition).
	.int LBRAC		// Go back to IMMEDIATE mode.
	.int EXIT		// Return from the function.

	defcode "immediate",9,F_IMMED,IMMEDIATE
	movl var_LATEST,%edi	// LATEST word.
	addl $4,%edi		// Point to name/flags byte.
	xorb $F_IMMED,(%edi)	// Toggle the IMMED bit.
	NEXT

	defcode "hidden",6,,HIDDEN
	pop %edi		// Dictionary entry.
	addl $4,%edi		// Point to name/flags byte.
	xorb $F_HIDDEN,(%edi)	// Toggle the HIDDEN bit.
	NEXT

	defcode "reveal",6,,REVEAL
	pop %edi		// Dictionary entry.
	addl $4,%edi		// Point to name/flags byte.
        mov $F_HIDDEN, %eax
	notl %eax
        andl %eax,(%edi)	// unset the HIDDEN bit.
	NEXT

	defword "hide",4,,HIDE
	.int WORD		// Get the word (after HIDE).
	.int FIND		// Look up in the dictionary.
	.int HIDDEN		// Set F_HIDDEN flag.
	.int EXIT		// Return.

	defcode "[']",3,,BTICKB
	lodsl			// Get the address of the next word and skip it.
	pushl %eax		// Push it on the stack.
	NEXT

	defword "'",1,,TICK
        .int WORD
        .int FIND
        .int TCFA
	.int EXIT

	defcode "branch",6,,BRANCH
	add (%esi),%esi		// add the offset to the instruction pointer
	NEXT

	defcode "0branch",7,,ZBRANCH
	pop %eax
	test %eax,%eax		// top of stack is zero?
	jz code_BRANCH		// if so, jump back to the branch function above
	lodsl			// otherwise we need to skip the offset
	NEXT

	defcode "litstring",9,,LITSTRING
	lodsl			// get the length of the string
	push %esi		// push the address of the start of the string
	push %eax		// push it on the stack
	addl %eax,%esi		// skip past the string
 	addl $3,%esi		// but round up to next 4 byte boundary
	andl $~3,%esi
	NEXT

	defcode "type",4,,TYPE
	mov $1,%ebx		// 1st param: stdout
	pop %edx		// 3rd param: length of string
	pop %ecx		// 2nd param: address of string
	mov $__NR_write,%eax	// write syscall
	int $0x80
	NEXT

	// QUIT must not return (ie. must not call EXIT).
	defword "quit",4,,QUIT
	.int RZ,RSPSTORE	// R0 RSP!, clear the return stack
	.int INTERPRET		// interpret the next word
	.int BRANCH,-8		// and loop (indefinitely)

	defcode "interpret",9,,INTERPRET
	call _WORD		// Returns %ecx = length, %edi = pointer to word.

	// Is it in the dictionary?
	xor %eax,%eax
	movl %eax,interpret_is_lit // Not a literal number (not yet anyway ...)
	call _FIND		// Returns %eax = pointer to header or 0 if not found.
	test %eax,%eax		// Found?
	jz 1f

	// In the dictionary.  Is it an IMMEDIATE codeword?
	mov %eax,%edi		// %edi = dictionary entry
	movb 4(%edi),%al	// Get name+flags.
	push %ax		// Just save it for now.
	call _TCFA		// Convert dictionary entry (in %edi) to codeword pointer.
	pop %ax
	andb $F_IMMED,%al	// Is IMMED flag set?
	mov %edi,%eax
	jnz 4f			// If IMMED, jump straight to executing.

	jmp 2f

1:	// Not in the dictionary (not a word) so assume it's a literal number.
	incl interpret_is_lit
	call _NUMBER		// Returns the parsed number in %eax, %ecx > 0 if error
	test %ecx,%ecx
	jnz 6f
	mov %eax,%ebx
	mov $LIT,%eax		// The word is LIT

2:	// Are we compiling or executing?
	movl var_STATE,%edx
	test %edx,%edx
	jz 4f			// Jump if executing.

	// Compiling - just append the word to the current dictionary definition.
	call _COMMA
	mov interpret_is_lit,%ecx // Was it a literal?
	test %ecx,%ecx
	jz 3f
	mov %ebx,%eax		// Yes, so LIT is followed by a number.
	call _COMMA
3:	NEXT

4:	// Executing - run it!
	mov interpret_is_lit,%ecx // Literal?
	test %ecx,%ecx		// Literal?
	jnz 5f

	// Not a literal, execute it now.  This never returns, but the codeword will
	// eventually call NEXT which will reenter the loop in QUIT.
	jmp *(%eax)

5:	// Executing a literal, which means push it on the stack.
	push %ebx
	NEXT

6:	// Parse error (not a known word or a number in the current BASE).
	// Print an error message followed by up to 40 characters of context.
	mov $2,%ebx		// 1st param: stderr
	mov $errmsg,%ecx	// 2nd param: error message
	mov $errmsgend-errmsg,%edx // 3rd param: length of string
	mov $__NR_write,%eax	// write syscall
	int $0x80

	mov (currkey),%ecx	// the error occurred just before currkey position
	mov %ecx,%edx
	sub $buffer,%edx	// %edx = currkey - buffer (length in buffer before currkey)
	cmp $40,%edx		// if > 40, then print only 40 characters
	jle 7f
	mov $40,%edx
7:	sub %edx,%ecx		// %ecx = start of area to print, %edx = length
	mov $__NR_write,%eax	// write syscall
	int $0x80

	mov $errmsgnl,%ecx	// newline
	mov $1,%edx
	mov $__NR_write,%eax	// write syscall
	int $0x80

	NEXT

	.section .rodata
errmsg: .ascii "PARSE ERROR: "
errmsgend:
errmsgnl: .ascii "\n"

	.data			// NB: easier to fit in the .data section
	.align 4
interpret_is_lit:
	.int 0			// Flag used to record if reading a literal

	defcode "char",4,,CHAR
	call _WORD		// Returns %ecx = length, %edi = pointer to word.
	xor %eax,%eax
	movb (%edi),%al		// Get the first character of the word.
	push %eax		// Push it onto the stack.
	NEXT

	defcode "execute",7,,EXECUTE
	pop %eax		// Get xt into %eax
	jmp *(%eax)		// and jump to it.
				// After xt runs its NEXT will continue executing the current word.

	defcode "syscall3",8,,SYSCALL3
	pop %eax		// System call number (see <asm/unistd.h>)
	pop %ebx		// First parameter.
	pop %ecx		// Second parameter
	pop %edx		// Third parameter
	int $0x80
	push %eax		// Result (negative for -errno)
	NEXT

	defcode "syscall2",8,,SYSCALL2
	pop %eax		// System call number (see <asm/unistd.h>)
	pop %ebx		// First parameter.
	pop %ecx		// Second parameter
	int $0x80
	push %eax		// Result (negative for -errno)
	NEXT

	defcode "syscall1",8,,SYSCALL1
	pop %eax		// System call number (see <asm/unistd.h>)
	pop %ebx		// First parameter.
	int $0x80
	push %eax		// Result (negative for -errno)
	NEXT

	defcode "syscall0",8,,SYSCALL0
	pop %eax		// System call number (see <asm/unistd.h>)
	int $0x80
	push %eax		// Result (negative for -errno)
	NEXT

/*
	DATA SEGMENT --------------------------------------------------

*/
	.text
	.set INITIAL_DATA_SEGMENT_SIZE,65536
set_up_data_segment:
	xor %ebx,%ebx		// Call brk(0)
	movl $__NR_brk,%eax
	int $0x80
	movl %eax,var_HERE	// Initialise HERE to point at beginning of data segment.
	addl $INITIAL_DATA_SEGMENT_SIZE,%eax	// Reserve nn bytes of memory for initial data segment.
	movl %eax,%ebx		// Call brk(HERE+INITIAL_DATA_SEGMENT_SIZE)
	movl $__NR_brk,%eax
	int $0x80
	ret

/*
	We allocate static buffers for the return static and input buffer (used when
	reading in files and text that the user types in).
*/
	.set RETURN_STACK_SIZE,8192
	.set BUFFER_SIZE,4096

	.bss
/* FORTH return stack. */
	.align 4096
return_stack:
	.space RETURN_STACK_SIZE
return_stack_top:		// Initial top of return stack.

/* This is used as a temporary input buffer when reading from files or the terminal. */
	.align 4096
buffer:
	.space BUFFER_SIZE

/* END OF janeforth.S */
