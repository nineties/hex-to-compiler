h@l@h@!h@C+h!k1k0-h@$k:k0-h@k1k0-+$h@C+h!ih@!h@C+h!kefh@!h@C+h!l!
h@l@h@!h@C+h!k1k0-h@$k h@k1k0-+$h@C+h!ih@!h@C+h!kefh@!h@C+h!l!

h@l@ h@!h@C+h! k1k0-h@$ k\h@k1k0-+$ h@C+h!
    i       h@!h@C+h!
    kkf     h@!h@C+h!
    kLf     h@!h@C+h!
    k:k0-   h@!h@C+h!
    k=f     h@!h@C+h!
    kJf     h@!h@C+h!
    k0k5-C* h@!h@C+h!
    kef     h@!h@C+h!
l!

\ **Now we can use single-line comments!**

\ plancklisp -
\ Copyright (C) 2025 nineties

\ This file implements a Lisp interpreter (PlanckLISP) using a Forth interpreter
\ (PlanckForth) that is written entirely by hand in machine code.

\ The process begins by bootstrapping a very minimal version of PlanckForth through
\ self-extension, gradually growing it into a usable Forth system.

\ In the 1st phase only single character words are registered
\ in the dictionary.
\ List of builtin words:
\ 'Q' ( n -- )          Exit the process
\ 'C' ( -- n )          The size of Cells
\ 'h' ( -- a-addr )     The address of 'here' cell
\ 'l' ( -- a-addr )     The address of 'latest' cell
\ 'k' ( -- c )          Read character
\ 't' ( c -- )          Print character
\ 'j' ( -- )            Unconditional branch
\ 'J' ( n -- )          Jump if a == 0
\ 'f' ( c -- xt )       Get execution token of c
\ 'x' ( xt -- ... )     Run the execution token
\ '@' ( a-addr -- w )   Load value from addr
\ '!' ( w a-addr -- )   Store value to addr
\ '?' ( c-addr -- c )   Load byte from addr
\ '$' ( c c-addr -- )   Store byte to addr
\ 'd' ( -- a-addr )     Get data stack pointer
\ 'D' ( a-addr -- )     Set data stack pointer
\ 'r' ( -- a-addr )     Get return stack pointer
\ 'R' ( a-addr -- )     Set return stack pointer
\ 'i' ( -- a-addr )     Get the interpreter function
\ 'e' ( -- )            Exit current function
\ 'L' ( -- u )          Load immediate
\ 'S' ( -- c-addr )     Load string literal
\ '+' ( a b -- c )      c = (a + b)
\ '-' ( a b -- c )      c = (a - b)
\ '*' ( a b -- c )      c = (a * b)
\ '/' ( a b -- c )      c = (a / b)
\ '%' ( a b -- c )      c = (a % b)
\ '&' ( a b -- c )      c = (a & b)
\ '|' ( a b -- c )      c = (a | b)
\ '^' ( a b -- c )      c = (a ^ b)
\ '<' ( a b -- c )      c = (a < b)
\ 'u' ( a b -- c )      c = (a unsigned< b)
\ '=' ( a b -- c )      c = (a == b)
\ '(' ( a b -- c )      c = a << b (logical)
\ ')' ( a b -- c )      c = a >> b (logical)
\ '%' ( a b -- c )      c = a >> b (arithmetic)
\ 'v' ( -- a-addr u )   argv and argc
\ 'V' ( -- c-addr )     Runtime information string

\ The 1st phase interpreter repeats execution of k, f and x.
\ The following line is an example program of planckforth
\ which prints "Hello World!\n"
\ --
\ kHtketkltkltkotk tkWtkotkrtkltkdtk!tk:k0-tk0k0-Q
\ --
\ This code repeats that 'k' reads a character and 't' prints it.
\ Note that ':' (58) minus '0' (48) is '\n' (10).

\ The structure of the dictionary.
\ +------+----------+---------+------------+---------------+
\ | link | len+flag | name... | padding... | code field ...|
\ +------+----------+---------+------------+---------------+
\ - link pointer to the previous entry (CELL byte)
\ - length of the name (6 bits)
\ - smudge bit (1 bit)
\ - immediate bit (1 bit)
\ - characters of the name (N bytes)
\ - padding to align CELL boundary if necessary.
\ - codewords and datawords (CELL-bye aligned)

\ The code group at the beginning of this file
\ defines ' ' and '\n' as no-op operation and
\ '\' to read following characters until '\n'.
\ Since I couldn't write a comment at the beginning,
\ I repost the definition of '\' for explanation.
\ --
\ h@                            ( save addr of new entry )
\ l@ h@!h@C+h!                  ( set link pointer. *here++ = latest )
\ k1k0-h@$ k\h@k1k0-+$ h@C+h!   ( write the name '\' and its length )
\ i       h@!h@C+h!             ( docol )
\ kkf     h@!h@C+h!             ( key )
\ kLf     h@!h@C+h!             ( lit )
\ k:k0-   h@!h@C+h!             ( '\n' )
\ k=f     h@!h@C+h!             ( = )
\ kJf     h@!h@C+h!             ( branch )
\ k0k5-C* h@!h@C+h!             ( -5*CELL )
\ kef     h@!h@C+h!             ( exit )
\ l!                            ( set latest to this new entry. )
\ --

\ That's all for the brief explanation. Let's restart bootstrap!

\ The COMMA operator
\ ',' ( a -- )  Store a to 'here' and increment 'here' CELL bytes.
h@l@ h@!h@C+h! k1k0-h@$ k,h@k1k0-+$ h@C+h!
    i   h@!h@C+h!   \ docol
    \ store 'a' to here
    khf h@!h@C+h!
    k@f h@!h@C+h!
    k!f h@!h@C+h!
    \ here <- here + CELL
    khf h@!h@C+h!
    k@f h@!h@C+h!
    kCf h@!h@C+h!
    k+f h@!h@C+h!
    khf h@!h@C+h!
    k!f h@!h@C+h!
    \ exit
    kef h@!h@C+h!
l!

\ TICK-like operator
\ '\'' ( "c" -- xt )    Get execution token of following character
\ NB: This definition is different from the usual definition of tick
\ because it does not skip leading spaces and can read only a single
\ character. It will be redefined in later phase
h@l@, k1k0-h@$ k'h@k1k0-+$ h@C+h!
    i, kkf, kff, kef,
l!

\ Utility for defining a word
\ 'c' ( "c" -- w )
\ Read character, create new word then push its address.
\ 'latest' will not be updated.
h@l@, k1k0-h@$ kch@k1k0-+$ h@C+h!
    i, 'h, '@, 'l, '@, ',,
    'L, k1k0-, 'h, '@, '$,                  \ fill 1
    'k, 'h, '@, 'L, k1k0-, '+, '$,          \ fill "c"
    'L, k0k0-, 'h, '@, 'L, k2k0-, '+, '$,   \ fill "\0"
    'h, '@, 'C, '+, 'h, '!,
'e, l!

\ '_' ( a -- ) DROP
c_ i, 'd, 'C, '+, 'D, 'e, l!

\ '#' ( a -- a a )  DUP
c# i, 'd, '@, 'e, l!



\ Implementations of TOR and FROMR are a bit tricky.
\ Since return-address will be placed at the top of return stack,
\ the code in the body of these function have to manipulate
\ 2nd element of the stack.

\ '{' ( a -- R:a ) TOR
\ Move value from data stack to return stack.
c{ i,
    'r, 'r, '@,     \ ( a rsp ret )
    'r, 'C, '-, '#, \ ( a rsp ret rsp-1 rsp-1 )
    'R,             \ ( a rsp+1 ret rsp ) extend return stack
    '!,             \ ( a rsp+1 ) store return address to the top
    '!,             \ store a to the 2nd
'e, l!

\ '}' ( R:a -- a ) FROMR
\ Move value from return stack to data stack.
c} i,
    'r, 'C, '+, '@, \ ( a ) load 2nd value
    'r, '@,         \ ( a ret ) load return addr
    'r, 'C, '+, '#, \ ( a ret rsp+1 rsp+1 )
    'R,             \ ( a ret rsp ) reduce return stack
    '!,             \ ( a , R:ret ) store return addr to top of return stack
'e, l!

\ 'o' ( a b -- a b a ) OVER
co i, 'd, 'C, '+, '@, 'e, l!

\ '~' ( a b -- b a ) SWAP
c~ i,
    'o,             \ ( a b a )
    '{,             \ ( a b , R:a )
    'd, 'C, '+,     \ ( a b sp+1 , R:a )
    '!,             \ ( b , R:a )
    '},             \ ( b a )
'e, l!

\ 'B' ( c -- ) C-COMMA
\ Store byte 'c' to here and increment it
cB i, 'h, '@, '$, 'h, '@, 'L, k1k0-, '+, 'h, '!, 'e, l!

\ 'm' ( c-addr u -- ) CMOVE,
\ Copy u bytes from c-addr to here,
\ increment here u bytes.
cm i,
\ <loop>
    '#, 'J, k>k0-C*,        \ goto <exit> if u=0
        '{,                 \ preserve u
        '#, '?, 'B,         \ copy byte
        'L, k1k0-, '+,      \ increment c-addr
        '}, 'L, k1k0-, '-,  \ decrement u
        'j, k0k?-C*,        \ goto <loop>
\ <exit>
    '_, '_,
'e, l!

\ 'a' ( c-addr -- a-addr ) ALIGNED
\ Round up to a nearest multiple of CELL
ca i,
    'L, Ck1k0--, '+,    \ ( a+CELL-1 )
    'L, k0k0-C-,        \ ( a+CELL-1 ~(CELL-1) )
    '&,
'e, l!

\ 'A' ( -- ) ALIGN
\ Round up 'here' to a nearest multiple of CELL
cA i, 'h, '@, 'a, 'h, '!, 'e, l!

\ 'E' ( c-addr1 c-addr2 -- flag ) STR=
\ Compare null-terminated strings.
\ Return 1 if they are same 0 otherwise.
cE i,
\ <loop>
    'o, '?, 'o, '?,         \ ( c-addr1 c-addr2 c1 c2 )
    'o, '=, 'J, k=k0-C*,    \ goto <not_equal> if c1<>c2
    'J, kAk0-C*,            \ goto <equal> if c1==0
    'L, k1k0-, '+, '~,      \ increment c-addr2
    'L, k1k0-, '+, '~,      \ increment c-addr1
    'j, k0kC-C*,            \ goto <loop>
\ <not_equal>
    '_, '_, '_, 'L, k0k0-, 'e,
\ <equal>
    '_, '_, 'L, k1k0-, 'e,
l!

\ 'z' ( c-addr -- u ) STRLEN
\ Calculate length of string
cz i,
    'L, k0k0-,  \ 0
\ <loop>
    'o, '?, 'J, k;k0-C*,    \ goto <exit> if '\0'
    'L, k1k0-, '+,  '~,     \ increment u
    'L, k1k0-, '+,  '~,     \ increment c-addr
    'j, k0k=-C*,            \ goto <loop>
\ <exit>
    '~, '_, 'e,
l!

\ 's' ( c -- n)
\ Return 1 if c==' ' or c=='\n', 0 otherwise.
cs i, '#, 'L, k , '=, '~, 'L, k:k0-, '=, '|, 'e, l!

\ 'W' ( "name" -- c-addr )
\ Skip leading spaces (' ' and '\n'),
\ Read name, then return its address.
\ The maximum length of the name is 63. The behavior is undefined
\ when the name exceeds 63 characters.
\ The buffer will be terminated with '\0'.
\ Note that it returns the address of statically allocated buffer,
\ so the content will be overwritten each time 'W' executed.

\ Allocate buffer of 63+1 bytes or more,
\ push the address for compilation of 'W'
h@ # kpk0-+ h! A
cW~
i,
    \ skip leading spaces
    'k, '#, 's, 'J, k4k0-C*, '_, 'j, k0k7-C*,
    \ p=address of buffer
    'L, #, '~,
\ <loop>
    \ ( p c )
    'o, '$,                     \ store c to p
    'L, k1k0-, '+,              \ increment p
    'k, '#, 's, 'J, k0k9-C*,    \ goto <loop> if c is not space
    '_,
    'L, k0k0-, 'o, '$,           \ fill \0
    '_, 'L, ,                    \ return buf
'e, l!

\ 'F' ( c-addr -- w )
\ Lookup multi-character word from dictionary.
\ Return 0 if the word is not found.
\ Entries with smudge-bit=1 are ignored.
cF i,
    'l, '@,
\ <loop> ( addr it )
    '#, 'J, kEk0-C*,        \ goto <exit> if it=NULL
        '#, 'C, '+, '?,     \ ( addr it len+flag )
        'L, k@, '&,         \ test smudge-bit of it
        'J, k4k0-C*,
\ <1>
            \ smudge-bit=1
            '@,             \ load link
            'j, k0k>-C*,    \ goto <loop>
\ <2>
            \ smudge-bit=0
            'o, 'o,                     \ ( addr it addr it )
            'L, Ck1k0-+, '+,        \ address of name
            \ ( addr1 it addr1 addr2 )
            'E, 'J, k0k:-C*,            \ goto <1> if different name
\ <exit>
    '{, '_, '}, \ Drop addr, return it
'e, l!

\ 'G' ( w -- xt )
\ Get CFA of the word
cG i,
    'C, '+, '#, '?, \ ( addr len+flag )
    'L, kok0-, '&,  \ take length
    '+,             \ add length to the addr
    'L, k2k0-, '+,  \ add 2 to the addr (len+field and \0)
    'a,             \ align
'e, l!

\ 'M' ( -- a-addr)
\ The state variable
\ 0: immediate mode
\ 1: compile mode
h@ k0k0-,   \ allocate 1 cell and fill 0
cM~ i, 'L, , 'e, l!

\ 'I'
\ The 2nd Phase Interpreter
cI i,
\ <loop>
    'W,                 \ read name from input
    'F,                 \ find word
    'M, '@,             \ read state
    'J, kAk0-C*,        \ goto <immediate> if state=0
\ <compile>
        '#, 'C, '+, '?, \ ( w len+flag )
        'L, k@k@+, '&,  \ test immediate bit
        'L, k0k0-, '=,
        'J, k5k0-C*,    \ goto <immediate> if immediate-bit=1
        'G, ',,         \ compile
        'j, k0kE-C*,    \ goto <loop>
\ <immediate>
        'G, 'x,         \ execute
        'j, k0kI-C*,    \ goto <loop>
l!

I \ Enter 2nd Phase

\ === 2nd Phase Interpreter ===

} _     \ Drop 1st phase interpreter from call stack

\ '\'' ( "name" -- xt )
\ Redefine existing '\'' which uses 'k' and 'f'
\ to use 'W' and 'F'.
c ' i , ' W , ' F , ' G , ' e , l !

\ [ immediate ( -- )
\ Switch to immediate mode
c [ i , ' L , k 0 k 0 - , ' M , ' ! , ' e , l !
\ Set immediate-bit of [
l @ C + # { ? k @ k @ + | } $

\ ] ( -- )
\ Switch to compile mode
c ] i , ' L , k 1 k 0 - , ' M , ' ! , ' e , l !

\ : ( "name" -- ) COLON
\ Read name, create word with smudge=1,
\ compile 'docol' and enter compile mode.
c : i ,
    ' A ,                   \ align here
    ' h , ' @ ,
    ' l , ' @ , ' , ,       \ fill link
    ' l , ' ! ,             \ update latest
    ' W ,                   \ read name ( addr )
    ' # , ' z , ' # ,       \ ( addr len len )
    ' L , k @ , ' | ,       \ set smudge-bit
    ' B ,                   \ fill length + smudge-bit
    ' m ,                   \ fill name
    ' L , k 0 k 0 - , ' B , \ fill \0
    ' A ,                   \ align here
    ' i , ' , ,             \ compile docol
    ' ] ,                   \ enter compile mode
' e , l !

\ ; ( -- ) SEMICOLON
\ Compile 'exit', unsmudge latest, and enter immediate mode.
c ; i ,
    ' A ,               \ align here
    ' L , ' e , ' , ,   \ compile exit
    ' l , ' @ ,
    ' C , ' + , ' # , ' ? ,
    ' L , k [ k d + ,   \ 0xbf
    ' & , ' ~ , ' $ ,   \ unsmudge
    ' [ ,               \ enter immediate mode
' e , l !
\ Set immediate-bit of ';'
l @ C + # { ? k @ k @ + | } $

: immediate-bit [ ' L , k @ k @ + , ] ; \ 0x80
: smudge-bit    [ ' L , k @ , ] ;       \ 0x40
: length-mask   [ ' L , k o k 0 - , ] ; \ 0x3f

\ ( "name" -- )
: set-immediate
    W F C + # { ? immediate-bit | } $
;

\ Set immediate-bit of single-line comment word \
\ so that we can write comments in compile-mode.
set-immediate \

\ Set immediate-bit of 'latest'
: immediate
    l @ C + # { ? immediate-bit | } $
;

: alias-builtin \ ( "name-new" "name-old" -- )
    \ Create new word "name-new".
    \ Copy code pointer of builtin word "name-old" to
    \ the new word "name-new".
    \ "name-old" must not be a FORTH word.
    A h @ l @ , l !         \ fill link, update latest
    W # z # B m             \ fill length and chars of "name-new"
    [ ' L , k 0 k 0 - , ] B \ fill \0
    A
    W F G @ ,               \ fill code-pointer of "name-old"
;

\ Add new names to builtin primities.
\ Instead of defining as a new FORTH word like shown below,
\ the aliases are created by copying their code-pointer.
\ : new-name old-name ;
\ Primitive operators which manipulate program counter and return stack
\ can not be defined as a FORTH word.

alias-builtin quit      Q
alias-builtin cell      C
alias-builtin &here     h
alias-builtin &latest   l
alias-builtin emit      t
alias-builtin branch    j
alias-builtin 0branch   J
alias-builtin execute   x
alias-builtin c@        ?
alias-builtin c!        $
alias-builtin sp@       d
alias-builtin sp!       D
alias-builtin rp@       r
alias-builtin rp!       R
alias-builtin docol     i
alias-builtin exit      e
alias-builtin lit       L
alias-builtin litstring S
alias-builtin /mod      /
alias-builtin and       &
alias-builtin or        |
alias-builtin xor       ^
alias-builtin u<        u
alias-builtin lshift    (
alias-builtin rshift    )
alias-builtin arshift   %
alias-builtin runtime-info_ V

: bye [ ' L , k 0 k 0 - , ] quit ;

\ Rename existing FORTH words
: >cfa      G ;
: c,        B ;
: memcpy,   m ;
: strlen    z ;
: streq     E ;
: state     M ;
: aligned   a ;
: align     A ;

: here      &here @ ;
: latest    &latest @ ;
: >dfa >cfa cell + ;

alias-builtin key k
: word W ;
: find F ;
: ' word find >cfa ;

\ === Compilers ===

\ compile: ( n -- )
\ runtime: ( -- n )
: literal
    lit lit ,   \ compile lit
    ,           \ compile n
; immediate

\ compile: ( "name" -- )
\ '[compile] word' compiles word *now* even if it is immediate
: [compile]
    ' ,
; immediate

\ ( xt -- )
\ postpone compilation of xt
: (compile)
    [compile] literal   \ compile 'literal'
    [ ' , ] literal ,   \ compile ,
;

\ compile: ( "name" -- )
\ 'compile word' compiles word *later* even if it is immediate
: compile
    ' (compile)
; immediate

\ runtime: ( w -- )
: compile, , ;

\ ( -- xt )
: :noname
    align
    here latest , &latest !
    smudge-bit c,       \ length 0
    align
    here
    [ docol ] literal , \ compile docol
    ]                   \ enter compile mode
;

\ ( "name" -- xt )
\ compile time tick
: [']
    '                   \ read name and get xt
    [compile] literal   \ call literal
; immediate

\ === Constants ===

\ Since we don't have integer literals yet,
\ define small integer words for convenience
\ and readability.
: 0     [ key 0 key 0 - ] literal ;
: 1     [ key 1 key 0 - ] literal ;
: 2     [ key 2 key 0 - ] literal ;
: 3     [ key 3 key 0 - ] literal ;
: 4     [ key 4 key 0 - ] literal ;
: 5     [ key 5 key 0 - ] literal ;
: 6     [ key 6 key 0 - ] literal ;
: 7     [ key 7 key 0 - ] literal ;
: 8     [ key 8 key 0 - ] literal ;
: 9     [ key 9 key 0 - ] literal ;
: 10    [ key : key 0 - ] literal ;
: 11    [ key ; key 0 - ] literal ;
: 12    [ key < key 0 - ] literal ;
: 13    [ key = key 0 - ] literal ;
: 14    [ key > key 0 - ] literal ;
: 15    [ key ? key 0 - ] literal ;
: 16    [ key @ key 0 - ] literal ;
: -1    [ key 0 key 1 - ] literal ;

: true 1 ;
: false 0 ;

\ === Address Arithmetic ===

: cell+ cell + ;
: cell- cell - ;
: cells cell * ;
: char+ 1 + ;
: char- 1 - ;
: chars ;

\ === Stack Manipulation ===

: drop  sp@ cell+ sp! ;     \ ( w -- )
: dup   sp@ @ ;             \ ( w -- w w )

: >r rp@ rp@ @ rp@ cell - dup rp! ! ! ;         \ ( w -- R:w )
: r> rp@ cell + @ rp@ @ rp@  cell + dup rp! ! ; \ ( R:w -- w)
: r@ rp@ cell + @ ; \ ( -- w, R: w -- w )

: swap  sp@ cell + dup @ >r ! r> ;  \ ( a b -- b a )
: rot   >r swap r> swap ;           \ ( a b c -- b c a )
: -rot  swap >r swap r> ;           \ ( a b c -- c a b )
: nip   swap drop ;                 \ ( a b -- b )
: over  >r dup r> swap ;            \ ( a b -- a b a )
: tuck  dup -rot ;                  \ ( a b -- b a b )
: pick  cells sp@ + cell + @ ;      \ ( xu ... x0 u -- xu ... x0 xu )


: 2drop drop drop ;                 \ ( a b -- )
: 3drop 2drop drop ;                \ ( a b c -- )
: 2dup  over over ;                 \ ( a b -- a b a b )
: 3dup  2 pick 2 pick 2 pick ;      \ ( a b c -- a b c a b c )
: 2swap >r -rot r> -rot ;           \ ( a b c d -- c d a b )
: 2nip  2swap 2drop ;               \ ( a b c d -- c d )
: 2over 3 pick 3 pick ;             \ ( a b c d -- a b c d a b )
: 2tuck 2swap 2over ;               \ ( a b c d -- c d a b c d )
: 2rot  >r >r 2swap r> r> 2swap ;   \ ( a b c d e f -- c d e f a b )
: -2rot 2swap >r >r 2swap r> r> ;   \ ( a b c d e f -- e f a b c d )

: rdrop r> rp@ ! ;  \ ( R:w -- )

\ ( R xu ... x0 u -- xu ... x0 xu )
: rpick
    cells rp@ + cell + @
;

\ ( -- a-addr )
\ The bottom address of stacks.
\ sp@ and rp@ points bottom if runtime so far is correct.
: sp0 [ sp@ ] literal ;
: rp0 [ rp@ ] literal ;

\ === Integer Arithmetic ===

: 1+ 1 + ;
: 1- 1 - ;

: /     /mod swap drop ;
: mod   /mod drop ;

\ ( n -- -n )
: negate 0 swap - ;

\ ( n1 -- n2 )
: not false = ;

\ ( n1 -- n2 )
\ bitwise invert
: invert -1 xor ;

: >     swap < ;
: <=    > not ;
: >=    < not ;
: u>    swap u< ;
: u<=   u> not ;
: u>=   u< not ;
: <>    = not ;

: 0=    0 = ;
: 0<>   0 <> ;
: 0<    0 < ;
: 0>    0 > ;
: 0<=   0 <= ;
: 0>=   0 >= ;

\ ( x a b -- f )
\ Returns a <= x & x < b if a <= b.
\ It is equivalent to x-a u< b-a. See chapter 4 of
\ Hacker's delight.
: within over - >r - r> u< ;

\ arithmetic shift
: 2* 1 lshift ;
: 2/ 1 arshift ;

\ === Conditional Branch ===
\ <condition> if <if-true> then
\ <condition> if <if-true> else <if-false> then
\ <condition> unless <if-false> then
\ <condition> unless <if-false> else <if-true> then

\ compile: ( -- orig )
\ runtime: ( n -- )
: if
    compile 0branch
    here 0 ,    \ save location of offset, fill dummy
; immediate

\ compile: ( orig -- )
\ runtime: ( -- )
: then
    here        \ ( orig dest )
    over -      \ ( orig offset )
    swap !      \ fill offset to orig
; immediate

\ compile: ( orig1 -- orig2 )
\ runtime: ( -- )
: else
    compile branch
    here 0 ,    \ save location of offset, fill dummy
    swap
    \ fill offset, here-orig1, to orig1
    here
    over -
    swap !
; immediate

\ compile: ( -- orig )
\ runtime: ( n -- )
: unless
    compile not
    [compile] if
; immediate

\ ( n -- n n | n )
\ duplicate if n<>0
: ?dup dup if dup then ;

\ === Loops ===
\ begin <body> <condition> until
\ begin <body> again
\ begin <condition> while <body> repeat

\ compile: ( -- dest )
\ runtime: ( -- )
: begin
    here        \ save location
; immediate

\ compile: ( dest -- )
\ runtime: ( n -- )
: until
    compile 0branch
    here - ,    \ fill offset
; immediate

\ compile: ( dest -- )
\ runtime: ( -- )
: again
    compile branch
    here - ,    \ fill offset
; immediate

\ compile: ( dest -- orig dest )
\ runtime: ( n -- )
\ dest=location of begin
\ orig=location of while
: while
    compile 0branch
    here swap
    0 ,        \ save location, fill dummy
; immediate

\ compile: ( orig dest -- )
\ runtime: ( -- )
\ dest=location of begin
\ orig=location of while
: repeat
    compile branch
    here - ,                \ fill offset from here to begin
    here over - swap !      \ backfill offset from while to here
; immediate

\ === Recursive Call ===

\ recursive call.
\ compiles xt of current definition
: recurse
    latest >cfa ,
; immediate

\ === Case ===

\ ---
\ <value> case
\   <value1> of <case1> endof
\   <value2> of <case2> endof
\   ...
\   <default case>
\ endcase
\ ---
\ This is equivalent to
\ ---
\ <value>
\ <value1> over = if drop <case1> else
\ <value2> over = if drop <case2> else
\ ...
\ <default case>
\ then ... then then
\ ---


\ compile: ( -- 0 )
\ runtime: ( n -- )
: case
    0       \ push 0 to indicate there is no more case
; immediate

\ compile: ( -- orig )
: of
    compile over
    compile =
    [compile] if
    compile drop
; immediate

\ <value> a b rangeof <body> endof
\ Execute <body> when
\ a <= <value> and <value> <= b
: rangeof
    compile 2
    compile pick
    compile >=
    compile swap
    compile 2
    compile pick
    compile <=
    compile and
    [compile] if
    compile drop
; immediate

\ compile: ( orig1 -- orig2 )
: endof
    [compile] else
; immediate

: endcase
    begin ?dup while
        [compile] then
    repeat
; immediate

\ === Integer Arithmetic (that require control flow words) ===
\ ( a b -- c )
: max 2dup > if drop else nip then ;
: min 2dup < if drop else nip then ;

: abs dup 0< if negate then ;

\ === Multiline Comment ===

: '('   [ key ( ] literal ;
: ')'   [ key ) ] literal ;

: (
    1   \ depth counter
    begin ?dup while
        key case
        '(' of 1+ endof \ increment depth
        ')' of 1- endof \ decrement depth
        drop
        endcase
    repeat
; immediate


(
    Now we can use multiline comment with ( nests. )
)

( === Memory Operation === )

: +! ( n a-addr -- ) tuck @ + swap ! ;
: -! ( n a-addr -- ) tuck @ swap - swap ! ;

\ allocate n bytes
: allot ( n -- ) &here +!  ;

( === create and does> === )

\ no-operation
: nop ;

\ ( "name" -- )
\ Read name and create new dictionary entry.
\ When the word is executed, it pushs value of here
\ at the end of the entry.
: create
    align
    latest ,                    \ fill link
    here cell- &latest !        \ update latest
    word dup strlen
    dup c, memcpy, 0 c, align    \ fill length, name and \0
    docol ,                     \ compile docol
    ['] lit ,
    here 3 cells + ,            \ compile the address
    ['] nop ,                   \ does>, if any, will fill this cell
    ['] exit ,                  \ compile exit
;

: >body ( xt -- a-addr )
    5 cells +
;

: (does>)
    latest >cfa
    3 cells + ! \ replace nop
;

: does>
    align
    0 [compile] literal \ literal for xt
    here cell-          \ save addr of xt

    \ replace nop with xt at runtime
    compile (does>)

    [compile] ; \ finish compilation of initialization part
    :noname     \ start compilation of does> part
    swap !      \ backfill xt to the operand of literal
; immediate

( === Variable and Constant === )

\ ( "name" -- )
: variable create 0 , ;

\ ( n "name" -- )
: constant create , does> @ ;

( === Value === )

\ ( n "name" -- )
: value create , does> @ ;

\ ( n "name" -- )
: to
    word find >cfa >body
    state @ if
        [compile] literal
        compile !
    else
        !
    then
; immediate

( === Printing Numbers === )

\ Skip reading spaces, read characters and returns first character
: char      ( <spaces>ccc -- c ) word c@ ;

\ compile-time version of char
: [char]    ( compile: <spaces>ccc -- ; runtime: --- c )
    char
    [compile] literal
; immediate


: '\n' [ key : key 0 - ] literal ; \ newline (10)
: bl   [ key P key 0 - ] literal ; \ space (32)
: '"'  [char] "" ;

: cr    '\n' emit ;
: space bl emit ;


variable base   \ number base
: decimal   10 base ! ;
: hex       16 base ! ;

decimal \ set default to decimal

: '0' [char] 0 ;
: '9' [char] 9 ;
: 'a' [char] a ;
: 'x' [char] x ;
: 'z' [char] z ;
: 'A' [char] A ;
: 'Z' [char] Z ;
: '-' [char] - ;
: '&' [char] & ;
: '#' [char] # ;
: '%' [char] % ;
: '$' [char] $ ;
: '\'' [char] ' ;
: '\\' [char] \ ;
: 'a'  [char] a ;
: 'b'  [char] b ;
: 't'  [char] t ;
: 'n'  [char] n ;
: 'v'  [char] v ;
: 'f'  [char] f ;
: 'r'  [char] r ;

\ Display unsigned integer u2 with number base u1.
: print-uint ( u1 u2 -- )
    over /mod   ( base mod quot )
    ?dup if
        >r over r> \ ( base mod base quot )
        recurse
    then
    dup 10 < if '0' + else 10 - 'a' + then emit
    drop
;

\ Display signed integer n with number base u.
: print-int ( u n -- )
    dup 0< if '-' emit negate then
    print-uint
;

\ Display unsigned integer followed by a space.
: u. ( u -- ) base @ swap print-uint space ;

\ Display n followed by a space.
: . ( n -- ) base @ swap print-int space ;

\ Display n as a signed decimal number followed by a space.
: dec. ( n -- ) 10 swap print-int space ;

\ Display u as an unsigned hex number prefixed with $
\ and followed by a space.
: hex. ( u -- ) '$' emit 16 swap print-uint space ;

\ Number of characters of u in 'base'
: uwidth ( u -- u )
    base @ /
    ?dup if recurse 1+ else 1 then
;

: spaces ( n -- )
    begin dup 0> while space 1- repeat drop
;

\ Display unsigned integer u right aligned in n characters.
: u.r ( u n -- )
    over uwidth
    - spaces base @ swap print-uint
;

\ Display signed integer n1 right aligned in n2 characters.
: .r ( n1 n2 -- )
    over 0>= if
        u.r
    else
        swap negate
        dup uwidth 1+
        rot swap - spaces
        '-' emit
        base @ swap print-uint
    then
;

( === Parsing Numbers === )

\ Parse string c-addr as an unsigned integer with base u
\ and return n. f represents the conversion is success or not.
: parse-uint ( u c-addr -- n f )
    0   \ accumulator
    begin over c@ while
        \ ( base addr acc )
        >r                  \ save acc
        dup c@ swap 1+ >r   \ load char, increment addr and save
        dup case
        '0' '9' rangeof '0' - endof
        'a' 'z' rangeof 'a' - 10 + endof
        'A' 'Z' rangeof 'A' - 10 + endof
            \ failed to convert
            2drop r> r> nip false
            exit
        endcase
        2dup
        \ ( base n base n )
        swap 0 swap
        \ ( base n n 0 base )
        within unless
            \ failed to convert
            2drop r> r> nip false
            exit
        then
        \ ( base addr n acc )
        r> swap r>
        3 pick * +
    repeat
    \ success
    nip nip true
;

: parse-int ( u c-addr -- n f )
    dup c@ '-' = if
        1+ parse-uint swap negate swap
    else
        parse-uint
    then
;

\ Return ascii-code of corresponding escaped char
\ e.g '\n' escaped-char -> 10
: escaped-char ( n -- n )
    case
    '0' of 0 endof
    'a' of 7 endof
    'b' of 8 endof
    't' of 9 endof
    'n' of 10 endof
    'v' of 11 endof
    'f' of 12 endof
    'r' of 13 endof
    [char] ' of [char] ' endof
    [char] " of [char] " endof
    '\\' of '\\' endof
    drop -1
    endcase
;

\ Parse string as number.
\ This function interprets prefixes that specifies number base.
: >number ( c-addr -- n f )
    dup c@ unless
        drop
        0 false
        exit
    then
    dup c@ case
    '-' of
        1+
        recurse if
            negate true
        else
            false
        then
    endof
    '&' of 1+ 10 swap parse-int endof
    '#' of 1+ 10 swap parse-int endof
    '$' of 1+ 16 swap parse-int endof
    '%' of 1+ 2 swap parse-int endof
    '0' of
        \ hexadecimal
        \ ( addr )
        1+
        dup c@ unless
            drop 0 true exit
        then
        dup c@ 'x' = if
            1+ 16 swap parse-uint exit
        then
        drop 0 false exit
    endof
    '\'' of
        \ character code
        \ ( addr )
        1+
        dup c@ unless
            drop 0 false exit
        then
        dup c@ '\\' = if
            1+ dup c@ escaped-char swap 1+
        else
            dup c@ swap 1+
        then
        c@ case
        0 of true exit endof
        '\'' of true exit endof
            drop 0 false
        endcase
    endof
        \ default case
        \ ( addr base )
        drop base @ swap parse-uint
    endcase
;

( === String === )

\ c-addr2 = c-addr1+n
\ u2 = u1-n
: succ-buffer ( c-addr1 u1 n -- c-addr2 u2 )
    dup -rot - >r + r>
;

\ ( c-from c-to u -- )
\ Copy u bytes from c-from to c-to.
\ The memory regions must not be overlapped.
: memcpy
    begin dup 0> while
        1- >r   \ decrement u, save
        over c@
        over c! \ copy character
        1+ >r   \ increment c-to, save
        1+      \ increment c-from
        r> r>
    repeat 3drop
;

\ we already have memcpy,

\ ( c-from c-to -- )
\ copy nul terminated string from c-from to c-to
: strcpy
    begin over c@ dup while
        \ ( c-from c-to c )
        over c!
        1+ swap 1+ swap
    repeat
    over c!
    2drop
;

\ ( c-addr -- )
\ copy string to here including \0
: strcpy,
    begin dup c@ dup while
        c, 1+
    repeat 2drop
    0 c,
;

\ ( c-from c-to u -- )
: strncpy
    begin dup 0> while
        >r
        \ ( c-from c-to )
        over c@ over c!
        over c@ unless r> 3drop exit then
        1+ swap 1+ swap r> 1-
    repeat
    drop 1- 0 swap c! drop
;

\ ( c-addr1 c-addr2 u -- f )
: strneq
    begin dup 0> while
        1- >r
        dup 1+ >r c@
        swap dup 1+ >r c@
        <> if rdrop rdrop rdrop false exit then
        r> r> r>
    repeat
    3drop true
;

\ Print string
: type ( c-addr -- )
    begin dup c@ dup while  \ while c<>\0
        emit 1+
    repeat
    2drop
;

\ Print string up to u characters
: typen ( c-addr u -- )
    begin dup 0> while
        1- swap dup c@ dup unless
            2drop exit
        then
        emit 1+ swap
    repeat 2drop
;

\ Allocate a buffer for string literal
bl bl * constant s-buffer-size  \ 1024
create s-buffer s-buffer-size allot

\ Parse string delimited by "
\ compile mode: the string is stored as operand of 'string' operator.
\ immediate mode: the string is stored to temporary buffer.
: s"
    state @ if
        compile litstring
        here 0 ,    \ save location of length and fill dummy
        0           \ length of the string + 1 (\0)
        begin key dup '"' <> while
            dup '\\' = if drop key escaped-char then
            c,      \ store character
            1+      \ increment length
        repeat drop
        0 c,        \ store \0
        1+ aligned
        swap !      \ back-fill length
        align
    else
        s-buffer dup    \ save start address
        begin key dup '"' <> while
            dup '\\' = if drop key escaped-char then
            over c! \ store char
            1+      \ increment address
        repeat drop
        0 swap c!   \ store \0
    then
; immediate

\ Print string delimited by "
: ."
    [compile] s"
    state @ if
        compile type
    else
        type
    then
; immediate

( === 3rd Phase Interpreter === )
create word-buffer s" 64" >number drop cell+ allot

: interpret
    word                   \ read name from input
    \ ( addr )
    dup word-buffer strcpy  \ save input
    dup find                \ lookup dictionary
    ?dup if
        \ Found the word
        nip
        state @ if
            \ compile mode
            dup cell+ c@ immediate-bit and if
                \ execute immediate word
                >cfa execute
            else
                \ compile the word
                >cfa ,
            then
        else
            \ immediate mode
            >cfa execute
        then
    else
        >number drop
        \ Not found
        state @ if
            \ compile mode
            [compile] literal
        then
    then
;

:noname
    rp0 rp! \ drop 2nd phase
    begin
        interpret
    again
; execute

decimal

( === Do-loop === )

\ limit start do ... loop

1 constant do-mark
2 constant leave-mark

create do-stack 16 cells allot
variable do-sp
do-stack 16 cells + do-sp !

: >do ( w -- do: w )
    cell do-sp -!
    do-sp @ !
;

: do> ( do: w -- w )
    do-sp @ @
    cell do-sp +!
;

: do@ ( do: w -- w, do: w)
    do-sp @ @
;

\ compile: ( -- do: dest mark )
: do
    compile >r  \ save start
    compile >r  \ save limit
    here >do do-mark >do
; immediate

\ compile: ( -- ... )
: ?do
    compile 2dup
    compile >r  \ save start
    compile >r  \ save limit
    compile <>
    compile 0branch
    0 ,
    here >do do-mark >do
    here cell- >do leave-mark >do
; immediate

: leave ( -- do: orig mark )
    compile branch
    here >do
    0 ,        \ fill dummy offset
    leave-mark >do
; immediate

: backpatch-leave ( dest , do: orig1 mark1 ... -- do: origN markN ... )
    begin do@ leave-mark = while
        do> drop do>
        2dup -
        swap !
    repeat
    drop
;

: loop
    compile r>
    compile r>
    compile 1+
    compile 2dup
    compile >r
    compile >r
    compile =
    compile 0branch
    here cell + backpatch-leave     \ leave jumps to here
    do> drop            \ do-mark
    do> here - ,
    compile rdrop
    compile rdrop
; immediate

\ This code is taken from Gforth
: crossed-boundary? ( d n i )
    swap -      ( d i-n )
    2dup +      ( d i-n i+d-n )
    over xor    ( d i-n (i-n)^(i+d-n) )
    >r xor r>   ( d^(i-n) (i^n)^(i+d-n) )
    and 0<
;

: +loop
    compile r>
    compile r>
    compile 3dup
    compile rot
    compile +
    compile >r
    compile >r
    compile crossed-boundary?
    compile 0branch
    here cell + backpatch-leave     \ leave jumps to here
    do> drop            \ do-mark
    do> here - ,
    compile rdrop
    compile rdrop
; immediate

: unloop ( R:a b -- )
    compile rdrop
    compile rdrop
; immediate

: i 2 rpick ;
: j 4 rpick ;
: k 6 rpick ;

( === Dump of data stack === )

\ ( -- n )
\ Number of elements in the stack
: depth     sp0 sp@ - cell- cell / ;
: rdepth    rp0 rp@ - cell / ;

: .s ( -- )
    depth
    '<' emit 0 u.r '>' emit space
    sp@ sp0 ( beg end )
    begin 2dup < while
        cell- dup @ .
    repeat 2drop
    cr
;

( === Data Structure === )

\ align n1 to u-byte boundary
: aligned-by ( n1 u -- n2 )
    1- dup invert   \ ( n1 u-1 ~(u-1) )
    -rot + and
;

\ align here to u-byte boundary
: align-by ( u -- )
    here swap aligned-by &here !
;

: struct ( -- offset )
    0
;

\ struct ... end-struct new-word
\ defines new-word as an operator
\ that returns alignment and size of the struct.
\ new-word: ( -- align size )
: end-struct ( offset "name" -- )
    create , does> @ cell swap
;

: cell% ( -- align size ) cell cell ;
: char% ( -- align size ) 1 1 ;
: byte% 1 1 ;
: ptr% cell% ;
: int% cell% ;
: i32% 4 4 ;
: u32% 4 4 ;
: i16% 2 2 ;
: u16% 2 2 ;

\ allocate user memory
: %allot ( align size -- addr )
    here -rot swap align-by allot
;

: field ( offset1 align size "name" -- offset2 )
    \ align offset with 'align'
    -rot aligned-by \ ( size offset )
    create
        dup ,   \ fill offset
        +       \ return new offset
    does> @ +
;

( === End of bootstrap of PlanckForth === )
( === Implementation of PlanckLISP === )

\ :noname
\    begin
\        parse-sexp
\        eval-sexp
\        drop
\    again
\ ; execute
