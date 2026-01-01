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
: allot ( n -- ) aligned &here +!  ;

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

( === Throw and Catch === )

\ 'xt catch' saves data stack pointer and a marker
\ to indicate where to return on return stack
\ then execute 'xt'.
\ When 'n throw' is executed, the catch statement returns
\ 'n'. If no throw is executed, returns 0.

\ At the beginning of execution of 'xt', return stack
\ contains following information.
\ +-------------------------+
\ | original return address |
\ | saved stack pointer     |
\ | exception marker        | <- top of return stack
\ +-------------------------+
\ If no 'throw' is called, after execution of 'xt'
\ program goes to the exception-marker because it is
\ on the top of return stack.
\ The exception-marker drops 'saved stack pointer',
\ push 0 to indicate no error and return to the
\ 'original return address'.
\ When 'n throw' is called, it scans return stack
\ to find the exception-marker, restore return stack pointer
\ and data stack pointer, push error code, and returns to
\ the 'original return address'

create exception-marker
    ' rdrop ,   \ drop saved stack pointer
    0 literal   \ push 0 to indicate no-error
    ' exit ,

: catch ( xt -- n )
    sp@ cell+ >r            \ save stack pointer
    exception-marker >r     \ push exception marker
    execute
;

: success 0 ;

: throw ( w -- )
    ?dup unless exit then   \ do nothing if no error
    rp@
    begin
        dup rp0 cell- <     \ rp < rp0
    while
        dup @               \ load return stack entry
        exception-marker = if
            rp!     \ restore return stack pointer
            rdrop   \ drop exception marker

            \ Reserve enough working space of data stack since
            \ following code manipulates data stack pointer
            \ and write value to data stack directly via
            \ address.
            dup dup dup dup

            r>      \ original stack pointer
            \ ( n sp )
            cell-   \ allocate space for error code
            tuck !  \ store error code of top of stack
            sp!     \ restore data stack pointer
            exit
        then
        cell+
    repeat
    drop
;

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

: '0' [char] 0 ;
: '9' [char] 9 ;
: 'a' [char] a ;
: 'z' [char] z ;
: 'A' [char] A ;
: 'Z' [char] Z ;
: 'b' [char] b ;
: 'B' [char] B ;
: 'o' [char] o ;
: 'O' [char] O ;
: 'x' [char] x ;
: 'X' [char] X ;
: '+' [char] + ;
: '-' [char] - ;
: '\'' [char] ' ;
: '\\' [char] \ ;
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
: u. ( u -- ) 10 swap print-uint space ;

\ Display n followed by a space.
: . ( n -- ) 10 swap print-int space ;

\ Display n as a signed decimal number followed by a space.
: dec. ( n -- ) 10 swap print-int space ;

\ Display u as an unsigned hex number prefixed with $
\ and followed by a space.
: hex. ( u -- ) '0' emit 'x' emit 16 swap print-uint space ;

\ Number of characters of u in decimal
: uwidth ( u -- u )
    10 /
    ?dup if recurse 1+ else 1 then
;

: spaces ( n -- )
    begin dup 0> while space 1- repeat drop
;

\ Display unsigned integer u right aligned in n characters.
: u.r ( u n -- )
    over uwidth
    - spaces 10 swap print-uint
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
        10 swap print-uint
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
            2drop r> r> nip nip false
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
        \ null string
        drop 0 false exit
    then
    dup c@ case
        '+' of 1+ recurse endof
        '-' of 1+ recurse if negate true else false then endof
        '0' of
            1+ dup c@ case
                'b' of 1+ 2 swap parse-uint endof
                'B' of 1+ 2 swap parse-uint endof
                'o' of 1+ 8 swap parse-uint endof
                'O' of 1+ 8 swap parse-uint endof
                'x' of 1+ 16 swap parse-uint endof
                'X' of 1+ 16 swap parse-uint endof
                drop
                1- 8 swap parse-uint endof
            endcase
        endof
        '\'' of \ character code
            1+ dup c@ unless drop 0 false exit then
            dup c@ '\\' = if
                1+ dup c@ escaped-char swap 1+
            else
                dup c@ swap 1+
            then
            c@ '\'' = if true else drop 0 false then
        endof
        drop 10 swap parse-uint
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

\ Will define the error message corresponds to this error later
\ because we can't write string literal yet.
char 0 char B - constant STRING-OVERFLOW-ERROR \ -18

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

( === Error Code and Messages === )

\ Single linked list of error code and messages.
\ Thre structure of each entry:
\ | link | code | message ... |
variable error-list
0 error-list !

: error>next    ( a-addr -- a-addr) @ ;
: error>message ( a-addr -- c-addr ) 2 cells + ;
: error>code    ( a-addr -- n ) cell+ @ ;

: add-error ( n c-addr -- )
    error-list here
    ( n c-addr )
    over @ ,    \ fill link
    swap !      \ update error-list
    swap ,      \ fill error-code
    strcpy,     \ fill message
;

: def-error ( n c-addr "name" -- )
    create over ,
    add-error
    does> @
;

STRING-OVERFLOW-ERROR s" Too long string literal" add-error

variable next-user-error
s" -256" >number drop next-user-error !

\ Create new user defined error and returns error code.
: exception ( c-addr -- n )
    next-user-error @ swap add-error
    next-user-error @
    1 next-user-error -!
;

( === 3rd Phase Interpreter === )
s" -13" >number drop s" Undefined word" def-error UNDEFINED-WORD-ERROR
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
        >number unless UNDEFINED-WORD-ERROR throw
        then
        \ Not found
        state @ if
            \ compile mode
            [compile] literal
        then
    then
;

:noname
    rp0 rp! \ drop 2nd stage
    begin
        ['] interpret catch
        ?dup if
            \ lookup error code
            error-list @
            begin ?dup while
                \ ( error-code error-entry )
                dup error>code
                2 pick = if
                    error>message type
                    ." : "
                    word-buffer type cr
                    bye
                then
                error>next
            repeat
            ." Unknown error code: "
            word-buffer type
            ."  (" 0 .r ." )" cr
            bye
        then
    again
; execute

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

: field ( offset1 align size "name" -- offset2 )
    \ align offset with 'align'
    -rot aligned-by \ ( size offset )
    create
        dup ,   \ fill offset
        +       \ return new offset
    does> @ +
;

( === Defer and Is === )
\ Defer and Is enables use of mutual recursion.

\ defer even
\ defer odd

\ : even-impl dup 0= if drop true else 1- odd then ;
\ : odd-impl  dup 0= if drop false else 1- even then ;

\ ' even-impl is even
\ ' odd-impl is odd

: defer ( "name" -- )
    create 0 , does> @ execute
;

: is ( xt "name" -- )
    word find >cfa >body !
;

( === Command-line Arguments === )

variable argc
variable argv
v argc ! argv !

: arg ( u -- c-addr )
    dup argc @ < if
        cells argv @ + @
    else
        drop 0
    then
;

\ Remove 1 arg, update argv and argc
: shift-args ( -- )
    argc @ 1 = if exit then
    argc @ 1 do
        i 1+ arg            \ argv[i+1]
        i cells argv @ +    \ &argv[i]
        !                   \ copy argv[i+1] to argv[i]
    loop
    1 argc -!
;

\ Take 1 arg and shift arguments
: next-arg ( -- c-addr )
    argc @ 1 = if 0 exit then
    1 arg
    shift-args
;

( === Error-codes === )

-1 s" Aborted" def-error ABORTED-ERROR
-37 s" File I/O exception" def-error FILE-IO-ERROR
-39 s" Unexpected end of file" def-error UNEXPECTED-EOF-ERROR
-59 s" ALLOCATE" def-error ALLOCATE-ERROR
-62 s" CLOSE-FILE" def-error CLOSE-FILE-ERROR
-68 s" FLUSH-FILE" def-error FLUSH-FILE-ERROR
-69 s" OPEN-FILE" def-error OPEN-FILE-ERROR
-70 s" READ-FILE" def-error READ-FILE-ERROR
-71 s" READ-LINE" def-error READ-LINE-ERROR
-75 s" WRITE-FILE" def-error WRITE-FILE-ERROR

: abort ABORTED-ERROR throw ;

s" Not implemented" exception constant NOT-IMPLEMENTED
: not-implemented NOT-IMPLEMENTED throw ;

s" Not supported" exception constant NOT-SUPPORTED
: not-supported NOT-SUPPORTED throw ;

( 31 bytes )
s" Not reachable here. may be a bug" exception constant NOT-REACHABLE
: not-reachable NOT-REACHABLE throw ;

( === System Calls === )

0b000 constant eax immediate
0b001 constant ecx immediate
0b010 constant edx immediate
0b011 constant ebx immediate
0b100 constant esp immediate
0b101 constant ebp immediate
0b110 constant esi immediate
0b111 constant edi immediate

: mod-reg-r/m ( mod reg r/m -- u )
    0
    swap 0x7 and or
    swap 0x7 and 8 * or
    swap 0x3 and 64 * or
;

: scale-index-byte ( scale index byte -- u )
    0
    swap 0x7 and or
    swap 0x7 and 8 * or
    swap 0x3 and 64 * or
;

\ compile 'pop reg' and 'push reg'
: pop ( reg -- ) 0x58 + c, ; immediate
: push ( reg -- ) 0x50 + c, ; immediate

\ lodsl; jmp *(%eax);
: next ( -- ) 0xad c, 0xff c, 0x20 c, ; immediate
: int80 ( -- ) 0xcd c, 0x80 c, ; immediate

\ movl disp(reg1), reg2
: movmr ( disp reg1 reg2 -- )
    0x8b c, \ opcode
    swap dup 0b100 = if \ if reg1=esp
        \ ( disp reg2 reg1 )
        0b01 -rot mod-reg-r/m c,
        0b00 0b100 0b100 scale-index-byte c,
    else
        \ ( disp reg2 reg1 )
        0b01 -rot mod-reg-r/m c,
    then
    c, \ displacement
; immediate

\ overwrite code field by DFA
: ;asm
    [compile] ; \ finish compilation
    latest dup >dfa swap >cfa !
; immediate

: syscall0 ( n -- e )
    eax pop
    int80
    eax push
    next
;asm

: syscall1 ( arg1 n -- e )
    eax pop
    ebx pop
    int80
    eax push
    next
;asm

: syscall2 ( arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    int80
    eax push
    next
;asm

: syscall3 ( arg3 arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    edx pop
    int80
    eax push
    next
;asm

: syscall4 ( arg4 arg3 arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    edx pop
    esi push            \ save program counter ( arg4 esi )
    [ 4 ] esp esi movmr     \ movl 4(%esp), %esi
    int80
    esi pop             \ restore esi
    ebx pop
    eax push
    next
;asm

: syscall5 ( arg5 arg4 arg3 arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    edx pop
    esi push            \ save esi ( arg5 arg4 esi )
    [ 4 ] esp esi movmr
    [ 8 ] esp edi movmr
    int80
    esi pop
    ebx pop
    ebx pop
    eax push
    next
;asm

: syscall6 ( arg6 arg5 arg4 arg3 arg2 arg1 n -- e )
    eax pop
    ebx pop
    ecx pop
    edx pop
    esi push
    ebp push    \ ( arg6 arg5 arg4 esi ebp )
    [ 8 ] esp esi movmr
    [ 12 ] esp edi movmr
    [ 16 ] esp ebp movmr
    int80
    ebp pop
    esi pop
    ebx pop
    ebx pop
    ebx pop
    eax push
    next
;asm

\ Allocate u bytes of user memory
: allocate ( u -- addr )
    here swap allot
;

\ allocate user memory
: %allocate ( align size -- addr )
    over 1- + allocate 
    swap 1- tuck + swap invert and
;

\ Bootstrapping version of free do nothing.
: (free) ( addr -- ) drop ;

( === File I/O === )

-1 constant EOF

\ file access methods (fam)
0x000 constant R/O  \ read-only
0x241 constant W/O  \ write-only
0x242 constant R/W  \ read-write

1024 constant BUFSIZE
128 constant FILENAME-MAX

\ File
struct
    cell% field file>fd     \ file descriptor
    cell% field file>read   ( c-addr u fd -- n )
    cell% field file>write  ( c-addr u fd -- n )

    char% field file>fam
    char% FILENAME-MAX * field file>name

    \ read buffer
    cell% field file>rbuf
    cell% field file>rbeg   \ read head
    cell% field file>rend

    \ write buffer
    cell% field file>wbuf
    cell% field file>wbeg   \ write head
    cell% field file>wend
end-struct file%

: writable? ( file -- f ) file>fam c@ R/O <> ;
: readable? ( file -- f ) file>fam c@ W/O <> ;

\ Write buffer
\ +-------------+-----+
\ |aaaaaaaaaaaaa|     |
\ +-------------+-----+
\ ^             ^     ^
\ wbuf          wbeg  wend

: write-buffer-content ( file -- c-addr u )
    dup file>wbeg @ swap file>wbuf @ tuck -
;

: empty-write-buffer ( file -- )
    dup file>wbuf @ over file>wbeg !
    dup file>wbuf @ over file>wend !
    drop
;

: succ-write-buffer ( file n -- )
    swap file>wbeg +!
;

: write-buffer-count ( file -- n )
    dup file>wbeg @ swap file>wbuf @ -
;

\ Read buffer
\ +-------------+-----+
\ |     |aaaaaaa|     |
\ +-------------+-----+
\ ^     ^       ^
\ rbuf  rbeg    rend

: read-buffer-content ( file -- c-addr u)
    dup file>rend @ swap file>rbeg @ tuck -
;

: empty-read-buffer ( file -- )
    dup file>rbuf @ over file>rbeg !
    dup file>rbuf @ over file>rend !
    drop
;

: succ-read-buffer ( file n -- )
    swap file>rbeg +!
;

: read-buffer-count ( file -- n )
    dup file>rend @ swap file>rbeg @ -
;

\ Flush output buffer of file, return error-code.
: flush-file ( file -- e )
    dup writable? unless FLUSH-FILE-ERROR exit then
    dup write-buffer-content ( file buf u )
    begin
        ( file buf u )
        dup 0= if 2drop empty-write-buffer success exit then
        2dup 4 pick file>fd @ 5 pick file>write @ execute
        ( file buf u n )
        dup 0< if 2drop FLUSH-FILE-ERROR exit then
        ( file buf u n )
        2dup < if not-reachable then
        tuck - >r + r>
        ( file buf+n u-n )
    again
;

\ Write bytes from c-addr u to file, return error-code.
: write-file ( c-addr u file -- e )
    dup writable? unless WRITE-FILE-ERROR exit then
    over 0<= if 3drop WRITE-FILE-ERROR exit then

    dup write-buffer-content nip BUFSIZE swap - ( space )
    2 pick
    ( c-addr u file space u )
    >= if
        \ enogu space, copy u-bytes from c-addr to buf
        ( c-addr u file )
        2 pick over file>wbeg @ 3 pick memcpy
        \ increment wbeg
        swap succ-write-buffer drop success exit
    then
    ( c-addr u file buf )
    not-implemented
    dup flush-file throw

    over BUFSIZE <= if
        \ fill data to wbuf
        2 pick over file>wbeg @ 3 pick memcpy
        swap succ-write-buffer drop success exit
    then

    \ write large data directly to the file
    begin
        ( c-addr u file )
        2 pick 2 pick 2 pick file>fd @ 3 pick file>write @ execute
        ( c-addr u file n )
        dup 0< if 2drop 2drop WRITE-FILE-ERROR exit then
        swap >r succ-buffer r>
        over 0>
    until
    empty-write-buffer 2drop success
;

\ Read u1-bytes at most from file, write it to c-addr.
\ Return number of bytes read and error-code.
: read-file ( c-addr u1 file -- u2 e )
    dup readable? unless READ-FILE-ERROR exit then
    over 0<= if 3drop 0 success exit then

    dup read-buffer-count 2 pick ( count u1 )
    >= if
        \ enough data in read buffer
        dup file>rbeg @ 3 pick 3 pick memcpy
        \ increment rbeg
        over succ-read-buffer
        nip success exit
    then

    \ copy rbeg..rend to the buffer
    dup read-buffer-content 4 pick swap memcpy
    ( buf u file )
    dup read-buffer-count dup >r
    ( buf u file n , R:written )
    swap >r succ-buffer r>
    dup empty-read-buffer

    ( buf u file , R:count )
    over BUFSIZE <= if
        \ read data to rbuf as much as BUFSIZE
        dup file>rbuf @ BUFSIZE 2 pick file>fd @ 3 pick file>read @ execute
        dup 0< if 2drop 2drop r> READ-FILE-ERROR exit then
        ( buf u file n , R:count )
        dup 2 pick file>rend +!
        2 pick min
        over file>rbeg @ 4 pick 2 pick memcpy
        dup 2 pick file>rbeg +!
        ( buf u file n , R:count )
        >r 3drop r> r> + success
    else
        \ read large data directly from the file
        dup file>fd @ swap file>read @ execute
        ( n , R:count )
        dup 0< if drop r> READ-FILE-ERROR exit then
        r> + success
    then
;

\ Read a character. Return EOF at end of input.
: key-file ( file -- c )
    0 sp@ 1 3 pick read-file throw
    ( file c u )
    1 = if
        nip
    else
        2drop EOF
    then
;

\ Read characters from 'file' to the buffer c-addr u1
\ until reaches '\n' or end of file, null character is
\ stored at last.
\ u2 is the number of characters written to the buffer.
\ flag=true if it reads '\n'. e is error code.
: read-line ( c-addr u1 file -- u2 e )
    over 1- 0 do
        2 pick i + 1 2 pick read-file
        dup 0< if false leave then
        drop
        ( c-addr u1 file u2 )
        0= if i success false leave then \ EOF
        2 pick i + c@ '\n' = if
            i 1+ success true leave
        then
    loop
    ( c-addr u1 file u2 e flag )
    >r >r
    3 pick over + 0 swap c! \ fill '\0'
    >r 3drop r> r> r> swap
;

3 constant SYS-READ
4 constant SYS-WRITE
5 constant SYS-OPEN
6 constant SYS-CLOSE

: (open) ( c-addr fam -- fd )
    0b110100100 -rot swap SYS-OPEN syscall3
;

: (close) ( obj -- n )
    SYS-CLOSE syscall1
;

: (read) ( c-addr u fd -- n )
    >r swap r> SYS-READ syscall3
;

: (write) ( c-addr u1 fd -- n )
    >r swap r>              \ ( u1 u1 c-addr fd )
    SYS-WRITE syscall3      \ ( u2 )
;

: open-file ( c-addr fam -- file e )
    2dup (open) dup 0< if
        ( c-addr fam fd )
        3drop 0 OPEN-FILE-ERROR exit
    then
    file% %allocate
    tuck file>fd !
    tuck file>fam !
    tuck file>name FILENAME-MAX strncpy
    ['] (read) over file>read !
    ['] (write) over file>write !
    dup file>fam @ W/O <> if
        BUFSIZE allocate over file>rbuf !
        dup file>rbuf @ over file>rbeg !
        dup file>rbuf @ over file>rend !
    then
    dup file>fam @ R/O <> if
        BUFSIZE allocate over file>wbuf !
        dup file>wbuf @ over file>wbeg !
        dup file>wbuf @ BUFSIZE + over file>wend !
    then
    success
;

: close-file ( file -- e )
    dup file>fd @ swap
    ( fd file )
    \ release heap objects
    dup file>rbuf @ (free)
    dup file>wbuf @ (free)
    (free)
    \ close file object
    (close) 0= if success else CLOSE-FILE-ERROR then
;

( === End of bootstrap of PlanckForth === )
( === Implementation of PlanckLISP === )

( === Single Linked List === )
struct
    cell% field list>val
    cell% field list>next
end-struct list%

: list-push ( v list -- new-list )
    list% %allocate
    tuck list>next !
    tuck list>val !
;

: list-push! ( v list -- )
    dup >r @ list-push r> !
;

( === Nodes === )

struct
    cell% field node>type
    cell% field node>arg0
    cell% field node>arg1
    cell% field node>arg2
    cell% field node>arg3
    cell% field node>arg4
end-struct node%

0 constant Nint
1 constant Nsymbol
2 constant Nstr
3 constant Nquote
4 constant Nqquote
5 constant Nunquote
6 constant Nnil
7 constant Ncons
8 constant Nlambda
9 constant Nmacro
10 constant Nprim

: make-node0 ( type -- node )
    1 cells allocate
    tuck node>type !
;

: make-node1 ( arg0 type -- node )
    2 cells allocate
    tuck node>type !
    tuck node>arg0 !
;

: make-node2 ( arg1 arg0 type -- node )
    3 cells allocate
    tuck node>type !
    tuck node>arg0 !
    tuck node>arg1 !
;

: make-node3 ( arg2 arg1 arg0 type -- node )
    4 cells allocate
    tuck node>type !
    tuck node>arg0 !
    tuck node>arg1 !
    tuck node>arg2 !
;

Nnil make-node0 constant nil
: make-cons ( cdr car -- cons )
    Ncons make-node2
;

: make-lambda ( body params env -- node ) Nlambda make-node3 ;
: make-macro ( body params env -- node ) Nmacro make-node3 ;
: make-prim ( xt sym -- node ) Nprim make-node2 ;

: car ( node -- node ) node>arg0 @ ;
: cdr ( node -- node ) node>arg1 @ ;
: caar car car ;
: cadr cdr car ;
: cdar car cdr ;
: cddr cdr cdr ; 
: caddr cdr cdr car ;
: cons-len ( cons -- int )
    dup nil = if drop 0 else cdr recurse 1+ then
;

: make-int ( n -- atom ) Nint make-node1 ;
: to-int ( atom -- n ) node>arg0 @ ;

: make-str ( c-addr -- atom ) Nstr make-node1 ;
: to-str ( atom -- n ) node>arg0 @ ;

variable symlist
0 symlist !

: strdup ( c-addr -- c-addr )
    dup strlen 1+ allocate tuck strcpy
;

: make-symbol ( c-addr -- atom )
    \ lookup existing symbols
    symlist @ begin dup while
        over over list>val @ node>arg0 @ streq if list>val @ nip exit then
        list>next @
    repeat drop

    strdup Nsymbol make-node1
    dup symlist list-push!
;

: make-quote ( atom -- atom ) Nquote make-node1 ;
: make-qquote ( atom -- atom ) Nqquote make-node1 ;
: make-unquote ( atom -- atom ) Nunquote make-node1 ;

: int? node>type @ Nint = ;
: sym? node>type @ Nsymbol = ;
: sym>name node>arg0 @ ; 

( === Builtin symbols === )
s" true" make-symbol constant Strue
s" def" make-symbol constant Sdef
s" set" make-symbol constant Sset
s" if" make-symbol constant Sif
s" while" make-symbol constant Swhile
s" do" make-symbol constant Sdo
s" lambda" make-symbol constant Slambda
s" macro" make-symbol constant Smacro

( === Parser and Printer === )

: is-blank ( c -- bool )
    case
        bl   of true endof
        '\t' of true endof
        '\n' of true endof
        drop false
    endcase
;

: is-atom-char ( c -- bool )
    case
        '0' '9' rangeof true endof
        'a' 'z' rangeof true endof
        'A' 'Z' rangeof true endof
        '+' of true endof
        '-' of true endof
        '*' of true endof
        '/' of true endof
        '<' of true endof
        '>' of true endof
        '=' of true endof
        '?' of true endof
        '!' of true endof
        '_' of true endof
        ':' of true endof
        '$' of true endof
        '%' of true endof
        '&' of true endof
        '|' of true endof
        '^' of true endof
        '~' of true endof
        '@' of true endof
        drop false
    endcase
;

: print-sexp ( sexp -- )
    dup @ case
    Nint of to-int 10 swap print-int endof
    Nstr of to-str type endof
    Nsymbol of sym>name type endof
    Nquote of '\'' emit node>arg0 @ recurse endof
    Nqquote of '`' emit node>arg0 @ recurse endof
    Nunquote of ',' emit node>arg0 @ recurse endof
    Nnil of drop ." ()" endof
    Ncons of 
        '(' emit
        dup car recurse
        cdr
        begin dup nil <> while 
            bl emit
            dup car recurse cdr
        repeat drop
        ')' emit
    endof
    Nlambda of
        ." (lambda "
        dup node>arg1 @ recurse
        bl emit
        node>arg2 @ recurse
        ')' emit
    endof
    Nmacro of
        ." (macro "
        dup node>arg1 @ recurse
        bl emit
        node>arg2 @ recurse
        ')' emit
    endof
    Nprim of node>arg0 @ recurse endof
        not-reachable
    endcase
;

: parse-error
    ." Parse Error" cr 1 quit
;

create strbuf 1024 allot
: parse-str ( c-addr -- node c-addr )
    strbuf swap ( buf str )
    begin dup c@ '"' <> while
        dup c@ '\\' = if
            1+ dup >r c@ escaped-char dup 0< if ." invalid escaped char" cr 1 quit then
            over c!
            1+ r> 1+
        else
            dup >r c@ over c! 1+ r> 1+
        then
    repeat
    1+ >r 1+ 0 swap c! strbuf strdup make-str r>
;

create tokbuf 1024 allot

: parse-atom ( c-addr -- sexp c-addr )
    dup c@ case
        '"'  of 1+ parse-str endof
        drop tokbuf ( from to )
        begin over c@ is-atom-char while
            over c@ over c!
            1+ swap 1+ swap
        repeat
        0 swap c!
        tokbuf >number if make-int else drop tokbuf make-symbol then
        swap
    endcase
;

: skip-spaces-and-comments ( c-addr -- c-addr )
    begin
        dup c@ is-blank if
            1+
        else dup c@ ';' = if
            begin dup c@ '\n' <> while 1+ repeat
        else
            exit
        then then
    again
;

defer parse-sexp

: parse-sexp-list ( c-addr -- sexp c-addr )
    skip-spaces-and-comments
    dup c@ ')' = if 1+ nil swap exit then
    parse-sexp ( car c-addr )
    recurse    ( car cdr c-addr )
    >r swap make-cons r>
;

:noname ( c-addr -- sexp c-addr )
    skip-spaces-and-comments
    dup c@ case
        '('  of 1+ parse-sexp-list endof
        '\'' of 1+ recurse swap make-quote swap endof
        '`'  of 1+ recurse swap make-qquote swap endof
        ','  of 1+ recurse swap make-unquote swap endof
        drop parse-atom over unless parse-error then
    endcase
; is parse-sexp

( === Eval === )
struct
    cell% field env>sym
    cell% field env>value
    cell% field env>next
end-struct env%

: env-push ( env sym node -- env' )
    env% %allocate
    tuck env>value !
    tuck env>sym !
    tuck env>next !
;

: env-update ( env sym node -- )
    rot begin dup while
        over over env>sym @ = if
            ( node sym env )
            nip env>value ! exit
        then
        env>next @
    repeat
    ." variable " sym>name type ." is not found" cr 1 quit
;

: env-find ( env sym -- node )
    swap begin dup while
        over over env>sym @ = if
            env>value @ nip exit
        then
        env>next @
    repeat
    2drop 0
;

: print-env ( env -- )
    '{' emit
    begin dup while
        dup env>sym @ sym>name type ."  : "
        dup env>value @ print-sexp
        env>next @ dup if ." , " then
    repeat drop
    '}' emit cr
;

variable root-env

\ add primitive function
: add-prim ( c-addr xt -- )
    swap make-symbol tuck make-prim
    root-env @ -rot env-push root-env !
;

s" prim:add"    :noname to-int swap to-int + make-int ; add-prim
s" prim:sub"    :noname to-int swap to-int - make-int ; add-prim
s" prim:mul"    :noname to-int swap to-int * make-int ; add-prim
s" prim:div"    :noname to-int swap to-int / make-int ; add-prim
s" prim:mod"    :noname to-int swap to-int % make-int ; add-prim
s" prim:and"    :noname to-int swap to-int & make-int ; add-prim
s" prim:or"     :noname to-int swap to-int | make-int ; add-prim
s" prim:xor"    :noname to-int swap to-int ^ make-int ; add-prim
s" prim:less"    :noname to-int swap to-int < if Strue else nil then ; add-prim
s" prim:uless"   :noname to-int swap to-int u if Strue else nil then ; add-prim
s" prim:equal"   :noname to-int swap to-int = if Strue else nil then ; add-prim
s" prim:lshift"  :noname to-int swap to-int lshift make-int ; add-prim
s" prim:rshift"  :noname to-int swap to-int rshift make-int ; add-prim
s" prim:arshift" :noname to-int swap to-int arshift make-int ; add-prim
s" prim:cons" :noname make-cons ; add-prim
s" prim:car" :noname car ; add-prim
s" prim:cdr" :noname cdr ; add-prim
s" prim:nil_p" :noname nil = if Strue else nil then ; add-prim
s" prim:print" :noname print-sexp nil ; add-prim

defer eval-sexp
defer eval-cons
defer eval-qquote

:noname ( env sexp -- env sexp )
    dup @ case
    Nint of ( do nothing ) endof
    Nstr of ( do nothing ) endof
    Nsymbol of
        2dup
        ( env sym env sym )
        env-find ?dup unless
            ." undefined variable: " sym>name type cr
            1 quit
        then
        nip
    endof
    Nquote  of node>arg0 @ endof
    Nqquote of node>arg0 @ 0 eval-qquote endof
    Nnil of ( do nothing ) endof
    Ncons of eval-cons endof
    Nlambda of endof
    Nmacro of endof
    Nprim of endof
        not-reachable
    endcase
; is eval-sexp

: eval-list ( env cons -- env cons )
    dup nil = if drop nil else
        over >r
        2dup car eval-sexp nip >r
        ( env cons R: env car' )
        cdr recurse nip r> make-cons r> swap
    then
;

: apply ( env args fn -- env value )
    ( outer-env args fn )
    dup node>arg2 @ >r
    dup node>arg1 @ >r
    node>arg0 @
    ( outer-env args env R: body params )
    swap r> swap
    ( outer-env env params args ; R: body )
    \ bind args to params
    over sym? if
        env-push
    else
        2dup cons-len swap cons-len <> if
            ." incorrect number of arguments: " 
            swap print-sexp
            ."  <-> "
            print-sexp
            cr 1 quit
        then
        
        \ update env
        begin dup nil <> while
            2dup >r >r
            car >r car r> env-push
            r> cdr r> cdr
        repeat 2drop
    then
    r>

    ( outer-env env' body )
    eval-sexp
    ( outer-env env' value )
    nip
;

: flatten-args ( args -- argn-1 ... arg0 )
    dup nil = if drop else dup >r cdr recurse r> car then
;

: call-prim ( env args fn -- env value )
    node>arg1 @ >r ( env args R: xt )
    flatten-args r> ( env argn-1 ... arg0 xt )
    execute
;

:noname ( env sexp -- env sexp )
    dup car case
    Sdef of \ (def sym sexp): define new variable
        dup cons-len 3 <> if ." malformed 'def' expr" cr 1 quit then
        cdr
        dup car dup sym? unless ." malformed 'def' expr" cr 1 quit then
        ( env args sym )
        >r cadr eval-sexp r>
        ( env val sym )
        swap env-push
        nil
    endof
    Sset of \ (set sym sexp): update the variable
        dup cons-len 3 <> if ." malformed 'set' expr" cr 1 quit then
        cdr
        dup car dup sym? unless ." malformed 'set' expr" cr 1 quit then
        ( env args sym )
        >r cadr eval-sexp r>
        ( env val sym )
        2 pick >r env-update r> nil
    endof
    Sif of \ (if condition ifthen ifelse)
        dup cons-len 4 <> if ." malformed 'if' expr" cr 1 quit then
        cdr
        ( env args )
        2dup car eval-sexp nil <> if
            ( env args env' )
            swap cadr eval-sexp
            ( env env' val )
            nip \ returns outer env
        else
            swap caddr eval-sexp
            nip \ returns outer env
        then
    endof
    Swhile of \ (while condition body)
        dup cons-len 3 <> if ." malformed 'while' expr" cr 1 quit then
        ( env args )
        swap >r
        cdr dup car swap cadr
        ( cond body R: env )
        begin r> dup >r 2 pick eval-sexp nil <> while
            over eval-sexp 2drop
        repeat drop
        2drop r> nil
    endof
    Sdo of \ (do e0 e1 ...)
        cdr nil swap ( env nil args )
        begin dup nil <> while
            nip dup >r ( env args R: args )
            car eval-sexp r> cdr
        repeat
        ( env last nil )
        drop
    endof
    Slambda of \ (lambda params body)
        ( env node )
        cdr over >r
        dup cadr swap car r>
        ( env body params env)
        make-lambda
    endof
    Smacro of \ (macro params body)
        ( env node )
        cdr over >r
        dup cadr swap car r>
        ( env body params env)
        make-macro
    endof
        ( env sexp car )
        >r over r> eval-sexp nip
        ( env sexp fn )
        >r cdr r>
        ( env args fn )

        \ If fn is not 'macro', evaluate args before application
        dup node>type @ Nmacro <> if
            >r eval-list r>
        then
            
        dup node>type @ case
            Nlambda of apply endof
            Nmacro of apply eval-sexp endof
            Nprim of call-prim endof
            ( default case )
            drop
            print-sexp ."  is not a function" cr 1 quit
        endcase
    endcase
; is eval-cons

:noname ( env sexp nestlevel - env sexp )
    >r
    dup node>type @ case
    Nint of r> drop endof
    Nstr of r> drop endof
    Nsymbol of r> drop endof
    Nquote of node>arg0 @ r> recurse make-quote endof
    Nqquote of node>arg0 @ r> 1+ recurse make-qquote endof
    Nunquote of
        \ eval arg if level = 0
        r> ?dup unless
            node>arg0 @ eval-sexp
        else
            >r node>arg0 @ r> 1- recurse make-unquote
        then
    endof
    Nnil of r> drop endof
    Ncons of
        tuck ( cons env cons ; R: nest )
        car r> dup >r eval-qquote
        ( cons env' car' ; R: nest )
        -rot swap cdr r>
        ( car' env' cdr nest )
        eval-qquote
        ( car' env' cdr' )
        rot
        ( env' cdr' car' )
        make-cons
    endof
    not-reachable
    endcase
; is eval-qquote

0x100000 constant MAX_PLISP_FILE_SIZE
: eval-file ( env c-str -- env' )
    R/O open-file throw dup >r >r ( env R: file file )
    MAX_PLISP_FILE_SIZE dup allocate tuck ( env mem size mem R: file file )
    swap r> read-file throw ( env mem read-size )
    MAX_PLISP_FILE_SIZE >= if ." too large file" cr 1 quit then
    r> close-file throw

    ( env c-addr )
    swap >r
    begin
        skip-spaces-and-comments
        dup c@ unless ( EOF ) drop r> exit then
        parse-sexp swap
        r> swap eval-sexp drop >r
    again
;

:noname
    root-env @

    s" init.lisp" eval-file
    argc @ 1 <= if ." no input file" cr 1 quit then
    1 arg eval-file

    drop
; execute

0 quit
