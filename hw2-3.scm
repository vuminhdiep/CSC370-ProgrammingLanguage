;1: Ambiguous for example: (01010)1 vs 0(10101) can both valid
<binary-string-ambi>::= 0 | 1 | 0<binary-string-ambi> | 1<binary-string-ambi>
;2
<binary-string>::= <binary-string>0 | 0<binary-string> | <binary-string>1 | 1<binary-string> | 0 | 1
;3: Palindromes should have first half of the string matched to reverse of second half of the string.
;;If string has odd length, we have 0 | 1. If it is even, we have " "
<binary-str-palindrome> ::= 0<binary-str-palindrome>0 | 1<binary-str-palindrome>1 | 0 | 1 | " "
;4
<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<letter> ::= 'A' | 'B' | 'C' | 'D' | 'E' | 'F' 
<signed> ::= '+' | '-'
<hex> ::= <signed> <digit> | <letter> | <signed> <hex> <digit> | <hex> <letter>
;5
<function>::= def <ID> (): | def <ID> (<ID>): | def <ID> (<ID>=<default-value>):
<default-value>::= <number> | <string>
