# Program-2
For this assignment you'll be writing a parser for a simple grammar. As with the last assignment, it's as much about how you use the AI tools as it is code correctness. There are two basic approaches: You may write a recursive descent top-down parser, or use the parser generator tool brag to produce a parser. The two approaches are about equal in difficulty; one has you grappling with code, the other with documentation and dealing with the syntax-object that brag returns.   Either way, your code should have a function called parse, which takes one parameter: the name of the source code to be processed.   (parse "input.txt")   Your program should read in the specified source file and parse it according to the assigned grammar. Output will be one of two things:   On a successful parse, the word "Accept",  indicating the program is syntactically correct, followed by the parse tree itself; or   A message "Syntax error on line XX," where XX is the physical line of the file. (Note that this grammar allows more than one statement, one logical line, on the same physical line.) 


The grammar: 

program -> stmt-list EOF 
stmt-list -> stmt stmt-list | epsilon
stmt -> if-stmt | while-stmt | assign-stmt | read-stmt | print-stmt | compound-stmt
compound-stmt -> stmt {; stmt}*
if-stmt -> if expr comp-op expr then begin stmt-list end {else begin stmt-list end}
while-stmt -> while expr comp-op expr begin stmt-list end 
assign-stmt -> id := expr 
read-stmt -> read id 
print-stmt -> print expr 
expr -> term + expr | term - expr | term comp-op term | term 
term -> factor * term | factor / term | factor 
factor -> id | num | (expr)
num ->  sign nonzero | sign nonzero digit* | sign nonzero digit* . digit digit*
sign -> + | - | epsilon
id ->  [alpha, followed by 0 or more alphanumerics, hyphens, or underscores in any combination] 
comp-op -> = | > | < | >= | <= | <>
nonzero -> 1|2|3|4|5|6|7|8|9
digit -> nonzero | 0 
comment -> /*  [any text, including multi-line, except close-comment symbol] */ 
 

EOF = end of file
Expressions cannot cross a line break. Statements can cross a line break.
Whitespace is not significant except to separate items, or line breaks to mark end of expressions.
Multiple statements (logical lines) can appear on the same physical line, separated by semicolons. The semicolon is not used as a general end-of-statement marker, but indicates that another statement follows on the same line.
Integers have an optional sign. If more than 1 digit, the first digit is nonzero. There is no maximum length of an integer; we'll let the implementation deal with the large-number problem.
Floating-point numbers are supported. If a decimal point is present, there must be at least one digit after the decimal point.
Scientific notation is not supported.
Hexadecimal, octal, or binary notation for integers is not supported.
Comments are not nested. The first closing symbol */ closes all comments; nested open- comment symbols are considered part of the comment: 
   /* /* /*  This is a comment that ends with one closing symbol. */ 
As indicated above,  /* Comments can extend 
   across more than one 
   physical line. */  
