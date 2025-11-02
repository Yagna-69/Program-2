#lang racket

;; Parser for Program 2
;; Implements a recursive descent parser for the specified grammar

(provide parse)

;; Token structure: (type value line-number)
(struct token (type value line) #:transparent)

;; Parser state
(define current-tokens '())
(define current-pos 0)
(define parse-error-line #f)

;; Get current token
(define (peek-token)
  (if (< current-pos (length current-tokens))
      (list-ref current-tokens current-pos)
      (token 'EOF 'EOF (if (null? current-tokens) 1 (token-line (last current-tokens))))))

;; Advance to next token
(define (next-token!)
  (set! current-pos (+ current-pos 1)))

;; Check if current token matches expected type
(define (match-token? type)
  (let ([tok (peek-token)])
    (eq? (token-type tok) type)))

;; Expect a specific token type
(define (expect-token type)
  (let ([tok (peek-token)])
    (if (eq? (token-type tok) type)
        (begin
          (next-token!)
          tok)
        (begin
          (set! parse-error-line (token-line tok))
          (error 'parse "Expected ~a but got ~a on line ~a"
                 type (token-type tok) (token-line tok))))))

;; Tokenizer/Lexer

;; Remove comments from source
(define (remove-comments str)
  (let loop ([chars (string->list str)]
             [result '()]
             [in-comment? #f]
             [line 1]
             [lines '((1 . ""))])
    (cond
      [(null? chars)
       (if in-comment?
           ;; Unclosed comment - return empty
           ""
           (list->string (reverse result)))]
      [(and in-comment? (>= (length chars) 2)
            (char=? (car chars) #\*)
            (char=? (cadr chars) #\/))
       ;; Close comment
       (loop (cddr chars) (cons #\space result) #f line lines)]
      [in-comment?
       ;; Inside comment, preserve newlines
       (if (char=? (car chars) #\newline)
           (loop (cdr chars) (cons #\newline result) #t (+ line 1) lines)
           (loop (cdr chars) (cons #\space result) #t line lines))]
      [(and (>= (length chars) 2)
            (char=? (car chars) #\/)
            (char=? (cadr chars) #\*))
       ;; Open comment
       (loop (cddr chars) (cons #\space (cons #\space result)) #t line lines)]
      [(char=? (car chars) #\newline)
       (loop (cdr chars) (cons #\newline result) #f (+ line 1) lines)]
      [else
       (loop (cdr chars) (cons (car chars) result) #f line lines)])))

;; Check if previous token can be followed by a signed number
(define (can-have-signed-number? last-tok)
  (or (not last-tok)
      (memq (token-type last-tok)
            '(ASSIGN PLUS MINUS MULT DIV LPAREN EQ GT LT GTE LTE NEQ
              IF WHILE READ PRINT THEN BEGIN NEWLINE SEMICOLON))))

;; Tokenize input
(define (tokenize str)
  (let ([cleaned (remove-comments str)])
    (let loop ([chars (string->list cleaned)]
               [result '()]
               [line 1]
               [col 1]
               [last-token #f])
      (cond
        [(null? chars) (reverse result)]

        ;; Skip whitespace (except newlines)
        [(and (char-whitespace? (car chars)) (not (char=? (car chars) #\newline)))
         (loop (cdr chars) result line (+ col 1) last-token)]

        ;; Newline
        [(char=? (car chars) #\newline)
         (let ([tok (token 'NEWLINE "\n" line)])
           (loop (cdr chars) (cons tok result) (+ line 1) 1 tok))]

        ;; Numbers (including signed numbers in appropriate context)
        [(or (char-numeric? (car chars))
             (and (or (char=? (car chars) #\+) (char=? (car chars) #\-))
                  (not (null? (cdr chars)))
                  (char-numeric? (cadr chars))
                  (can-have-signed-number? last-token)))
         (let-values ([(num rest) (read-number chars line)])
           (loop rest (cons num result) line (+ col (string-length (token-value num))) num))]

        ;; Identifiers and keywords
        [(char-alphabetic? (car chars))
         (let-values ([(id rest) (read-identifier chars line)])
           (loop rest (cons id result) line (+ col (string-length (token-value id))) id))]

        ;; Operators and punctuation
        [(char=? (car chars) #\:)
         (if (and (not (null? (cdr chars))) (char=? (cadr chars) #\=))
             (let ([tok (token 'ASSIGN ":=" line)])
               (loop (cddr chars) (cons tok result) line (+ col 2) tok))
             (loop (cdr chars) result line (+ col 1) last-token))]

        [(char=? (car chars) #\>)
         (if (and (not (null? (cdr chars))) (char=? (cadr chars) #\=))
             (let ([tok (token 'GTE ">=" line)])
               (loop (cddr chars) (cons tok result) line (+ col 2) tok))
             (let ([tok (token 'GT ">" line)])
               (loop (cdr chars) (cons tok result) line (+ col 1) tok)))]

        [(char=? (car chars) #\<)
         (cond
           [(and (not (null? (cdr chars))) (char=? (cadr chars) #\=))
            (let ([tok (token 'LTE "<=" line)])
              (loop (cddr chars) (cons tok result) line (+ col 2) tok))]
           [(and (not (null? (cdr chars))) (char=? (cadr chars) #\>))
            (let ([tok (token 'NEQ "<>" line)])
              (loop (cddr chars) (cons tok result) line (+ col 2) tok))]
           [else
            (let ([tok (token 'LT "<" line)])
              (loop (cdr chars) (cons tok result) line (+ col 1) tok))])]

        [(char=? (car chars) #\=)
         (let ([tok (token 'EQ "=" line)])
           (loop (cdr chars) (cons tok result) line (+ col 1) tok))]

        [(char=? (car chars) #\+)
         (let ([tok (token 'PLUS "+" line)])
           (loop (cdr chars) (cons tok result) line (+ col 1) tok))]

        [(char=? (car chars) #\-)
         (let ([tok (token 'MINUS "-" line)])
           (loop (cdr chars) (cons tok result) line (+ col 1) tok))]

        [(char=? (car chars) #\*)
         (let ([tok (token 'MULT "*" line)])
           (loop (cdr chars) (cons tok result) line (+ col 1) tok))]

        [(char=? (car chars) #\/)
         (let ([tok (token 'DIV "/" line)])
           (loop (cdr chars) (cons tok result) line (+ col 1) tok))]

        [(char=? (car chars) #\()
         (let ([tok (token 'LPAREN "(" line)])
           (loop (cdr chars) (cons tok result) line (+ col 1) tok))]

        [(char=? (car chars) #\))
         (let ([tok (token 'RPAREN ")" line)])
           (loop (cdr chars) (cons tok result) line (+ col 1) tok))]

        [(char=? (car chars) #\;)
         (let ([tok (token 'SEMICOLON ";" line)])
           (loop (cdr chars) (cons tok result) line (+ col 1) tok))]

        [else
         (loop (cdr chars) result line (+ col 1) last-token)]))))

;; Read a number from character list
(define (read-number chars line)
  (let loop ([rest chars]
             [num-chars '()])
    (cond
      [(null? rest)
       (values (token 'NUM (list->string (reverse num-chars)) line) rest)]
      [(or (char-numeric? (car rest))
           (char=? (car rest) #\.)
           (and (null? num-chars)
                (or (char=? (car rest) #\+) (char=? (car rest) #\-))))
       (loop (cdr rest) (cons (car rest) num-chars))]
      [else
       (values (token 'NUM (list->string (reverse num-chars)) line) rest)])))

;; Read an identifier from character list
(define (read-identifier chars line)
  (let loop ([rest chars]
             [id-chars '()])
    (cond
      [(null? rest)
       (let ([id-str (list->string (reverse id-chars))])
         (values (make-keyword-or-id id-str line) rest))]
      [(or (char-alphabetic? (car rest))
           (char-numeric? (car rest))
           (char=? (car rest) #\-)
           (char=? (car rest) #\_))
       (loop (cdr rest) (cons (car rest) id-chars))]
      [else
       (let ([id-str (list->string (reverse id-chars))])
         (values (make-keyword-or-id id-str line) rest))])))

;; Create keyword or identifier token
(define (make-keyword-or-id str line)
  (case (string->symbol str)
    [(if) (token 'IF str line)]
    [(then) (token 'THEN str line)]
    [(else) (token 'ELSE str line)]
    [(while) (token 'WHILE str line)]
    [(begin) (token 'BEGIN str line)]
    [(end) (token 'END str line)]
    [(read) (token 'READ str line)]
    [(print) (token 'PRINT str line)]
    [else (token 'ID str line)]))

;; Skip newlines
(define (skip-newlines!)
  (when (match-token? 'NEWLINE)
    (next-token!)
    (skip-newlines!)))

;; Grammar parsing functions

;; program -> stmt-list EOF
(define (parse-program)
  (skip-newlines!)
  (let ([stmts (parse-stmt-list)])
    (skip-newlines!)
    (expect-token 'EOF)
    (list 'program stmts)))

;; stmt-list -> stmt stmt-list | epsilon
(define (parse-stmt-list)
  (skip-newlines!)
  (if (or (match-token? 'EOF) (match-token? 'END))
      '(stmt-list)
      (let ([stmt (parse-stmt)])
        (skip-newlines!)
        (cons 'stmt-list (cons stmt (cdr (parse-stmt-list)))))))

;; stmt -> if-stmt | while-stmt | assign-stmt | read-stmt | print-stmt
(define (parse-stmt)
  (skip-newlines!)
  (cond
    [(match-token? 'IF) (parse-if-stmt)]
    [(match-token? 'WHILE) (parse-while-stmt)]
    [(match-token? 'READ) (parse-read-stmt)]
    [(match-token? 'PRINT) (parse-print-stmt)]
    [(match-token? 'ID) (parse-assign-stmt)]
    [(match-token? 'SEMICOLON)
     ;; Skip semicolon and parse next statement
     (next-token!)
     (parse-stmt)]
    [(match-token? 'NEWLINE)
     ;; Skip newline and continue
     (next-token!)
     (parse-stmt)]
    [else
     (let ([tok (peek-token)])
       (set! parse-error-line (token-line tok))
       (error 'parse "Unexpected token ~a on line ~a" (token-type tok) (token-line tok)))]))

;; if-stmt -> if expr comp-op expr then begin stmt-list end [else begin stmt-list end]
(define (parse-if-stmt)
  (expect-token 'IF)
  (let* ([expr1 (parse-expr)]
         [_ (skip-newlines!)]
         [comp-op (parse-comp-op)]
         [_ (skip-newlines!)]
         [expr2 (parse-expr)])
    (skip-newlines!)
    (expect-token 'THEN)
    (skip-newlines!)
    (expect-token 'BEGIN)
    (let ([then-stmts (parse-stmt-list)])
      (skip-newlines!)
      (expect-token 'END)
      (skip-newlines!)
      (if (match-token? 'ELSE)
          (begin
            (next-token!)
            (skip-newlines!)
            (expect-token 'BEGIN)
            (let ([else-stmts (parse-stmt-list)])
              (skip-newlines!)
              (expect-token 'END)
              (list 'if-stmt expr1 comp-op expr2 then-stmts else-stmts)))
          (list 'if-stmt expr1 comp-op expr2 then-stmts)))))

;; while-stmt -> while expr comp-op expr begin stmt-list end
(define (parse-while-stmt)
  (expect-token 'WHILE)
  (let* ([expr1 (parse-expr)]
         [_ (skip-newlines!)]
         [comp-op (parse-comp-op)]
         [_ (skip-newlines!)]
         [expr2 (parse-expr)])
    (skip-newlines!)
    (expect-token 'BEGIN)
    (let ([stmts (parse-stmt-list)])
      (skip-newlines!)
      (expect-token 'END)
      (list 'while-stmt expr1 comp-op expr2 stmts))))

;; assign-stmt -> id := expr
(define (parse-assign-stmt)
  (let ([id (expect-token 'ID)])
    (expect-token 'ASSIGN)
    (let ([expr (parse-expr)])
      (list 'assign-stmt (token-value id) expr))))

;; read-stmt -> read id
(define (parse-read-stmt)
  (expect-token 'READ)
  (let ([id (expect-token 'ID)])
    (list 'read-stmt (token-value id))))

;; print-stmt -> print expr
(define (parse-print-stmt)
  (expect-token 'PRINT)
  (let ([expr (parse-expr)])
    (list 'print-stmt expr)))

;; Check for newline in expression (error if found)
(define (check-no-newline!)
  (when (match-token? 'NEWLINE)
    (let ([tok (peek-token)])
      (set! parse-error-line (token-line tok))
      (error 'parse "Expression cannot cross line break on line ~a" (token-line tok)))))

;; expr -> term + expr | term - expr | term
(define (parse-expr)
  (let ([t (parse-term)])
    (check-no-newline!)
    (cond
      [(match-token? 'PLUS)
       (next-token!)
       (check-no-newline!)
       (let ([e (parse-expr)])
         (list 'expr t '+ e))]
      [(match-token? 'MINUS)
       (next-token!)
       (check-no-newline!)
       (let ([e (parse-expr)])
         (list 'expr t '- e))]
      [else
       (list 'expr t)])))

;; term -> factor * term | factor / term | factor
(define (parse-term)
  (let ([f (parse-factor)])
    (check-no-newline!)
    (cond
      [(match-token? 'MULT)
       (next-token!)
       (check-no-newline!)
       (let ([t (parse-term)])
         (list 'term f '* t))]
      [(match-token? 'DIV)
       (next-token!)
       (check-no-newline!)
       (let ([t (parse-term)])
         (list 'term f '/ t))]
      [else
       (list 'term f)])))

;; factor -> id | num | (expr)
(define (parse-factor)
  (check-no-newline!)
  (cond
    [(match-token? 'ID)
     (let ([id (expect-token 'ID)])
       (list 'factor 'id (token-value id)))]
    [(match-token? 'NUM)
     (let ([num (expect-token 'NUM)])
       (list 'factor 'num (token-value num)))]
    [(match-token? 'LPAREN)
     (next-token!)
     (check-no-newline!)
     (let ([expr (parse-expr)])
       (check-no-newline!)
       (expect-token 'RPAREN)
       (list 'factor expr))]
    [else
     (let ([tok (peek-token)])
       (set! parse-error-line (token-line tok))
       (error 'parse "Expected factor on line ~a" (token-line tok)))]))

;; comp-op -> = | > | < | >= | <= | <>
(define (parse-comp-op)
  (let ([tok (peek-token)])
    (cond
      [(match-token? 'EQ) (next-token!) '=]
      [(match-token? 'GT) (next-token!) '>]
      [(match-token? 'LT) (next-token!) '<]
      [(match-token? 'GTE) (next-token!) '>=]
      [(match-token? 'LTE) (next-token!) '<=]
      [(match-token? 'NEQ) (next-token!) '<>]
      [else
       (set! parse-error-line (token-line tok))
       (error 'parse "Expected comparison operator on line ~a" (token-line tok))])))

;; Main parse function
(define (parse filename)
  (set! current-pos 0)
  (set! parse-error-line #f)

  ;; Read file
  (let ([contents (file->string filename)])
    ;; Tokenize
    (set! current-tokens (tokenize contents))

    ;; Try to parse
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (if parse-error-line
                           (printf "Syntax error on line ~a\n" parse-error-line)
                           (printf "Syntax error\n")))])
      (let ([tree (parse-program)])
        (printf "Accept\n")
        (pretty-print tree)))))
