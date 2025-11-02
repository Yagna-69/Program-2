#lang racket

;; Test file for the parser
;; This file can be used to test the parser in DrRacket

(require "parser.rkt")

;; Test with correct files
(displayln "Testing Correct01.txt:")
(parse "Correct01.txt")

(displayln "\nTesting Correct02.txt:")
(parse "Correct02.txt")

(displayln "\nTesting Correct03.txt:")
(parse "Correct03.txt")

(displayln "\nTesting Correct04.txt:")
(parse "Correct04.txt")

;; Test with error files
(displayln "\nTesting Mismatched-parens.txt:")
(parse "Mismatched-parens.txt")

(displayln "\nTesting Mismatched-end.txt:")
(parse "Mismatched-end.txt")

(displayln "\nTesting bad-comment-1.txt:")
(parse "bad-comment-1.txt")

(displayln "\nTesting bad-comment-2.txt:")
(parse "bad-comment-2.txt")
