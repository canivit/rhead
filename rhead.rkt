#lang racket

;; stdin is an InputPort that represents the standard input of this program
(define stdin (current-input-port))

;; stout is an OutputPort that represents the standard output of this program
(define stdout (current-output-port))

;; Nat is an integer that is greater than or equal to zero
;; Examples:
(define NAT-1 0)
(define NAT-2 1)
(define NAT-3 2)

;; [Maybe Nat] is one of
;; - Nat
;; - #f
;; It represents the result of command line arguments parsing
;; It is a Nat if the command line arguments are valid
;; It is a #f if the command line arguments are not valid
;; Examples:
(define MAYBE-NAT-1 0)
(define MAYBE-NAT-2 1)
(define MAYBE-NAT-3 2)
(define MAYBE-NAT-4 #f)

;; main : [Vector-of String] InputPort OutputPort -> Void
;; Runs the program from the given command line arguments and using the given input and output port
(define (main args input output)
  ;; args is a [Vector-of String]
  ;; it repesents the command line arguments passed into this program 
  (define count (parse-args args))
  (cond [(number? count) (print-lines (take-n (read-all-lines input) count) output)]
        [(false? count) (println "error" output)]))

;; parse-args : [Vector-of String] -> [Maybe Nat]
;; Parses the given command line arguments and produces a maybe nat
(define (parse-args args)
  (if (= (vector-length args) 1)
      (parse-single-arg (vector-ref args 0))
      #f))

;; parse-single-arg : String -> [Maybe Nat]
;; Parses the given singe command line argument and produces a maybe nat
(define (parse-single-arg arg)
  (define (dash-check arg)
    (if (char=? (string-ref arg 0) #\-)
        (substring arg 1)
        #f))
  
  (define (number-check arg)
    (if (string? arg)
        (string->number arg)
        #f))
  
  (define (nat-check arg)
    (if (and (number? arg) (integer? arg) (>= arg 0))
        arg
        #f))  
  (nat-check (number-check (dash-check arg))))

;; read-all-lines : InputPort -> [List-of String]
;; Reads all lines from the given input port and produces a list
;; where each element represents a single line
(define (read-all-lines input)  
  (for/list ([line (in-lines input 'any)])
    line))

;; print-lines : [List-of String] OutputPort -> Void
;; Prints each element in the given list as a seperate line to the given output port
(define (print-lines lines output)
  (for ([l lines])
    (println l output)))

;; take-n : {X} [List-of X] Nat -> [Lit-of X]
;; Produces a new list by taking the first n elements of the given list
;; If n is larger then the length of the list, produces the entire list
(define (take-n lox n)
  (define len (length lox))
  (if (> n len) lox (take lox n)))

(main (current-command-line-arguments) stdin stdout)