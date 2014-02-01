;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname line-list-rkt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Question 3A

(define-struct line (slope intercept))
;;A Line = (make-line Num Num)

;;Template 

(define (my-listofline-fn lstofLine)
  (cond 
    [(empty? lstofLine)....]
    [else (....(my-line-fn (first lstofLine))
               ..(my-listofline-fn (rest lstofLine))..)]))

(define (my-line-fn myline)
  (...(line-slope myline)...
   ...(line-intercept myline)...))

;;Question 3B
;;negate-slope: (listof Line) -> (listof Line)

;;The purpose of this function is to take in a listof Line, listoflines 
;; and negate the slopes of all the lines. 

(define (negate-slope listoflines)
  (cond
    [(empty? listoflines) empty]
    [(= 0 (line-slope (first listoflines))) 
     (cons (first listoflines) (negate-slope (rest listoflines)))]
    [else (cons (- (line-slope (first listoflines)))
                (negate-slope (rest listoflines)))]))

;;3C
;;The purpose of this function is to take in a listof Lines, listofline
;; and return a listof Lines which has the lines that have a postive 
;;slope or have a positive y or x-intercept

(define (positive-line listofline)
(cond
  [(empty? listoslines) empty]
  [(and (equal? (line-slope (first listofline)) 'undefined)
        (>= (line-intercept (first listofline)) 0))
   (cons (first listofline) (positive-line (rest listofline)))]
  [(or (>= (line-slope (first listofline)) 0)
       (>= (line-intercept (first listofline)) 0))
   (cons (first listofline) (positive-line (rest listofline)))]
  [else (positive-line (rest listofline))]))

















       