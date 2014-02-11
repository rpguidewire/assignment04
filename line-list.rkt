;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname line-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Question 3A

(define-struct line (slope intercept))
;;A Line = (make-line Num Num)

(define line1 (make-line 1 0))
(define line2 (make-line 0 3))
(define line3 (make-line -1/2 -2))
(define line4 (make-line 'undefined -4))
(define line-list (cons line1 (cons line2 (cons line3 (cons line4 empty)))))

;;Template 
;
;(define (my-listofline-fn lstofLine)
;  (cond 
;    [(empty? lstofLine)....]
;    [else (....(my-line-fn (first lstofLine))
;               ..(my-listofline-fn (rest lstofLine))..)]))
;
;(define (my-line-fn myline)
;  (...(line-slope myline)...
;   ...(line-intercept myline)...))

;;Question 3B
;;negate-slope: (listof Line) -> (listof Line)

;;The purpose of this function is to take in a listof Line, listoflines 
;; and negate the slopes of all the lines. 
;;Examples 

(define (negate-slope listoflines)
  (cond
    [(empty? listoflines) empty]
    [(equal? 'undefined (line-slope (first listoflines)))
             (cons (make-line 'undefined (line-intercept (first listoflines))) 
                   (negate-slope (rest listoflines)))]
;    [(equal? 0 (line-slope (first listoflines))) 
;     (cons (first listoflines) (negate-slope (rest listoflines)))]
    [else (cons (make-line (* -1 (line-slope (first  listoflines))) 
                           (line-intercept (first listoflines)))
                (negate-slope (rest listoflines)))]))

;;3C
;;The purpose of this function is to take in a listof Lines, listofline
;; and return a listof Lines which has the lines that have a postive 
;;slope or have a positive y or x-intercept

(define (positive-line lol)
(cond
  [(empty? lol) empty]
  
  [(and (equal? (line-slope (first lol)) 'undefined)
        (> (line-intercept (first lol)) 0))
   (cons (first lol) (positive-line (rest lol)))]
  
  [(and (equal? (line-slope (first lol)) 'undefined)
        (<= (line-intercept (first lol)) 0))
     (positive-line (rest lol))]
  
  [(or (> (line-slope (first lol)) 0)
       (> (line-intercept (first lol)) 0))
   (cons (first lol) (positive-line (rest lol)))]
  
  [else (positive-line (rest lol))]))

(check-expect (positive-line line-list) 
              (list (make-line 1 0) (make-line 0 3)))


;;Question 3D
;;through-point: (listof Line) Posn -> (listof Line)

;; The purpose of this function is to take in a listof Lines, lol, and a posn, posn1 and return a listof Lines
;; which pass through the given posn. 

;;Function 
(define (through-point lol posn1)
   (cond 
     [(empty? lol) empty]
     
     [(or 
       (equal? (line-slope (first lol)) 'undefined)
       (= (line-slope (first lol)) 0))
      (cons (first lol) (through-point (rest lol) posn1))]
     
     [(= (posn-y posn1) (+ (* (line-slope (first lol)) (posn-x posn1))
                           (line-intercept (first lol))))
                           ;(- (posn-y posn1) (* (line-slope (first lol)) (posn-x posn1)))))
      (cons (first lol) (through-point (rest lol) posn1))]
     
     [else (through-point (rest lol) posn1)]))
                        
;;Quetion 3E

(define (parallel-non-intersect-lines linelist)
  (cond 
    [(empty? (rest (rest linelist))) (cons (= (line-slope (first linelist))
                                              (line-slope (first (rest linelist)))) empty)]
    [(= (line-slope (first linelist))
        (line-slope (first (rest linelist)))) (cons true (parallel-non-intersect-lines
                                                          (rest (rest linelist))))]
    [else (cons false (parallel-non-intersect-lines (rest (rest linelist))))]))

;Tests 

(check-expect (parallel-non-intersect-lines (cons (make-line 3 2)
                                              (cons (make-line 3 5)
                                                (cons (make-line 4 7)
                                                  (cons (make-line 4 -4)
                                                   (cons (make-line 5 -2)
                                                      (cons (make-line 5 -4) empty))))))) (list true true true))
(check-expect (parallel-non-intersect-lines (cons (make-line 3 4)
                                              (cons (make-line 3 4)
                                                (cons (make-line 5 4)
                                                  (cons (make-line 4 4)
                                                   (cons (make-line 5 4)
                                                      (cons (make-line 5 4) empty))))))) (list true false true))

(check-expect (parallel-non-intersect-lines (cons (make-line 3 4)
                                              (cons (make-line 1/3 4)
                                                (cons (make-line 5 4)
                                                  (cons (make-line 4 4)
                                                   (cons (make-line 5 4)
                                                      (cons (make-line 5 4) empty))))))) (list false false true))

(check-expect (parallel-non-intersect-lines (cons (make-line 1/3 -10)
                                              (cons (make-line 2/3 -4)
                                                (cons (make-line 3/5 3)
                                                  (cons (make-line 4/3 -8)
                                                   (cons (make-line 5/4 -3)
                                                      (cons (make-line 5/3 -4) empty))))))) (list false false false))

(check-expect (parallel-non-intersect-lines (cons (make-line 1/3 -1)
                                                  (cons (make-line 4/3 -4)
                                                   (cons (make-line 5/4 1)
                                                      (cons (make-line 5/3 3) empty))))) (list false false))

(check-expect (parallel-non-intersect-lines (cons (make-line 1/3 4)
                                                  (cons (make-line 1/3 -8)
                                                   (cons (make-line 5/4 -10)
                                                      (cons (make-line 5/4 4) empty))))) (list true true))

(check-expect (parallel-non-intersect-lines (cons (make-line 1/3 -0.4)
                                                  (cons (make-line 2/3 5)
                                                   (cons (make-line 5/2 -4)
                                                      (cons (make-line 5/3 8) empty))))) (list false false))

(check-expect (parallel-non-intersect-lines (cons (make-line 3 4)
                                              (cons (make-line 3 -1.4) empty))) (list true))

(check-expect (parallel-non-intersect-lines (cons (make-line 3 4)
                                              (cons (make-line 4 1.4) empty))) (list false))










       