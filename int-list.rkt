;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname int-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Question 2A
;;sum-fav: (listof Int) Int -> Int

;;The purpose of this function if to take in a listof Int, loi, and a 
;;integer favourtie. When resturn the sum of all the numbers within
;; the loi which are greater than the int favourite. 

;;Examples 
(check-expect (sum-fav (cons 4 (cons -5 (cons 6 empty))) 4 ) 10)
(check-expect (sum-fav (cons 3.4 (cons 2.3 (cons 4.0 empty))) 3) (+ 4 3.4))

;;Function Definition 
(define (sum-fav loi favourite)
  (cond
    [(empty? loi) 0]
    [(>= (first loi) favourite) 
     (+ (first loi) (sum-fav (rest loi) favourite))]
    [else (sum-fav (rest loi) favourite)]))

;;Tests 
(check-expect (sum-fav (cons 20 (cons 34 (cons 12 (cons 90 empty)))) 50) 90)
(check-expect (sum-fav (cons -7 (cons 8 (cons -9 empty))) 9) 0)
(check-expect (sum-fav (cons 10 (cons 2 (cons 4 (cons 40 empty)))) 9) 50)
(check-expect (sum-fav (cons -23 (cons -10 (cons -3 (cons -45 empty)))) -11) -13)
(check-expect (sum-fav (cons -12 (cons -9 (cons 0 (cons 20 (cons 35 empty))))) 0) 55)

;;Question 2B
;; reciprocate: (listof Int) -> (listof Num) 

;;The Purpose of this function is to take in a listof Int, loi and return
;;a list of numbers which are the reciprocals for the Integers in loi. 
;;If the input is 0 return 'undefined. 

;;Examples 
(check-expect (reciprocate (cons 0 (cons 'undefined (cons 4 (cons 2/3 (cons 1/4 empty))))))
              (cons 'undefined (cons 0 (cons 1/4 (cons 3/2 (cons 4 empty))))))

(check-expect (reciprocate (cons 1.5 (cons 'undefined (cons 0.75 (cons 2/3 empty)))))
              (cons 2/3 (cons 0 (cons 4/3 (cons 1.5 empty)))))

;;Function Definition 
(define (reciprocate loi)
  (cond
    [(empty? loi) empty]
    [(equal? 'undefined (first loi)) (cons 0 (reciprocate (rest loi)))]
    [(= 0 (first loi)) (cons 'undefined (reciprocate (rest loi)))]
    [else (cons (/ 1 (first loi)) (reciprocate (rest loi)))]))

;;Tests 
(check-expect (reciprocate (cons 0.5 (cons 0.8 (cons 0 empty))))
              (cons 2 (cons 5/4 (cons 'undefined empty))))

(check-expect (reciprocate (cons 3/5 (cons -2/3 (cons -4 (cons -1/2 (cons -5/6 empty))))))
              (cons 5/3 (cons -3/2 (cons -1/4 (cons -2 (cons -6/5 empty))))))


;;Question 2C
;;ascending-or-decending? 

;;The Purpose of this function is to take in a list of integers, loi and a Symbol, sym
;;which tells whether the list should be ascending or decending. The function returns true, 
;; the the symbol matches that state of the list, else it returns false

;;Examples 
(check-expect (ascending-or-descending? 
               (cons 3 (cons 5 (cons 6 (cons 8 (cons 10 empty))))) 'ascending)
              true)

(check-expect (ascending-or-descending? 
               (cons -3 (cons -5 (cons -6 (cons -8 (cons -10 empty))))) 'ascending)
              false)

;;Function 
(define (ascending-or-descending? loi sym)
  (cond
    [(empty? loi) true]
    [(symbol=? 'ascending sym) (is-asc? (rest loi) (first loi))]
    [(symbol=? 'descending sym) (is-des? (rest loi) (first loi))]))

;;Tests 

(check-expect (ascending-or-descending? 
               (cons 3 (cons 10 (cons 6 (cons 8 (cons 10 empty))))) 'ascending)
              false)

(check-expect (ascending-or-descending? 
               (cons 3 (cons 3 (cons 4 (cons 4 (cons 5 empty))))) 'ascending)
              false)

(check-expect (ascending-or-descending? 
               (cons 13 (cons 3 (cons 0.4 (cons 0.04 (cons 0.005 empty))))) 'descending)
              true)

(check-expect (ascending-or-descending? 
               (cons 20 (cons 13 (cons 10 (cons 8 (cons 5 empty))))) 'descending)
              true)

(check-expect (ascending-or-descending? 
               (cons 20 (cons 33 (cons 40 (cons 58 (cons 95 empty))))) 'descending)
              false)

(check-expect (ascending-or-descending? 
               (cons 20 (cons 33 (cons 40 (cons 58 (cons 95 empty))))) 'ascending)
              true)

(check-expect (ascending-or-descending? empty 'ascending) true)

(check-expect (ascending-or-descending? empty 'descending) true)

;;Helper 1 
;;The purpose of this funcion is to take in a list, loi and a number, firstnum, corresponding
;;to the first number of this given list. 
;; It returns true is the list is in ascending order, else return false. 

;;Examples 

;;Function Definition 
(check-expect (

(define (is-asc? loi firstnum)
 (cond
   [(empty? loi) true]
   [(< firstnum (first loi)) (is-asc? (rest loi) (first loi))]
   [else false]))

;;Tests 


;;Helper 2 
;;The purpose of this funcion is to take in a list, loi and a number, firstnum, corresponding
;;to the first number of this given list. 
;; It returns true is the list is in descending order, else return false. 
(define (is-des? loi firstnum)
 (cond
   [(empty? loi) true]
   [(> firstnum (first loi)) (is-des? (rest loi) (first loi))]
   [else false]))

