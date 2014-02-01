;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname int-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;sum-fav: (listof Int) Int -> Int

;;The purpose of this function if to take in a listof Int, loi, and a 
;;integer favourtie. When resturn the sum of all the numbers within
;; the loi which are greater than the int favourite. 

(define (sum-fav loi favourite)
  (cond
    [(empty? loi) 0]
    [(> (first loi) favourite) 
     (+ (first loi) (sum-fav (rest loi) favourite))]
    [else (sum-fav (rest loi) favourite)]))


;; reciprocate: (listof Int) -> (listof Num) 

;;The Purpose of this function is to take in a listof Int, loi and return
;;a list of numbers which are the reciprocals for the Integers in loi. 
;;If the input is 0 return 'undefined. 

(define (reciprocate loi)
  (cond
    [(empty? loi) empty]
    [(= 0 (first loi)) (cons 'undefined (reciprocate (rest loi)))]
    [else (cons (/ 1 (first loi)) (reciprocate (rest loi)))]))

;;ascending-or-decending? 

(define (ascending-or-descending? loi sym)
  (cond
    [(empty? loi) true]
    [(symbol=? 'ascending sym) (is-asc? (rest loi) (first loi))]
    [(symbol=? 'descending sym) (is-des? (rest loi) (first loi))]))

(define (is-asc? loi firstnum)
 (cond
   [(empty? loi) true]
   [(< firstnum (first loi)) (is-asc? (rest loi) (first loi))]
   [else false]))

(define (is-des? loi firstnum)
 (cond
   [(empty? loi) true]
   [(> firstnum (first loi)) (is-des? (rest loi) (first loi))]
   [else false]))