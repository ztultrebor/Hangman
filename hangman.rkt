;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hangman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


; A Game of Hangman


; =========================
; data definitions


(define-struct letter [char guessed])
; A Letter is a [1String Boolean]
; one per each letter of a word, as well as a bool that
; turns true when the player guesses it
#;
(define (fn-on-letter l)
  (... (fn-on-1string (letter-char l)) ... (fn-on-bool (letter-guessed l))))


; A Word is a [ListOf Letter]
; how the game represents the secret word
#;
(define (fn-on-word w)
  (cons (fn-on-letter (first w)) (fn-on-word (rest w))))


(define-struct game [word condemned])
; A Game is a [Word N]
; contains a representation of the secret word,
; as well as the state of the condemned man
#;
(define (fn-on-game g)
  (... (fn-on-word (game-word g)) ... (fn-on-N (game-condemned g))))



; =========================
; constants

(define DICTIONARY '("eggman", "walrus"))
(define HEIGHT 750)
(define CANVAS (empty-scene (quotient (* 1618 HEIGHT) 1000) HEIGHT "white"))
(define TEXTSIZE 64)
(define CHARCARD
  (rectangle TEXTSIZE (quotient (* 3 TEXTSIZE) 4) "solid" "white"))
(define NULLSPACE (rectangle 0 0 "solid" "white"))



; =========================
; functions

(define (play hangman)
  ; WarObjects -> WarObjects
  ; run the pocket universe
  (big-bang hangman
    #;[on-tick xxxxxx]
    #;[on-key xxxxxxx]
    [to-draw render]
    #;[stop-when xxxxxxx xxxxxxx]))


(define (render hangman)
  (overlay
   (foldr beside NULLSPACE
         (map
          (lambda (ltr) (overlay
                         (text (letter-char ltr) TEXTSIZE "black")
                         CHARCARD))
          (game-word hangman)))
  CANVAS))
  

(define (get-random-word dictionary)
  "narwallawner")


; ==========================
; actions

(define WORD (map (lambda (ch) (make-letter ch #t))
                  (explode (get-random-word DICTIONARY))))

(define GAME (make-game WORD 5))

(play GAME)