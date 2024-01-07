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
  ; Game -> Game
  ; run the pocket universe
  (big-bang hangman
    #;[on-tick xxxxxx]
    [on-key guess]
    [to-draw render]
    [stop-when end? game-over]))


(define (render hangman)
  ; Game -> Img
  ; render the state of the game
  (overlay
   (render-word (game-word hangman))
   CANVAS))


(define (guess hangman ke)
  ; Game KeyEvent -> Game
  ; determines a proper course of actions on a keyboard strike
  (cond
    [(key=? " " ke) hangman]
    [(key=? "\b" ke) hangman]
    [(key=? "left" ke) hangman]
    [(key=? "right" ke) hangman]
    [(key=? "\r" ke) hangman]
    [(key=? "\t" ke) hangman]
    [(key=? "shift" ke) hangman]
    [(key=? "rshift" ke) hangman]
    [else (check-guess hangman ke)]))


(define (end? hangman)
  ; Game -> Boolean
  ; ends the game when solved or dead
  (or
   (= 0 (game-condemned hangman))
   (andmap (lambda (ch) (letter-guessed ch)) (game-word hangman))))


(define (game-over hangman)
  ; Game -> Img
  ; render the end-screen, win or lose
  (cond
    [(= 0 (game-condemned hangman))
     (overlay
      (above 
       (text "Game Over!" 128 "black")
       (text "You have failed" 64 "black"))
      (render hangman))]
    [(andmap (lambda (ch) (letter-guessed ch)) (game-word hangman))
     (overlay
      (above 
       (text "Game Over!" 128 "black")
       (text "You win!" 64 "black"))
      (render hangman))]))


(define (check-guess gm ke)
  ; Game KeyEvent -> Game
  (local (
          (define ch (string-downcase ke))
          (define (comp ltr) (string=? ch (letter-char ltr))))
    ; - IN -
    (cond
      [(ormap comp (game-word gm))
       (make-game 
        (map
         (lambda (ltr) (if (comp ltr) (make-letter (letter-char ltr) #t) ltr))
         (game-word gm))
        (game-condemned gm))]
      [else (make-game (game-word gm) (sub1 (game-condemned gm)))])))


(define (render-word wd)
  ; Word -> Img
  ; displays the letters of the secret word, or not,
  ; based on if they've been guessed
  (foldr beside NULLSPACE
         (map
          (lambda (ltr) (overlay
                         (cond
                           [(false? (letter-guessed ltr))
                            (text "_" TEXTSIZE "black")]
                           [else (text (letter-char ltr) TEXTSIZE "black")])
                         CHARCARD))
          wd)))
  

(define (get-random-word dictionary)
  "narwallawner")



; ==========================
;checks

(define mini-game-start (make-game  (list (make-letter "t" #f)
                                          (make-letter "h" #f)
                                          (make-letter "e" #f)) 5))
(define mini-game-t (make-game  (list (make-letter "t" #t)
                                      (make-letter "h" #f)
                                      (make-letter "e" #f)) 5))
(define mini-game-h (make-game  (list (make-letter "t" #f)
                                      (make-letter "h" #t)
                                      (make-letter "e" #f)) 5))
(define mini-game-e (make-game  (list (make-letter "t" #f)
                                      (make-letter "h" #f)
                                      (make-letter "e" #t)) 5))
(define mini-game-false (make-game  (list (make-letter "t" #f)
                                          (make-letter "h" #f)
                                          (make-letter "e" #f)) 4))
(define mini-game-win (make-game  (list (make-letter "t" #t)
                                        (make-letter "h" #t)
                                        (make-letter "e" #t)) 5))
(define mini-game-lose (make-game  (list (make-letter "t" #t)
                                         (make-letter "h" #f)
                                         (make-letter "e" #t)) 0))
(check-expect (check-guess mini-game-start "T") mini-game-t)
(check-expect (check-guess mini-game-start "h") mini-game-h)
(check-expect (check-guess mini-game-start "E") mini-game-e)
(check-expect (check-guess mini-game-start "x") mini-game-false)
(check-expect (guess mini-game-start "T") mini-game-t)
(check-expect (guess mini-game-start "\r") mini-game-start)
(check-satisfied mini-game-win end?)
(check-satisfied mini-game-lose end?)



; ==========================
; actions

(define WORD (map (lambda (ch) (make-letter ch #f))
                  (explode (get-random-word DICTIONARY))))

(define GAME (make-game WORD 5))

(check-guess GAME "w")

(play GAME)