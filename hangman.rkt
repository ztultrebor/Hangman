;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hangman) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)


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


(define-struct game [word condemned guesses])
; A Game is a [Word N Word]
; contains a representation of the secret word,
; as well as the state of the condemned man
#;
(define (fn-on-game g)
  (... (fn-on-word (game-word g)) ... (fn-on-N (game-condemned g))
       ... (fn-on-word (game-guesses g))))



; =========================
; constants

(define LOCATION "/usr/share/dict/words")
(define DICTIONARY (read-lines LOCATION))
(define HEIGHT 750)
(define CANVAS (empty-scene (quotient (* 1618 HEIGHT) 1000) HEIGHT "white"))
(define TEXTSIZE 64)
(define CHARCARD ; the backside of the letter "card"
  (rectangle TEXTSIZE TEXTSIZE "solid" "white"))
(define INVISICHARCARD
  (rectangle TEXTSIZE TEXTSIZE "outline" "white"))
(define ALPHABET "abcdefghijklmnopqrstuvwxyz")
(define GUESSTEXTSIZE 16)
(define GUESSCHARCARD
  (rectangle GUESSTEXTSIZE GUESSTEXTSIZE "solid" "white"))
(define SCAFFOLD (overlay (text "Γ" 200 "black")
                          (rectangle 256 256 "solid" "white")))
(define HEAD (above (rectangle 1 20 "solid" "black")
                    (circle 20 "outline" "black")))
(define BODY (rectangle 4 50 "solid" "black"))
(define LARM (rotate 45 (rectangle 2 20 "solid" "black")))
(define RARM (rotate 90 LARM))
(define LLEG (rotate 45 (rectangle 3 30 "solid" "black")))
(define RLEG (rotate 90 LLEG))
(define EYE (text "X" 8 "black"))
(define GRIMACE (add-curve (rectangle 20 10 "solid" "white")
                           0 5 75 1/2
                           20 5 75 1/2
                           "black"))
(define SPACER (rectangle 0 128 "solid" "white"))
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
   (above
    (render-scaffold (game-condemned hangman))
    SPACER
    (render-word-ingame (game-word hangman)))
   (overlay/align "right" "bottom"
                  (render-guesses (game-guesses hangman))
                  CANVAS)))


(define (guess hangman ke)
  ; Game KeyEvent -> Game
  ; determines a proper course of actions on a keyboard strike
  (cond
    [(> (string-length ke) 1) hangman]
    [(key=? " " ke) hangman]
    [(key=? "\b" ke) hangman]
    [(key=? "\r" ke) hangman]
    [(key=? "\t" ke) hangman]
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
  (local (
          (define (game-over-display hangman go-text text-color)
            (overlay
             (above 
              (text "Game Over!" 128 text-color)
              (text go-text 64 text-color))
             (overlay
              (above
               (render-scaffold (game-condemned hangman))
               SPACER
               (render-word-endgame (game-word hangman)))
              CANVAS))))
    (cond
      [(= 0 (game-condemned hangman))
       (game-over-display hangman "You have failed" "red")]
      [(andmap (lambda (ch) (letter-guessed ch)) (game-word hangman))
       (game-over-display hangman "You win!" "green")])))


(define (generate-word dictionary)
  ; [ListOf String] -> Word
  ; gets a random word from the dictionary and preps it for hangman game
  (local (
          (define selection
            (retrieve-word dictionary (random (length dictionary))))
          (define no-caps (string-downcase selection)))
          
    ; - IN -
    (string-to-word no-caps)))
  

(define (retrieve-word dictionary n)
  ; [ListOf String] N -> String
  ; retrieves the nth word from the dictionary
  (cond
    [(= 0 n) (first dictionary)]
    [else (retrieve-word (rest dictionary) (sub1 n))]))


(define (string-to-word str)
  ; String -> Word
  ; converts a dictuonary string into a hangman word
  (local (
          (define explosure (explode str)))
    ; - IN -
    (map (lambda (ch) (make-letter ch #f)) explosure)))


(define (check-guess gm ke)
  ; Game KeyEvent -> Game
  ; update game state based on valid guess
  (local (
          (define ch (string-downcase ke))
          (define (comp ltr)
            ; Letter -> Boolean
            (string=? ch (letter-char ltr)))
          (define (remap-word word)
            ; Word -> Word
            ; pinpoints correct guesses
            (map
             (lambda (ltr)
               (if (comp ltr) (make-letter (letter-char ltr) #t) ltr)) word)))
    ; - IN -
    (cond
      [(ormap comp (game-word gm))
       (make-game 
        (remap-word (game-word gm))
        (game-condemned gm)
        (remap-word (game-guesses gm)))]
      [else
       (make-game
        (game-word gm)
        (sub1 (game-condemned gm))
        (remap-word (game-guesses gm)))])))


(define (render-word word pred blocking-char fontsize fontcolor letter-backside)
  ; Word [1String -> Boolean] 1String N String Img -> Img
  ; abstraction for all word-rendering
  ; takes a word, a boolean predicate (false? or true?),
  ; a character to display in place of an unknown letter,
  ; the font size, the font color, and the "card" backside
  ; to dislay when the letter has not yet been revealed
  (foldr beside NULLSPACE
         (map
          (lambda (ltr) (overlay
                         (cond
                           [(pred (letter-guessed ltr))
                            (text blocking-char fontsize fontcolor)]
                           [else (text (letter-char ltr) fontsize fontcolor)])
                         letter-backside)) word)))


(define (render-word-ingame word)
  ; Word -> Img
  ; displays the letters of the secret word, or not,
  ; based on if they've been guessed
  (render-word word false? "_" TEXTSIZE "black" CHARCARD))


(define (render-word-endgame word)
  ; Word -> Img
  ; displays all the letters of the secret word
  (overlay 
   (render-word word true? "" TEXTSIZE "red" INVISICHARCARD)
   (render-word word false? "" TEXTSIZE "black" INVISICHARCARD)))


(define (render-guesses word)
  ; Word -> Img
  ; displays all the letters that have yet to be been guessed
  (render-word word true? "" GUESSTEXTSIZE "black" GUESSCHARCARD))


(define (true? x)
  ; Boolean -> Boolean
  ; opposite of false?
  (not (false? x)))


(define (render-scaffold n)
  ; N -> Img
  ; render the scaffold in various stages of construction
  (cond
    [(= 0 n) (place-image GRIMACE 184 108 (render-scaffold (add1 n)))]
    [(= 1 n) (place-image EYE 192 98 (render-scaffold (add1 n)))]
    [(= 2 n) (place-image EYE 176 98 (render-scaffold (add1 n)))]
    [(= 3 n) (place-image RLEG 173 178 (render-scaffold (add1 n)))]
    [(= 4 n) (place-image LLEG 195 178 (render-scaffold (add1 n)))]
    [(= 5 n) (place-image RARM 176 133 (render-scaffold (add1 n)))]
    [(= 6 n) (place-image LARM 192 133 (render-scaffold (add1 n)))]
    [(= 7 n) (place-image BODY 184 143 (render-scaffold (add1 n)))]
    [(= 8 n) (place-image HEAD 184 88 (render-scaffold (add1 n)))]
    [(= 9 n) (overlay SCAFFOLD (render-scaffold (add1 n)))]
    [(= 10 n) (rectangle 256 256 "solid" "white")]))



; ==========================
;checks

(define mini-game-start (make-game (list (make-letter "t" #f)
                                         (make-letter "h" #f)
                                         (make-letter "e" #f))
                                   5 (list (make-letter "t" #f)
                                           (make-letter "h" #f)
                                           (make-letter "e" #f))))
(define mini-game-t (make-game (list (make-letter "t" #t)
                                     (make-letter "h" #f)
                                     (make-letter "e" #f))
                               5 (list (make-letter "t" #t)
                                       (make-letter "h" #f)
                                       (make-letter "e" #f))))
(define mini-game-h (make-game (list (make-letter "t" #f)
                                     (make-letter "h" #t)
                                     (make-letter "e" #f))
                               5 (list (make-letter "t" #f)
                                       (make-letter "h" #t)
                                       (make-letter "e" #f))))
(define mini-game-e (make-game (list (make-letter "t" #f)
                                     (make-letter "h" #f)
                                     (make-letter "e" #t))
                               5 (list (make-letter "t" #f)
                                       (make-letter "h" #f)
                                       (make-letter "e" #t))))
(define mini-game-false (make-game (list (make-letter "t" #f)
                                         (make-letter "h" #f)
                                         (make-letter "e" #f))
                                   4 (list (make-letter "t" #f)
                                           (make-letter "h" #f)
                                           (make-letter "e" #f))))
(define mini-game-win (make-game (list (make-letter "t" #t)
                                       (make-letter "h" #t)
                                       (make-letter "e" #t))
                                 5 (list (make-letter "t" #f)
                                         (make-letter "h" #f)
                                         (make-letter "e" #f))))
(define mini-game-lose (make-game (list (make-letter "t" #t)
                                        (make-letter "h" #f)
                                        (make-letter "e" #t))
                                  0 (list (make-letter "t" #f)
                                          (make-letter "h" #f)
                                          (make-letter "e" #f))))
(define beatles-words (list "eggman" "walrus" "Lucy"))
(define stones-words (list "RT"))
(check-expect (check-guess mini-game-start "T") mini-game-t)
(check-expect (check-guess mini-game-start "h") mini-game-h)
(check-expect (check-guess mini-game-start "E") mini-game-e)
(check-expect (check-guess mini-game-start "x") mini-game-false)
(check-expect (guess mini-game-start "T") mini-game-t)
(check-expect (guess mini-game-start " ") mini-game-start)
(check-expect (guess mini-game-start "\r") mini-game-start)
(check-expect (guess mini-game-start "wheel-up") mini-game-start)
(check-satisfied mini-game-win end?)
(check-satisfied mini-game-lose end?)
(check-expect (retrieve-word beatles-words 1) "walrus")
(check-expect (generate-word stones-words)
              (list (make-letter "r" #false) (make-letter "t" #false)))



; ==========================
; actions


(define WORD (generate-word DICTIONARY))
(define GAME (make-game WORD 9 (string-to-word ALPHABET)))
(play GAME)