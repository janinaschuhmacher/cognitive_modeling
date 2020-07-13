;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ACT-R model by Janina Schuhmacher and Alexander Dienst



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    Original "Moon Landing" GUI by Lisa-Madeleine Dörr and Stefan Lindner (both chair for Applied Cognitive Modeling, TU Berlin)  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    November 2017 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    Reworked 2019 by Kai Preuß
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-model moonlanding

(sgp :v t :esc t :egs 3 :show-focus t :trace-detail medium :ult t :visual-num-finsts 10 :declarative-finst-span 1.5)   
    
(chunk-type try-strategy strategy state goal-loc steps-per-move)
(chunk-type encoding x-sps y-sps goal-loc try-right try-left try-fall one-right two-right one-below two-below one-left two-left left-below asteroid  outcome move)
    


(define-chunks 
    (goal isa try-strategy state start)
    (start) (encode-goal) (find-move) (move) (move-right) (move-left) (fall) (update-pos) (simulate-fall) (simulate-right) (simulate-left) (search-environment) (attend-asteroid) )


; Ziel suchen
    ; Start-Produktion (wird nur am Anfang einmal ausgeführt)
(p start-trial
   =goal>
      isa      try-strategy
      state    start
	  
	  !eval! (not *outcome*)

  ==>
; unterste Reihe nach grüner Plattform (Ziel) absuchen
   +visual-location>
      isa       visual-location
      color     green
      screen-y  highest
   
   ;  maximale Schrittzahl abrufen (gehört zum Weltwissen)   
   !bind! =steps *max-side-dist*
   
   =goal>
      state    encode-goal
   
   ; wie viele Schritte kann das Raumschiff machen, bis es fällt?
      steps-per-move  =steps
   )




; x-Koordinate des Ziels und X-Koordinate des Raumschiffs enkodieren
    
(p encode-goal
   =goal>
       state  encode-goal
   =visual-location>
      color   green
      screen-x    =goal-x
   ?imaginal>
       state   free

==> 
   ;transformiert, damit x- und y-Koordinaten verglichen werden können
   !bind!   =goal-square (round (- (/ =goal-x *elem-size*) 1.5))
   !bind!   =x-sps (first *spship-pos*)
   !bind!   =y-sps (second *spship-pos*)
   ;initiiert eine Liste, in der im Laufe des Spiels die Positionen der Asteroiden gespeichert werden
   !bind!   =asteroid-pos (list (list 0 0))


   ; Position des Raumschiffs im imaginal buffer speichern
    +imaginal>
       isa   encoding
   ; X-Koordinate des Raumschiffs
       x-sps =x-sps
   ; Y-Koordinate des Raumschiffs
       y-sps =y-sps
   ; Liste für Hindernisse in aktueller Situation initiieren
       asteroid   =asteroid-pos
       
 ; Zielposition im Goal-Buffer speichern (nur x-Koordinate, da die y-Koordinate immer gleich ist)
    =goal>
       state   find-move
   ; X-Koordinate des Ziels
       goal-loc =goal-square  
)
    
    
      
     
; Wenn das Ziel rechts des Raumschiffs liegt, 
; versucht das Raumschiff, zuerst nach rechts zu fliegen, sofern es dort keine Hindernisse gibt
(p try-right
   =goal>
       state   find-move
       goal-loc =goal-square
    =imaginal>
       isa   encoding
       x-sps     =x-sps
       y-sps     =y-sps   
   ;; die Produktion soll nicht feuern, wenn das Raumschiff schon festgestellt hat, dass es in diesem Schritt
   ;; nicht nach rechts fliegen sollte
       - try-right   0
   
   ; testet, dass das Ziel weiter rechts liegt als das Raumschiff
   !eval! (> =goal-square =x-sps) 
   ==>   
   
   ;ein Kästchen rechts des Raumschiffs auf Hindernisse überprüfen
   !bind! =next-square-x (* (+ =x-sps 2.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 0.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      ;da das Raumschiff sowohl die Wand, als auch Hindernisse vermeiden wil, sucht es nach Objekten vom kind oval, die nicht die Farbe des Ziels ('green') haben
       kind   oval
       - color   green
      screen-x =next-square-x
      screen-y =next-square-y 
   
   =imaginal>
   
   ; move-right: Raumschiff wird als nächstes evaluieren, ob es nach rechts fliegen könnte
   ; Konzept: Zielorientiertes Handeln
   =goal>
       state move-right
       goal-loc =goal-square

   )
    

	 
; falls ein Hindernis rechts des Raumschiffs liegt, kann es nicht nach rechts fliegen
(p dont-go-right-asteroid-to-the-right
   =goal>
       state move-right
   
   ?manual>
	 - state			busy  
   
   =imaginal>
       - try-right   0
   
   ; Hindernis rechts des Raumschiffs
   =visual-location>
      kind   oval
      - color   green
    
   ==>
   
    =imaginal>
   ; hier merke es sich, dass es in diesem Zug nicht mehr nach rechts gehen wird
       try-right     0
; sondern es wird eine andere Richtung ausprobieren
   =goal>
       state find-move
   )

   
; Ziel liegt links des Raumschiffs
; analog zu try-right
(p try-left
   =goal>
       state   find-move
       goal-loc =goal-square
    =imaginal>
       isa   encoding
       x-sps     =x-sps
       y-sps     =y-sps
       - try-left   0
   
   ; Abfrage, ob Ziel links des Raumschiffs liegt
   !eval! (< =goal-square =x-sps) 
   ==>   
   
   ;ein Kästchen links des Raumschiffs überprüfen
   !bind! =next-square-x (* (+ =x-sps 0.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 0.5) *elem-size*)
   
   
   +visual-location>
      isa      visual-location
   
   ;; analog zu try-right
      kind   oval
      - color   green
      screen-x =next-square-x
      screen-y =next-square-y 
    
   =imaginal>
   =goal>
       state move-left
       goal-loc =goal-square
   
)


	  
; Asteroid links des Raumschiffs  
    ; analog zu p dont-go-right-asteroid-to-the-right
(p dont-go-left-asteroid-to-the-left
   =goal>
       state move-left
       goal-loc =goal-square
   =visual-location>
      kind   oval 
      - color   green
       
   =imaginal>
      - try-left   0
   ==> 
   
   =imaginal>
       try-left  0
   
   =goal>
       state find-move
   )
    
    
; wenn das Ziel sich unter dem Raumschiff befindet, würde es sich absichtlich fallen lassen wollen
(p intentional-fall
    =goal>
       state   find-move
       goal-loc =goal-square
    =imaginal>
       isa   encoding
       x-sps   =x-sps
       y-sps   =y-sps
       - try-fall   0
   
; Ziel liegt unter dem Raumschiff
   !eval! (eq =goal-square =x-sps) 
   ==>   
      
   =imaginal>
   
   ;ein Kästchen unter dem Raumschiff auf Hindernisse überprüfen
   !bind! =next-square-x (* (+ =x-sps 1.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 1.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
       - color  green
      screen-x =next-square-x
      screen-y =next-square-y
   
   =goal>
       state fall
)

; falls sich ein Asteroid unter dem Raumschiff befindet, darf es sich in dem Zug nicht fallen lassen
(p dont-fall-asteroid-below
   =goal>
       state   fall
   
 ; Hindernis unter dem Raumschiff entdeckt  
   =visual-location>
      isa      visual-location
      kind   oval
      - color   green
 
   =imaginal>
       - one-below   "free"
   
   ==> 
      =imaginal>
       try-fall  0
   
   =goal>
       state find-move

   )
    
  ;; Konzept: Schritte simulieren


; unimttelbar rechts des Ramschiffs wurde kein Asteroid gefunden
    ; das Raumschiff will nach rechts gehen und überprüft, ob es danach einen weiteren sinnvollen Schritt machen könnte
    
(p simulate-step-right
   =goal>
       state   move-right

   =imaginal>
       isa   encoding
       x-sps     =x-sps
       y-sps     =y-sps
   
   ; unmittelbar rechts des Raumschiffs kein Hindernis gefunden
    ?visual-location>
       state   error
   
   ==>   
;zwei Kästchen rechts des Raumschiffs auf Hindernisse überprüfen
   !bind! =next-square-x (* (+ =x-sps 3.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 0.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
       - color green
      screen-x =next-square-x
      screen-y =next-square-y
  
   ;  enkodieren, dass das Feld unmittelbar rechts des Raumschiffs frei war
   =imaginal>
       one-right   "free"
       x-sps  =x-sps
       y-sps  =y-sps

   =goal>
       state   simulate-right
)

    
;; wenn zwei Felder rechts des Raumschiffs keine Hindernisse gefunden werden, kann es wie geplant nach rechts gehen
(p go-right
   =goal>
       state   simulate-right
   
   ?manual>
	 - state			busy  
   
     =imaginal>
   ; eins rechts neben dem Raumschiff ist frei
       one-right   "free"
       
   
   ; zwei Felder rechts des Raumschiffs wurde auch kein Hindernis gefunden
    ?visual-location>
       state   error
   
   ==> 
   +manual>
      cmd       press-key
      key       "e"
   
   ; merkt sich, dass das Raumschiff gerade nach rechts gegangen ist --> soll verhindern, dass das Raumschiff wieder zurückfliegt
    =imaginal>
       try-left   0
       one-right  nil
       move    "r"
   
   =goal>
       state update-pos
 
   )
         


;; 2mal rechts geht nicht
    ;; könnte das Raumschiff nach rechts und dann nach unten?
(p search-asteroids-right-below
   =goal>
       state   simulate-right
   
   =imaginal>
       isa   encoding
       x-sps     =x-sps
       y-sps     =y-sps
       one-right   "free"
   
   =visual-location>
       kind   oval
       - color   green
   
   ==> 
   
   ;könnte es nach rechts und dann nach unten?
   !bind! =next-square-x (* (+ =x-sps 2.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 1.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
      - color  green
      screen-x =next-square-x
      screen-y =next-square-y
   
   ;; zwei Felder rechts des Raumschiffs ist ein Hindernis
   =imaginal>
       two-right   0
       x-sps  =x-sps
       y-sps  =y-sps
   
   =goal>

   )

;; wenn es rechts unten auch ein Hindernis findet: 
;; nach einem Schritt nach rechts könnte es nicht weiter
;; also probiert es andere Richtungen aus
(p dont-go-right-simulation
   =goal>
       state   simulate-right

   =imaginal>
       two-right   0
   
   =visual-location>
       kind   oval
       - color   green
   
   ==> 
   
   ; das Raumschiff merkt sich, dass es in diesem Schritt nicht mehr versuchen wird, nach rechts zu gehen
   =imaginal>
       try-right   0

   =goal>
       state   find-move

   )


;; nach links     ;; Schritte vorausdenken
; analog zu simulate-step-right
    
(p simulate-step-left
   =goal>
       state   move-left

   =imaginal>
       isa   encoding
       x-sps     =x-sps
       y-sps     =y-sps
    
   ?visual-location>
       state   error
   
   ==>   
;zwei Kästchen links des Raumschiffs auf Hindernisse überprüfen
   !bind! =next-square-x (* (- =x-sps 0.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 0.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
       - color   green
      screen-x =next-square-x
      screen-y =next-square-y
   
   =imaginal>
       one-left   "free"
   
   =goal>
       state   simulate-left
         
)

;; wenn links des Raumschiffs keine Hindernisse gefunden werden, kann es wie geplant nach links gehen
(p go-left
   =goal>
       state   simulate-left
   
   ?manual>
	 - state			busy  

     =imaginal>
       one-left   "free"
   
    ?visual-location>
       state   error
   
   ==> 
   +manual>
      cmd       press-key
      key       "q"
   
   =imaginal>
       try-right   0
       one-left    nil
       move      "l"

   
   =goal>
       state update-pos
 
   )

; links unten?
; zweimal nach links geht nicht

(p search-asteroids-left-below
   =goal>
       state   simulate-left

   =imaginal>
       x-sps  =x-sps
       y-sps  =y-sps
       one-left  "free"
   
   =visual-location>
       kind   oval
       - color   green
   
   ==> 
   
   ;könnte es nach links und dann nach unten?
   !bind! =next-square-x (* (+ =x-sps 0.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 1.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
       - color   green
      screen-x =next-square-x
      screen-y =next-square-y
   
   =imaginal>
       two-left   0
   
   =goal>

   )

;; nach einem Schritt nach links könnte es nicht weiter
(p dont-go-left-simulation
   =goal>
       state   simulate-left

   =imaginal>
       two-left   0

   
   =visual-location>
       kind   oval
       - color   green
   
   ==> 
   
   =imaginal>
       try-left   0

   =goal>
       state   find-move

   )


;; nach unten     ;; Schritte vorausdenken
; Raumschiff will fallen und überprüft, ob es danach einen weiteren sinnvollen Schritt machen könnte
    
(p simulate-fall
   =goal>
       state   fall

   =imaginal>
       isa   encoding
       x-sps     =x-sps
       y-sps     =y-sps
       - one-below   "free"
   
   ?visual-location>
       state   error
   
   ==>   
;zwei Kästchen unter dem Raumschiff auf Hindernisse überprüfen
   !bind! =next-square-x (* (+ =x-sps 1.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 2.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
      - color  green
      screen-x =next-square-x
      screen-y =next-square-y
   
   =imaginal>
       one-below "free"
       
      
   =goal>
       state   simulate-fall
       
)

;; wenn zwei Kästchen unter dem Raumschiff keine Hindernisse gefunden werden, kann es wie geplant fallen
(p fall
   =goal>
       state   simulate-fall

    =imaginal>
       one-below   "free"
  
    ?visual-location>
       state   error
   
   ==> 
        =imaginal>
       one-below   nil
       move       "d"
   
   ; dann wird das Raumschiff die übrige Zeit nutzen, um die Umgebung abzusuchen
   =goal>
       state search-environment
 
   )

; kÖnnte das Raumschiff nach dem Fall einen Schritt nach links machen?
(p search-asteroids-left-fall
   =goal>
       state   simulate-fall

   =imaginal>
       x-sps  =x-sps
       y-sps  =y-sps
       one-below "free"
       left-below nil
 
   ;Hindernis liegt unter dem Raumschiff
   =visual-location>
       kind   oval
       - color   green
   
   ==> 
   
   ;könnte es nach links und dann nach unten?
   !bind! =next-square-x (* (+ =x-sps 0.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 1.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
      - color  green
      screen-x =next-square-x
      screen-y =next-square-y
   
   =imaginal>
       one-below   "free"
       two-below   0

   =goal>
       state   simulate-fall
       
   )

;; links unter dem Raumschiff liegt auch ein Hindernis --> rechts unten?

(p search-asteroids-right-fall
   =goal>
       state   simulate-fall

   =imaginal>
       x-sps  =x-sps
       y-sps  =y-sps
       one-below   "free"
       two-below   0
       left-below   nil
   
   =visual-location>
       kind   oval
       - color   green
   
   ==> 
   
  
   !bind! =next-square-x (* (+ =x-sps 2.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 1.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
      - color  green
      screen-x =next-square-x
      screen-y =next-square-y
   
   =imaginal>
       one-below    "free"
       two-below    0
       left-below   0
       
   =goal>
       state   simulate-fall

   )

;; wenn es nicht weiter könnte, nicht fallen lassen
(p dont-fall
   =goal>
       state     simulate-fall
   =visual-location>
      kind   oval
    
   =imaginal>
       one-below   "free"
       two-below   0
       left-below  0

   ==> 
   
   =imaginal>
       try-fall  0
   
   =goal>
       state find-move
   )
    
;; Asteroiden vermeiden
;; die folgenden Produktionen feuern, wenn das Raumschiff sich entschieden hat, im aktuellen Zug nicht in Richtung des Ziels zu fliegen
    
; das Raumschiff probiert aus, ob es Sinn macht, sich fallen zu lassen, weil rechts des Raumschiffs ein Asteroid ist
(p avoid-asteroids-to-right-try-fall
   =goal>
       state   find-move

   =imaginal>
       x-sps  =x-sps
       y-sps  =y-sps
       try-right 0
   ; sofern es Fallen in diesem Zug noch nicht ausgeschlossen hat
       - try-fall  0
   
   ?visual-location>
       - state   error
   ==>
      
;ein Kästchen unter dem Raumschiff überprüfen
   !bind! =next-square-x (* (+ =x-sps 1.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 1.5) *elem-size*)
   
   
   +visual-location>
      isa      visual-location
      kind   oval
      - color   green
      screen-x =next-square-x
      screen-y =next-square-y 
   
   =imaginal>
   
   =goal>
       state   fall
   
)

    
; Raumschiff will sich fallen lassen, weil in der Richtung des Ziels (links)
; ein Asteroid liegt (analog zur Produktion avoid-asteroids-to-right-fall)
(p avoid-asteroids-to-left-try-fall
   =goal>
       state   find-move

   =imaginal>
       x-sps  =x-sps
       y-sps  =y-sps
       try-left  0
       - try-fall  0
   

   ?visual-location>
       - state   error
   ==>
   
   !bind! =next-square-x (* (+ =x-sps 1.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 1.5) *elem-size*)
   
   
   +visual-location>
      isa      visual-location
      kind   oval
      - color   green
      screen-x =next-square-x
      screen-y =next-square-y 
   
   =imaginal>
   
   =goal>
       state   fall
   
)

 ; Ziel liegt unter dem Raumschiff, aber da kann es nicht hin, weil dort ein Hindernis liegt
    ; also überprüft es, ob es Sinn macht, nach rechts auszuweichen
    
(p avoid-asteroids-below
   =goal>
       state   find-move 
   
   =imaginal>
       x-sps  =x-sps
       y-sps  =y-sps
       try-fall    0
       - try-left   0
       - try-right  0
   ==> 

       ;ein Kästchen rechts des Raumschiffs auf Hindernisse überprüfen
       !bind! =next-square-x (* (+ =x-sps 2.5) *elem-size*)
       !bind! =next-square-y (* (+ =y-sps 0.5) *elem-size*)
   
      +visual-location>
      isa      visual-location
      kind   oval
      - color   green
      screen-x =next-square-x
      screen-y =next-square-y 
   
    =imaginal>
   
   =goal>
       state   move-right

   ) 

    ; wenn rechts des Raumschiffs kein Hindernis liegt --> nach rechts ausweichen
(p avoid-asteroids-below-going-right
   =goal>
       state   find-move
   
   =imaginal>
       try-fall    0
       - try-left   0
       - try-right  0
   
    ?visual-location>
       state   error
   
    ?manual>
	 - state			busy
   
   ==> 
   +manual>
     cmd       press-key
     key       "e"
   
    =imaginal>
       try-fall    nil
       try-left   0
   
   =goal>
       state update-pos
   ) 
    
; wenn rechts ein Hindernis liegt --> nach links ausweichen
(p avoid-asteroids-below-going-left
   =goal>
       state   find-move
    
   =imaginal>
       try-fall    0
   
    =visual-location>
      kind   oval
   
    ?manual>
	 - state			busy
   
   ==> 
   +manual>
     cmd       press-key
     key       "q"
   
    =imaginal>
       try-fall    nil
       try-right   0
   
   =goal>
       state update-pos
   ) 
   
    
; Asteroiden ausweichen und dabei nicht in Richtung Ziel gehen
    ;; diese Produktionen feuern, wenn das Raumschiff zwei Richtungen ausgeschlossen hat, weil sich dort Hindernisse befinden
    ;; d.h., dass nur noch eine Richtung übrig ist, in die es fliegen kann, ohne eine Kollision zu provozieren
    

 ; Rechts und unten wurden als sinnvolle Richtungen für den aktuellen Zug ausgeschlossen
    ; deshalb bleibt nur noch, nach links zu fliegen
(p avoid-asteroids-go-left
   =goal>
       state   find-move
   
   =imaginal>
       try-right   0
       try-fall    0

    ?manual>
	 - state			busy
   
   ==> 
   
    +manual>
     cmd       press-key
     key       "q"
   
    =imaginal>
       try-fall    nil
   
   =goal>
       state update-pos
   ) 
   
   
 ; Links und unten wurden als sinnvolle Richtungen für den aktuellen Zug ausgeschlossen
    ; deshalb bleibt nur noch, nach rechts zu fliegen  
(p avoid-asteroids-go-right
   =goal>
       state   find-move
   
   =imaginal>
       try-left   0
       try-fall    0
     
    ?manual>
	 - state			busy
   
   ==> 
   
    +manual>
     cmd       press-key
     key       "e"
   
    =imaginal>
       try-fall    nil
   
   =goal>
       state update-pos
   ) 
    

    
    ; wenn der manual Befehl ausgeführt wurde und sich die x-Position geändert hat, dann wird die Raumschiff-Position upgedatet
    
(p update-position   
   =goal>
       state   update-pos
       steps-per-move  =steps

    ?manual>
	 - state			busy 
   
    =imaginal>
       isa   encoding
       x-sps     =x-sps
       y-sps     =y-sps

   ;x-Position des Raumschiffs hat sich geändert
   !eval! (not(eq =x-sps (first *spship-pos*)))

   ==> 

   ; eigene Position updaten
    !bind!   =x-sps-new (first *spship-pos*)
    !bind!   =y-sps-new (second *spship-pos*)

   ; alle anderen Variablen, die den aktuellen Zustand vor dem Fall repräsentieren, werden auf 'nil' gesetzt
   ; bis auf try-right und try-left (dadurch soll verhindert werden, dass das Raumschiff wieder zurückfliegt)
   =imaginal>
       isa   encoding
       x-sps     =x-sps-new
       y-sps     =y-sps-new
       try-fall  nil
       one-right nil
       two-right nil
       one-below nil
       two-below nil
       one-left nil
       two-left nil
       left-below nil

   ; neuen Schritt überlegen
   =goal>
       state   find-move
   )
    
; wenn das Raumschiff darauf wartet, zu fallen, soll es die restliche Zeit nutzen, um die Umgebung nach Hindernissen abzusuchen
(p encode-environment-for-learning
   =goal>
       state   search-environment   ; nach Entscheidung zum Fall
   
   =imaginal>
       x-sps   =x-sps
       y-sps   =y-sps
   
   ?visual-location>
       state   free
   
   ?visual>
       state   free
   
   ; so lange wir noch nicht gefallen sind
    !eval! (eq =y-sps (second *spship-pos*))
   
   ==>

   !bind!   =spship-y  (* (+ =x-sps 0.5) *elem-size*)

   ; sucht Asteroiden, die sich in der Nähe des Raumschiffs befinden
   +visual-location>
        :attended nil
       color  red
   
   =goal>
       state   attend-asteroid
   =imaginal>
   
   )
    
 
; die Asteroiden sollen verarbeitet werden
(p attend-asteroids
   =goal>
       state   attend-asteroid
   
   =imaginal>
       x-sps   =x-sps
       y-sps   =y-sps
       asteroid   =asteroids
   
   =visual-location>
       color  red
       screen-x  =screen-x
       screen-y  =screen-y
    
    ?visual>
        state free
   
    ; so lange das Raumschiff noch nicht gefallen ist
    !eval! (eq =y-sps (second *spship-pos*))
   
   ==>
   ; Abstand des Asteroiden zum Raumschiff berechnen
   !bind! =asteroid-pos-x (round (- =x-sps (- (/ =screen-x *elem-size*) 1.5)))
   !bind! =asteroid-pos-y (round (- =y-sps (- (/ =screen-y *elem-size*) 0.5)))
   
   ; die enkodierten Asteroiden in diesem Zug werden in einer Liste mit den jeweiligen Abständen in 
   ; x- und y-Richtung im asteroid-Slot des imaginal buffers gespeichert
   !bind! =asteroid-pos  (cons =asteroid-pos-x '(=asteroid-pos-y))
   !bind! =new-asteroids (append '=asteroids '(=asteroid-pos))

   ;Aufmerksamkeit auf Asteroiden lenken
   +visual>
        cmd move-attention
        screen-pos =visual-location
   
   ; in imaginal buffer schreiben
   =imaginal>
       asteroid  =new-asteroids
   
   ; weitere Asteroiden suchen
   =goal>
       state   search-environment
   
   )

    ; wenn das Raumschiff gefallen ist, wird der imaginal buffer geleert, damit die 
    ; Infos über die gefundenen Asteroiden ins deklarative Gedächtnis gelangen
  (p clear-new-imaginal-chunk
    =goal>
         goal-loc =goal-square
     
     =imaginal>
       - move   nil
         y-sps   =y-sps
     
     ; nachdem das Raumschiff gefallen ist
    !eval! (not(eq =y-sps (second *spship-pos*)))

     ==>
     -imaginal>
     
     ; danach soll die Position des Raumschiffs aktualisiert werden
     =goal>
         state   update-pos
  )
      

; Enkodiert die neue Position, nachdem das Raumschiff gefallen ist 
    (p update-position-after-falling
   =goal>
       state   update-pos
       
   ?imaginal>
        buffer   empty
       
    ?manual>
	 - state			busy 
   
   ==> 

   ; eigene Position updaten
    !bind!   =x-sps-new (first *spship-pos*)
    !bind!   =y-sps-new (second *spship-pos*)
    !bind!   =asteroid   (list 0 0)



; neue Position merken
    +imaginal>
       isa   encoding
       x-sps     =x-sps-new
       y-sps     =y-sps-new
       asteroid  =asteroid

   
   ; neuen Schritt überlegen
   =goal>
       state   find-move
   )
    

    
  
;; Erinnern!
; wenn das Spiel gewonnen wurde
(p learn-win-situation
   
   =goal>
       outcome   nil
   
   !eval! (eq *outcome* 'win)
   
   ==> 

   =goal>
       outcome   "win"
  )
    
; wenn das Spiel verloren wurde
(p learn-lose-situation
   
   =goal>
       outcome   nil
   
   !eval! (eq *outcome* 'lose)
   
   ==> 

   =goal>
       outcome   "lose"
     
  )    

; restliche Zeit nutzen, wenn das Raumschiff sich schon nicht mehr bewegt, um die gelernten Chunks zu labeln ('win' oder 'lose')
(p retrieve-chunks-for-labeling
   
   =goal>
       - outcome   nil
   
   ?retrieval>
       state   free
   
   ?imaginal>
       state   free

   ==> 
   
    +retrieval>
       isa   encoding
       - asteroid   nil
       - move      nil
       outcome   nil
       :recently-retrieved nil
   
   -imaginal>
   
   =goal>
   
)
    
(p label-chunks
   
   =retrieval>
       isa   encoding
       asteroid   =asteroids
       move       =move
       outcome   nil
   
   =goal>
       outcome   =outcome
   
   ?imaginal>
       buffer   empty
   
   ==>
    +imaginal>
       isa   encoding
       asteroid   =asteroids
       move       =move
       outcome   =outcome
   
   )
   
    
; Fall Back Optionen, falls keine andere Option Sinn macht
; falls das Raumschiff nicht mehr nach links zurückgehen kann, aber das Ziel links liegt, versucht es, sich fallen zu lassen
    (p fall-back-dont-go-left
   =goal>
       state   find-move
       goal-loc =goal-square
    =imaginal>
       isa   encoding
       x-sps   =x-sps
       y-sps   =y-sps
       try-left     0
       - try-fall   0
       
   
; Ziel liegt links des Raumschiff
   !eval! (< =goal-square =x-sps) 
   ==>   
      
   =imaginal>
   
   ;ein Kästchen unter dem Raumschiff auf Hindernisse überprüfen
   !bind! =next-square-x (* (+ =x-sps 1.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 1.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
       - color  green
      screen-x =next-square-x
      screen-y =next-square-y
   
   =goal>
       state fall
)

; falls das Raumschiff nicht mehr nach rechts zurückgehen kann,aber das Ziel rechts liegt, versucht es, sich fallen zu lassen
(p fall-back-dont-go-right
   =goal>
       state   find-move
       goal-loc =goal-square
    =imaginal>
       isa   encoding
       x-sps   =x-sps
       y-sps   =y-sps
       try-right     0
       - try-fall   0
       
   
; Ziel liegt links des Raumschiff
   !eval! (< =goal-square =x-sps) 
   ==>   
      
   =imaginal>
   
   ;ein Kästchen unter dem Raumschiff auf Hindernisse überprüfen
   !bind! =next-square-x (* (+ =x-sps 1.5) *elem-size*)
   !bind! =next-square-y (* (+ =y-sps 1.5) *elem-size*)
   
   +visual-location>
      isa      visual-location
      kind     oval
       - color  green
      screen-x =next-square-x
      screen-y =next-square-y
   
   =goal>
       state fall
)   
       
(goal-focus goal)

)