;; Potrebne biblioteke
(load "C:/Users/laura/quicklisp/setup.lisp")
(ql:quickload "hunchentoot")

(defun load-html-template (filename)
  "Učitaj statički HTML iz datoteke."
  (with-open-file (stream filename :direction :input)
    (let ((content ""))
      (loop for line = (read-line stream nil)
            while line
            do (setf content (concatenate 'string content line)))
      content)))

;; Definicija mreže
(defun create-grid (rows cols)
  "Kreiranje prazne mreže veličine ROWS x COLS."
  (make-array (list rows cols) :initial-element 0))

(defun set-cell (grid x y value)
  "Postavljanje vrijednosti ćelije na koordinatama X, Y."
  (setf (aref grid x y) value))

;; Globalna mreža
(defvar *grid* (create-grid 20 20))

(defun grid-to-html (grid)
  "Pretvaranje mreže u HTML tablicu s mogućnošću klikanja."
  (let ((html "<table>"))
    (dotimes (x (array-dimension grid 0))
      (setf html (concatenate 'string html "<tr>"))
      (dotimes (y (array-dimension grid 1))
        (let ((class (if (= (aref grid x y) 1) "alive" "dead")))
          (setf html (concatenate 'string html
                                  "<td class='" class "' data-row='" (princ-to-string x)
                                  "' data-col='" (princ-to-string y) "'></td>"))))
      (setf html (concatenate 'string html "</tr>")))
    (setf html (concatenate 'string html "</table>"))
    html))

(defun clear-grid ()
  "Postavljanje svih ćelija u mreži na 0  tj. resetiranje."
  (setf *grid* (create-grid (array-dimension *grid* 0) (array-dimension *grid* 1))))

(defun clear-grid-handler ()
  "Handler za čišćenje mreže i vraćanje prazne mreže."
  (clear-grid)
  (grid-to-html *grid*)) ; Vraća samo HTML mrežu

(defun resize-grid-handler (rows cols)
  "Promijeni veličinu mreže i vrati HTML mreže."
  (setf *grid* (create-grid (parse-integer rows) (parse-integer cols)))
  (grid-to-html *grid*)) ; Vraća samo HTML mreže

(defun toggle-cell-handler (row col)
  "Mijenja stanje ćelije na temelju klika korisnika."
  (let ((x (parse-integer row))
        (y (parse-integer col)))
    (set-cell *grid* x y (if (= (aref *grid* x y) 1) 0 1)))
  (grid-to-html *grid*)) ; Vraća samo HTML mreže

(defun count-neighbors (grid x y)
  "Broji žive susjede ćelije na koordinatama X, Y."
  (let ((count 0))
    (dotimes (dx 3)
      (dotimes (dy 3)
        (let ((nx (+ x (- dx 1)))
              (ny (+ y (- dy 1))))
          (when (and (not (and (= dx 1) (= dy 1)))  ;; Isključi samu ćeliju
                     (>= nx 0) (< nx (array-dimension grid 0))
                     (>= ny 0) (< ny (array-dimension grid 1))
                     (= (aref grid nx ny) 1))
            (incf count)))))
    count))

(defun evolve-grid (grid)
  "Stvaranje nove generacije mreže prema klasičnim pravilima Game of Life (B3/S23)."
  (let ((new-grid (create-grid (array-dimension grid 0) (array-dimension grid 1))))
    (dotimes (x (array-dimension grid 0))
      (dotimes (y (array-dimension grid 1))
        (let ((neighbors (count-neighbors grid x y)))
          (set-cell new-grid x y
                    (cond
                      ((= (aref grid x y) 1) ; Živa ćelija
                       (if (or (= neighbors 2) (= neighbors 3))
                           1 ; Ostaje živa
                           0)) ; Umire
                      ((= neighbors 3) ; Mrtva ćelija
                       1) ; Oživljava
                      (t 0)))))) ; Inače ostaje mrtva
    new-grid))

; Gotovi uztorci
(defun add-glider (grid x y)
  "Dodaje glider na mrežu s početkom na koordinatama X, Y, prilagođeno veličini mreže."
  (let ((glider-coordinates '((0 1) (1 2) (2 0) (2 1) (2 2)))) ; glider
    (dolist (offset glider-coordinates)
      (let* ((gx (+ x (first offset)))
             (gy (+ y (second offset))))
        ;; Postavi samo ako su koordinate unutar granica mreže
        (when (and (>= gx 0) (< gx (array-dimension grid 0))
                   (>= gy 0) (< gy (array-dimension grid 1)))
          (set-cell grid gx gy 1)))))) ; Postavi ćeliju na "živo"

(defun add-centered-glider (grid)
  "Dodaje glider u centar mreže."
  (let ((center-x (/ (array-dimension grid 0) 2))
        (center-y (/ (array-dimension grid 1) 2)))
    (add-glider grid (- center-x 1) (- center-y 1)))) ; Pomičemo za -1 da centriramo uzorak


(defun add-blinker (grid x y)
  "Dodaje Blinker uzorak na mrežu."
  (let ((blinker-coordinates '((0 0) (0 1) (0 2)))) ; blinker
    (dolist (offset blinker-coordinates)
      (let ((gx (+ x (first offset)))
            (gy (+ y (second offset))))
        (when (and (>= gx 0) (< gx (array-dimension grid 0))
                   (>= gy 0) (< gy (array-dimension grid 1)))
          (set-cell grid gx gy 1))))))

(defun add-centered-blinker (grid)
  "Dodaje Blinker u centar mreže."
  (let ((center-x (/ (array-dimension grid 0) 2))
        (center-y (/ (array-dimension grid 1) 2)))
    (add-blinker grid (- center-x 1) (- center-y 1)))) ; Postavi Blinker u centar


(defun add-r-pentomino (grid x y)
  "Dodaje R-pentomino na mrežu."
  (let ((r-pentomino-coordinates '((0 1) (0 2) (1 0) (1 1) (2 1)))) ; R-pentomino
    (dolist (offset r-pentomino-coordinates)
      (let ((gx (+ x (first offset)))
            (gy (+ y (second offset))))
        (when (and (>= gx 0) (< gx (array-dimension grid 0))
                   (>= gy 0) (< gy (array-dimension grid 1)))
          (set-cell grid gx gy 1))))))

(defun add-centered-r-pentomino (grid)
  "Dodaje R-pentomino u centar mreže."
  (let ((center-x (/ (array-dimension grid 0) 2))
        (center-y (/ (array-dimension grid 1) 2)))
    (add-r-pentomino grid (- center-x 1) (- center-y 2)))) ; Centriraj R-pentomino

(defun add-lwss (grid x y)
  "Dodaje Lightweight Spaceship na mrežu."
  (let ((lwss-coordinates '((0 0) (0 3) (1 4) (2 0) (2 4) (3 1) (3 2) (3 3) (3 4)))) ; lightweight spaceship
    (dolist (offset lwss-coordinates)
      (let ((gx (+ x (first offset)))
            (gy (+ y (second offset))))
        (when (and (>= gx 0) (< gx (array-dimension grid 0))
                   (>= gy 0) (< gy (array-dimension grid 1)))
          (set-cell grid gx gy 1))))))

(defun add-centered-lwss (grid)
  "Dodaje Lightweight Spaceship u centar mreže."
  (let ((center-x (/ (array-dimension grid 0) 2))
        (center-y (/ (array-dimension grid 1) 2)))
    (add-lwss grid (- center-x 2) (- center-y 2)))) ; Centriraj LWSS

(defun is-grid-empty (grid)
  "Provjerava je li mreža prazna."
  (loop for x from 0 below (array-dimension grid 0)
        do (loop for y from 0 below (array-dimension grid 1)
                 do (when (= (aref grid x y) 1)
                      (return-from is-grid-empty nil))))
  t)

(defun evolve-handler ()
  "Prikazuje sljedeću generaciju mreže."
  (setf *grid* (evolve-grid *grid*))
  (if (is-grid-empty *grid*)
      (progn
        (setf (hunchentoot:content-type*) "text/plain")
        "END") ; Vraća signal za kraj igre
      (progn
        (setf (hunchentoot:content-type*) "text/html")
        (grid-to-html *grid*)))) ; Inače vraća HTML mreže


;; Handleri za web server
(defun game-of-life-handler ()
  (format t "Pokušavam učitati HTML predložak...~%")
  (let ((template (load-html-template "C:/Users/laura/OneDrive/Radna površina/DP_projekt/index.html")))
    (format t "Predložak učitan. Zamjenjujem {{GRID}}...~%")
    (setf (hunchentoot:content-type*) "text/html")
    (let ((grid-html (grid-to-html *grid*)))
      (format t "HTML mreža generirana.~%")
      (cl-ppcre:regex-replace-all "\{\{GRID\}\}" template grid-html))))

(hunchentoot:define-easy-handler (grid-handler :uri "/grid") ()
  "Handler za dinamičko osvježavanje mreže."
  (setf (hunchentoot:content-type*) "text/html")
  (grid-to-html *grid*)) ; Vraća samo mrežu

(hunchentoot:define-easy-handler (resize-grid :uri "/resize-grid") (rows cols)
  "Handler za promjenu dimenzija mreže."
  (resize-grid-handler rows cols))

(hunchentoot:define-easy-handler (home :uri "/") ()
  (game-of-life-handler))

(hunchentoot:define-easy-handler (add-centered-glider-handler :uri "/add-centered-glider") ()
  "Handler za dodavanje glidera u centar mreže."
  (add-centered-glider *grid*)
  (setf (hunchentoot:content-type*) "text/html")
  (grid-to-html *grid*))

(hunchentoot:define-easy-handler (add-centered-blinker-handler :uri "/add-centered-blinker") ()
  "Handler za dodavanje centriranog Blinkera."
  (add-centered-blinker *grid*)
  (setf (hunchentoot:content-type*) "text/html")
  (grid-to-html *grid*))

(hunchentoot:define-easy-handler (add-centered-r-pentomino-handler :uri "/add-centered-r-pentomino") ()
  "Handler za dodavanje centriranog R-pentomina."
  (add-centered-r-pentomino *grid*)
  (setf (hunchentoot:content-type*) "text/html")
  (grid-to-html *grid*))

(hunchentoot:define-easy-handler (add-centered-lwss-handler :uri "/add-centered-lwss") ()
  "Handler za dodavanje centriranog Lightweight Spaceship."
  (add-centered-lwss *grid*)
  (setf (hunchentoot:content-type*) "text/html")
  (grid-to-html *grid*))

(hunchentoot:define-easy-handler (toggle-cell :uri "/toggle-cell") (row col)
  (toggle-cell-handler row col))

(hunchentoot:define-easy-handler (evolve :uri "/evolve") ()
  "Handler za sljedeću generaciju mreže."
  (evolve-handler))

(hunchentoot:define-easy-handler (clear-grid-handler :uri "/clear-grid") ()
  "Handler za čišćenje mreže."
  (clear-grid)
  (setf (hunchentoot:content-type*) "text/html")
  (grid-to-html *grid*)) ; Vraća HTML mreže

;; Server
(defvar *server* nil)

(defun start-server ()
  "Pokreće server na portu 8080."
  (if (and (boundp '*server*) *server*)
      (format t "Server već radi na http://localhost:8080~%")
      (progn
        (setf *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080)))
        (format t "Server pokrenut na http://localhost:8080~%"))))

(defun stop-server ()
  "Zaustavlja server."
  (when (and (boundp '*server*) *server*)
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "Server zaustavljen.~%")))

;; Pokreni server
(start-server)