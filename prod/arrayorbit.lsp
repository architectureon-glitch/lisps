;;; ===========================================================================
;;;  ARRAYORBIT.LSP  -  Reseau orbital 3D (inspire de ARRAYROT)
;;;
;;;  Commande : ARRAYORBIT   (raccourci : AORB)
;;;
;;;  Principe (Terre autour du Soleil, Lune autour de la Terre) :
;;;   - AXE ORBITAL predefini : X / Y / Z (du SCU) + point de passage,
;;;     2 points, ou une LIGNE existante. "Memoire" reprend le dernier axe.
;;;   - Les elements tournent autour de cet axe (angle a remplir, N elements).
;;;   - NIVEAUX : empilement le long de l'axe (pas + decalage angulaire).
;;;   - Decalage axial par element : helice / spirale.
;;;   - AXE FIXE  : l'orientation des elements ne change pas, l'axe de
;;;                 rotation propre reste parallele a lui-meme (la Terre
;;;                 garde son inclinaison face aux etoiles).
;;;     AXE SUIT  : l'orientation et l'axe propre tournent avec l'orbite
;;;                 (la Lune montre toujours la meme face a la Terre).
;;;   - ROTATION PROPRE : increment d'angle par element autour de l'axe propre,
;;;     incline d'un angle donne par rapport a l'axe orbital.
;;;   - SATELLITES : second jeu d'objets (la Lune) qui orbite autour de chaque
;;;     element ; position de depart = position relative dans le dessin.
;;;
;;;  - Reseau non associatif ; la selection d'origine devient le 1er element
;;;  - Un seul U annule tout le reseau
;;;  - Dernieres valeurs memorisees (Entree = valeur precedente)
;;; ===========================================================================

(vl-load-com)

;;; --- Mode autonome : si le noyau (core/lb-core.lsp) n'est pas charge, ---------
;;; --- definitions minimales des fonctions lb: utilisees ici -------------------
(if (not lb:enregistrer)
  (defun lb:enregistrer (cmd desc) cmd))

(if (not lb:doc)
  (defun lb:doc () (vla-get-ActiveDocument (vlax-get-acad-object))))

(if (not lb:debut)
  (defun lb:debut (vars)
    (setq lb:*sysvars* (mapcar '(lambda (v) (cons v (getvar v))) vars))
    (vla-EndUndoMark (lb:doc))
    (vla-StartUndoMark (lb:doc))
    (princ)))

(if (not lb:fin)
  (defun lb:fin (msg)
    (foreach p lb:*sysvars*
      (if (cdr p) (setvar (car p) (cdr p))))
    (setq lb:*sysvars* nil)
    (vla-EndUndoMark (lb:doc))
    (if (and msg
             (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*,*ANNUL*,*QUIT*")))
      (princ (strcat "\n** Erreur : " msg " **")))
    (princ)))

(lb:enregistrer "AORB""Reseau orbital 3D autour d'un axe (axe fixe/suit, niveaux, satellites)")

;;; --- Valeurs par defaut ----------------------------------------------------
(or lb:*aorb-mode*  (setq lb:*aorb-mode*  "Fixe"))
(or lb:*aorb-nb*    (setq lb:*aorb-nb*    12))
(or lb:*aorb-fill*  (setq lb:*aorb-fill*  360.0))
(or lb:*aorb-nl*    (setq lb:*aorb-nl*    1))
(or lb:*aorb-dz*    (setq lb:*aorb-dz*    1.0))
(or lb:*aorb-twist* (setq lb:*aorb-twist* 0.0))
(or lb:*aorb-hel*   (setq lb:*aorb-hel*   0.0))
(or lb:*aorb-spin*  (setq lb:*aorb-spin*  0.0))
(or lb:*aorb-tilt*  (setq lb:*aorb-tilt*  0.0))
(or lb:*aorb-nm*    (setq lb:*aorb-nm*    1))
(or lb:*aorb-mstep* (setq lb:*aorb-mstep* 30.0))

;;; --- Algebre vectorielle 3D --------------------------------------------------
(defun aorb:v+ (a b) (mapcar '+ a b))
(defun aorb:v- (a b) (mapcar '- a b))
(defun aorb:v* (k v) (mapcar '(lambda (x) (* k x)) v))
(defun aorb:dot (a b) (apply '+ (mapcar '* a b)))

(defun aorb:cross (a b)
  (list (- (* (cadr a) (caddr b)) (* (caddr a) (cadr b)))
        (- (* (caddr a) (car b)) (* (car a) (caddr b)))
        (- (* (car a) (cadr b)) (* (cadr a) (car b)))))

;; Vecteur unitaire (nil si vecteur nul)
(defun aorb:unit (v / l)
  (setq l (sqrt (aorb:dot v v)))
  (if (> l 1e-12) (aorb:v* (/ 1.0 l) v)))

;; Rotation du vecteur V d'un angle A autour de l'axe unitaire K (Rodrigues)
(defun aorb:rotvec (v k a)
  (aorb:v+ (aorb:v+ (aorb:v* (cos a) v)
                    (aorb:v* (sin a) (aorb:cross k v)))
           (aorb:v* (* (aorb:dot k v) (- 1.0 (cos a))) k)))

;; Rotation du point P autour de l'axe (C, K unitaire)
(defun aorb:rotpt (p c k a)
  (aorb:v+ c (aorb:rotvec (aorb:v- p c) k a)))

;; Vecteur unitaire perpendiculaire a l'axe A, dans le plan (A, V) si possible
(defun aorb:perp (a v / u)
  (setq u (aorb:unit (aorb:v- v (aorb:v* (aorb:dot v a) a))))
  (or u
      (aorb:unit (aorb:cross a (if (< (abs (car a)) 0.9) '(1.0 0.0 0.0) '(0.0 1.0 0.0))))))

;;; --- Saisie de l'axe (resultat en SCG dans lb:*aorb-c* / lb:*aorb-a*) --------
(defun aorb:axe (/ ok r c p1 p2 e d)
  (while (not ok)
    (initget "X Y Z Points Objet Memoire")
    (setq r (getkword "\nAxe de rotation [X/Y/Z/Points/Objet/Memoire] <Z> : "))
    (cond
      ;; X / Y / Z du SCU courant + point de passage
      ((or (null r) (member r '("X" "Y" "Z")))
       (initget 1)
       (setq c (getpoint "\nPoint de passage de l'axe : "))
       (setq lb:*aorb-c* (trans c 1 0)
             lb:*aorb-a* (aorb:unit
                           (trans (cond ((= r "X") '(1.0 0.0 0.0))
                                        ((= r "Y") '(0.0 1.0 0.0))
                                        (T '(0.0 0.0 1.0)))
                                  1 0 T))
             ok T))
      ;; 2 points
      ((= r "Points")
       (initget 1)
       (setq p1 (trans (getpoint "\nPremier point de l'axe : ") 1 0))
       (initget 1)
       (setq p2 (trans (getpoint (trans p1 0 1) "\nSecond point de l'axe : ") 1 0))
       (if (setq d (aorb:unit (aorb:v- p2 p1)))
         (setq lb:*aorb-c* p1 lb:*aorb-a* d ok T)
         (princ "\nPoints confondus.")))
      ;; Ligne existante
      ((= r "Objet")
       (if (and (setq e (entsel "\nSelectionnez une ligne : "))
                (= "LINE" (cdr (assoc 0 (setq e (entget (car e)))))))
         (if (setq d (aorb:unit (aorb:v- (cdr (assoc 11 e)) (cdr (assoc 10 e)))))
           (setq lb:*aorb-c* (cdr (assoc 10 e)) lb:*aorb-a* d ok T)
           (princ "\nLigne de longueur nulle."))
         (princ "\nObjet non valide : une LIGNE est attendue.")))
      ;; Dernier axe utilise
      ((= r "Memoire")
       (if (and lb:*aorb-c* lb:*aorb-a*)
         (setq ok T)
         (princ "\nAucun axe en memoire.")))))
  T)

;;; --- Utilitaires repris d'ARRAYROT -------------------------------------------
(defun aorb:ss->lst (ss / i l)
  (repeat (setq i (sslength ss))
    (setq l (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) l)))
  l)

(defun aorb:centre (objs / a b mn mx)
  (foreach o objs
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply 'vla-GetBoundingBox (list o 'a 'b))))
      (setq a  (vlax-safearray->list a)
            b  (vlax-safearray->list b)
            mn (if mn (mapcar 'min mn a) a)
            mx (if mx (mapcar 'max mx b) b))))
  (if mn (mapcar '(lambda (u v) (/ (+ u v) 2.0)) mn mx)))

(defun aorb:defbase (objs / o)
  (setq o (car objs))
  (if (and (= 1 (length objs))
           (= "AcDbBlockReference" (vla-get-ObjectName o)))
    (list (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o)))
          "Insertion")
    (list (aorb:centre objs) "Centre")))

(defun aorb:pointbase (msg def lab / p)
  (initget 0)
  (setq p (getpoint (strcat "\n" msg " <" lab "> : ")))
  (if p (trans p 1 0) def))

(defun aorb:copie (doc spc objs)
  (vlax-safearray->list
    (vlax-variant-value
      (vla-CopyObjects doc
        (vlax-make-variant
          (vlax-safearray-fill
            (vlax-make-safearray vlax-vbObject (cons 0 (1- (length objs))))
            objs))
        spc))))

;;; --- Rotation 3D d'un objet autour de la droite (PT, PT + DIR) ---------------
(defun aorb:rot3 (o pt dir ang)
  (if (not (equal ang 0.0 1e-10))
    (vla-Rotate3D o (vlax-3d-point pt) (vlax-3d-point (aorb:v+ pt dir)) ang)))

;;; --- Place les objets : BP -> PT, orientation autour de OAX, rotation propre -
(defun aorb:placer (objs bp pt oax oang sax sang / p1 p2)
  (setq p1 (vlax-3d-point bp)
        p2 (vlax-3d-point pt))
  (foreach o objs
    (if (not (equal bp pt 1e-9)) (vla-Move o p1 p2))
    (aorb:rot3 o pt oax oang)
    (aorb:rot3 o pt sax sang)))

(defun aorb:getnum (msg def kind bits / r)
  (initget bits)
  (setq r (cond ((= kind 'int)  (getint  (strcat "\n" msg " <" (itoa def) "> : ")))
                ((= kind 'dist) (getdist (strcat "\n" msg " <" (rtos def 2 4) "> : ")))
                (T              (getreal (strcat "\n" msg " <" (rtos def 2 4) "> : ")))))
  (if r r def))

;;; ===========================================================================
;;;  COMMANDE PRINCIPALE
;;; ===========================================================================
(defun c:ARRAYORBIT (/ *error* doc spc ss ssm objs mobjs db base bm n tot stp nl dz
                       twist hel spin tilt nm mstep c a suit u s0 offv off
                       i j k l phi theta pt pm sdir total)

  (defun *error* (msg) (lb:fin msg))
  (lb:debut '("CMDECHO"))

  (setq doc (lb:doc)
        spc (if (= 1 (getvar "CVPORT"))
              (vla-get-PaperSpace doc)
              (vla-get-ModelSpace doc)))

  ;; --- 1. Objets ------------------------------------------------------------
  (princ "\nSelectionnez les objets a mettre en reseau (la Terre) :")
  (if (not (setq ss (ssget "_:L"))) (exit))
  (setq objs (aorb:ss->lst ss))

  ;; --- 2. Axe orbital ---------------------------------------------------------
  (aorb:axe)
  (setq c lb:*aorb-c*
        a lb:*aorb-a*)

  ;; --- 3. Mode de l'axe -------------------------------------------------------
  (initget "Fixe Suit")
  (setq lb:*aorb-mode*
         (cond ((getkword (strcat "\nL'axe [Fixe/Suit la rotation] <" lb:*aorb-mode* "> : ")))
               (lb:*aorb-mode*))
        suit (= lb:*aorb-mode* "Suit"))

  ;; --- 4. Orbite ----------------------------------------------------------------
  (setq lb:*aorb-nb* (aorb:getnum "Nombre d'elements par orbite" lb:*aorb-nb* 'int 6)
        n            lb:*aorb-nb*)
  (setq lb:*aorb-fill* (aorb:getnum "Angle a remplir en degres (+ = trigo, - = horaire)"
                                    lb:*aorb-fill* 'real 2)
        lb:*aorb-fill* (max -360.0 (min 360.0 lb:*aorb-fill*))
        tot (* pi (/ lb:*aorb-fill* 180.0))
        stp (cond ((= n 1) 0.0)
                  ((equal (abs lb:*aorb-fill*) 360.0 1e-9) (/ tot n))
                  (T (/ tot (1- n)))))

  ;; --- 5. Niveaux (3D) et helice ------------------------------------------------
  (setq lb:*aorb-nl* (aorb:getnum "Nombre de niveaux le long de l'axe" lb:*aorb-nl* 'int 6)
        nl           lb:*aorb-nl*)
  (if (> nl 1)
    (setq lb:*aorb-dz*    (aorb:getnum "Pas entre niveaux (+/- selon le sens de l'axe)"
                                       lb:*aorb-dz* 'dist 0)
          lb:*aorb-twist* (aorb:getnum "Decalage angulaire entre niveaux (degres)"
                                       lb:*aorb-twist* 'real 0)))
  (setq lb:*aorb-hel* (aorb:getnum "Decalage axial par element (helice)" lb:*aorb-hel* 'dist 0)
        dz    (if (> nl 1) lb:*aorb-dz* 0.0)
        twist (if (> nl 1) (* pi (/ lb:*aorb-twist* 180.0)) 0.0)
        hel   lb:*aorb-hel*)

  ;; --- 6. Rotation propre -----------------------------------------------------
  (setq lb:*aorb-spin* (aorb:getnum "Rotation propre par element (degres, 0 = aucune)"
                                    lb:*aorb-spin* 'real 0)
        spin (* pi (/ lb:*aorb-spin* 180.0)))
  (if (/= spin 0.0)
    (setq lb:*aorb-tilt* (aorb:getnum "Inclinaison de l'axe propre / axe orbital (degres)"
                                      lb:*aorb-tilt* 'real 0)))
  (setq tilt (if (/= spin 0.0) (* pi (/ lb:*aorb-tilt* 180.0)) 0.0))

  ;; --- 7. Point de base (definit le rayon d'orbite) --------------------------------
  (setq db   (aorb:defbase objs)
        base (aorb:pointbase "Point de base des objets (distance a l'axe = rayon)"
                             (car db) (cadr db)))
  (if (< (distance base (aorb:rotpt base c a (/ pi 2.0))) 1e-9)
    (princ "\nAttention : point de base sur l'axe, rayon d'orbite nul."))

  ;; --- 8. Satellites (la Lune) --------------------------------------------------
  (princ "\nSelectionnez les satellites (la Lune) <Aucun> :")
  (if (setq ssm (ssget "_:L"))
    (progn
      (setq mobjs (aorb:ss->lst ssm)
            db    (aorb:defbase mobjs)
            bm    (aorb:pointbase "Point de base des satellites" (car db) (cadr db))
            lb:*aorb-nm* (aorb:getnum "Nombre de satellites par element" lb:*aorb-nm* 'int 6)
            nm    lb:*aorb-nm*
            lb:*aorb-mstep* (aorb:getnum "Avance du satellite par element (degres)"
                                         lb:*aorb-mstep* 'real 0)
            mstep (* pi (/ lb:*aorb-mstep* 180.0))
            offv  (aorb:v- bm base))))

  (setq total (* n nl (if mobjs (1+ nm) 1)))
  (if (> total 10000)
    (progn (princ (strcat "\nTrop d'elements (" (itoa total) " > 10000).")) (exit)))

  ;; --- 9. Construction : copies d'abord, sources (indice 0) en dernier ----------
  ;; axe propre initial : axe orbital incline de TILT vers la direction du rayon
  (setq u  (aorb:perp a (aorb:v- base c))
        s0 (aorb:v+ (aorb:v* (cos tilt) a) (aorb:v* (sin tilt) u)))

  (setq l nl)
  (while (>= (setq l (1- l)) 0)
    (setq i n)
    (while (>= (setq i (1- i)) 0)
      (setq k    (+ (* l n) i)
            phi  (+ (* i stp) (* l twist))
            pt   (aorb:v+ (aorb:rotpt base c a phi)
                          (aorb:v* (+ (* l dz) (* i hel)) a))
            sdir (if suit (aorb:rotvec s0 a phi) s0))

      ;; satellites de cet element
      (if mobjs
        (progn
          (setq off (if suit (aorb:rotvec offv a phi) offv)
                j   nm)
          (while (>= (setq j (1- j)) 0)
            (setq theta (+ (* j (/ (* 2.0 pi) nm)) (* k mstep))
                  pm    (aorb:v+ pt (aorb:rotvec off a theta)))
            (aorb:placer (if (and (zerop k) (zerop j)) mobjs (aorb:copie doc spc mobjs))
                         bm pm a (if suit (+ phi theta) 0.0) a 0.0))))

      ;; l'element lui-meme
      (aorb:placer (if (zerop k) objs (aorb:copie doc spc objs))
                   base pt a (if suit phi 0.0) sdir (* k spin))))

  (princ (strcat "\n" (itoa total) " groupe(s) d'objets en reseau orbital 3D."))
  (lb:fin nil)
)

;;; --- Raccourci --------------------------------------------------------------
(defun c:AORB () (c:ARRAYORBIT))

(princ)
