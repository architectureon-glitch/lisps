;;; ===========================================================================
;;;  ARRAYORBIT.LSP  -  Reseau orbital 3D : Lune -> Terre -> Soleil
;;;  (inspire de ARRAYROT)
;;;
;;;  Commande : ARRAYORBIT   (raccourci : AORB)
;;;
;;;  L'ensemble TERRE + LUNE est copie N fois autour de l'axe du SOLEIL.
;;;  Pour la copie numero i (0 = original, puis 1, 2 ... N-1) :
;;;    - la Lune a tourne de  i x (angle Lune)  autour de son axe
;;;      (son orbite autour de la Terre) ;
;;;    - la Terre a tourne de i x (angle Terre) sur son propre axe ;
;;;    - l'ensemble Terre + Lune est place a i x (pas) autour de l'axe du
;;;      Soleil, le pas venant de l'angle total et du nombre d'elements.
;;;
;;;  Deroulement (de la Lune vers le Soleil) :
;;;    LUNE   : objets (Entree = pas de Lune), axe, Fixe/Suit, angle par copie
;;;    TERRE  : objets, axe, Fixe/Suit, angle par copie
;;;    SOLEIL : axe, nombre total (original + copies), angle total
;;;
;;;  Axes : X/Y/Z du SCU + point de passage, 2 points, ligne existante,
;;;         Lune (la Terre reprend l'axe de la Lune), Memoire (dernier axe).
;;;
;;;  Fixe / Suit :
;;;    Lune   Fixe : la Lune garde son orientation en tournant autour de la Terre
;;;           Suit : la Lune pivote avec son orbite (meme face vers la Terre)
;;;    Terre  Fixe : l'axe de la Terre reste parallele a lui-meme autour du
;;;                  Soleil (comme l'inclinaison reelle de la Terre)
;;;           Suit : l'ensemble Terre + Lune pivote avec l'orbite du Soleil
;;;
;;;  - 11 elements = original + 10 copies
;;;  - Reseau non associatif ; les objets selectionnes restent l'element 0
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

(lb:enregistrer "AORB" "Terre + Lune copiees autour du Soleil (Lune et Terre tournent a chaque copie)")

;;; --- Valeurs par defaut ----------------------------------------------------
(or lb:*aorb-mode-lune*  (setq lb:*aorb-mode-lune*  "Suit"))
(or lb:*aorb-mode-terre* (setq lb:*aorb-mode-terre* "Fixe"))
(or lb:*aorb-rot-lune*   (setq lb:*aorb-rot-lune*   30.0))
(or lb:*aorb-rot-terre*  (setq lb:*aorb-rot-terre*  0.0))
(or lb:*aorb-nb*         (setq lb:*aorb-nb*         11))
(or lb:*aorb-fill*       (setq lb:*aorb-fill*       360.0))

;;; --- Algebre vectorielle 3D --------------------------------------------------
(defun aorb:v+ (a b) (mapcar '+ a b))
(defun aorb:v- (a b) (mapcar '- a b))
(defun aorb:v* (k v) (mapcar '(lambda (x) (* k x)) v))
(defun aorb:dot (a b) (apply '+ (mapcar '* a b)))
(defun aorb:rad (deg) (* pi (/ deg 180.0)))

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

;;; --- Saisie d'un axe -> (point direction) en SCG -----------------------------
;;;  LAB   : texte de la question
;;;  DEFPT : point de passage par defaut (SCG) pour X/Y/Z, ou nil
;;;  MEM   : dernier axe de ce role (option Memoire), ou nil
;;;  ALT   : axe de la Lune (option Lune), ou nil
(defun aorb:axe (lab defpt mem alt / ok r c p1 p2 e d res)
  (while (not ok)
    (initget (strcat "X Y Z Points Objet" (if alt " Lune" "") (if mem " Memoire" "")))
    (setq r (getkword (strcat "\n" lab " [X/Y/Z/Points/Objet"
                              (if alt "/Lune" "") (if mem "/Memoire" "") "] <Z> : ")))
    (cond
      ;; X / Y / Z du SCU courant + point de passage
      ((or (null r) (member r '("X" "Y" "Z")))
       (if defpt
         (setq c (getpoint "\nPoint de passage de l'axe <centre de l'objet> : "))
         (progn
           (initget 1)
           (setq c (getpoint "\nPoint de passage de l'axe : "))))
       (setq res (list (if c (trans c 1 0) defpt)
                       (aorb:unit
                         (trans (cond ((= r "X") '(1.0 0.0 0.0))
                                      ((= r "Y") '(0.0 1.0 0.0))
                                      (T '(0.0 0.0 1.0)))
                                1 0 T)))
             ok T))
      ;; 2 points
      ((= r "Points")
       (initget 1)
       (setq p1 (trans (getpoint "\nPremier point de l'axe : ") 1 0))
       (initget 1)
       (setq p2 (trans (getpoint (trans p1 0 1) "\nSecond point de l'axe : ") 1 0))
       (if (setq d (aorb:unit (aorb:v- p2 p1)))
         (setq res (list p1 d) ok T)
         (princ "\nPoints confondus.")))
      ;; Ligne existante
      ((= r "Objet")
       (if (and (setq e (entsel "\nSelectionnez une ligne : "))
                (= "LINE" (cdr (assoc 0 (setq e (entget (car e)))))))
         (if (setq d (aorb:unit (aorb:v- (cdr (assoc 11 e)) (cdr (assoc 10 e)))))
           (setq res (list (cdr (assoc 10 e)) d) ok T)
           (princ "\nLigne de longueur nulle."))
         (princ "\nObjet non valide : une LIGNE est attendue.")))
      ;; Meme axe que la Lune
      ((= r "Lune") (setq res alt ok T))
      ;; Dernier axe utilise pour ce role
      ((= r "Memoire") (setq res mem ok T))))
  res)

;;; --- Question Fixe / Suit ------------------------------------------------------
(defun aorb:mode (msg def)
  (initget "Fixe Suit")
  (cond ((getkword (strcat "\n" msg " [Fixe/Suit la rotation] <" def "> : ")))
        (def)))

;;; --- Saisie numerique avec valeur par defaut -------------------------------------
(defun aorb:getnum (msg def kind bits / r)
  (initget bits)
  (setq r (if (= kind 'int)
            (getint  (strcat "\n" msg " <" (itoa def) "> : "))
            (getreal (strcat "\n" msg " <" (rtos def 2 2) "> : "))))
  (if r r def))

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

;; Point de base : insertion si bloc unique, sinon centre de l'emprise
(defun aorb:base (objs / o)
  (setq o (car objs))
  (if (and (= 1 (length objs))
           (= "AcDbBlockReference" (vla-get-ObjectName o)))
    (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint o)))
    (aorb:centre objs)))

(defun aorb:copie (doc spc objs)
  (vlax-safearray->list
    (vlax-variant-value
      (vla-CopyObjects doc
        (vlax-make-variant
          (vlax-safearray-fill
            (vlax-make-safearray vlax-vbObject (cons 0 (1- (length objs))))
            objs))
        spc))))

;;; --- Transformations -----------------------------------------------------------

;; Rotation 3D d'un objet autour de la droite (PT, PT + DIR)
(defun aorb:rot3 (o pt dir ang)
  (if (not (equal ang 0.0 1e-12))
    (vla-Rotate3D o (vlax-3d-point pt) (vlax-3d-point (aorb:v+ pt dir)) ang)))

;; Fait tourner OBJS autour de l'axe AXE = (point direction) d'un angle ANG.
;;  SUIT = T   : les objets pivotent (rotation rigide autour de l'axe)
;;  SUIT = nil : orientation conservee, translation du point de base B
(defun aorb:orbite (objs b axe ang suit / d p0 p1)
  (if (and objs (not (equal ang 0.0 1e-12)))
    (if suit
      (foreach o objs (aorb:rot3 o (car axe) (cadr axe) ang))
      (progn
        (setq d  (aorb:v- (aorb:rotpt b (car axe) (cadr axe) ang) b)
              p0 (vlax-3d-point '(0.0 0.0 0.0))
              p1 (vlax-3d-point d))
        (foreach o objs (vla-Move o p0 p1))))))

;;; ===========================================================================
;;;  COMMANDE PRINCIPALE
;;; ===========================================================================
(defun c:ARRAYORBIT (/ *error* doc spc ss lune terre hs bl bt axl axt axs
                       suitl suitt al at n stp i cl ct)

  (defun *error* (msg) (lb:fin msg))
  (lb:debut '("CMDECHO"))

  (setq doc (lb:doc)
        spc (if (= 1 (getvar "CVPORT"))
              (vla-get-PaperSpace doc)
              (vla-get-ModelSpace doc)))

  ;; ============================ LUNE ===========================================
  (princ "\n=== LUNE ===")
  (princ "\nSelectionnez la Lune <Aucune> :")
  (if (setq ss (ssget "_:L"))
    (progn
      (setq lune (aorb:ss->lst ss)
            bl   (aorb:base lune)
            axl  (aorb:axe "Axe de la Lune (son orbite autour de la Terre)"
                           nil lb:*aorb-axe-lune* nil)
            lb:*aorb-axe-lune* axl
            lb:*aorb-mode-lune* (aorb:mode "La Lune" lb:*aorb-mode-lune*)
            suitl (= lb:*aorb-mode-lune* "Suit")
            lb:*aorb-rot-lune* (aorb:getnum "Rotation de la Lune autour de la Terre, par copie (degres)"
                                            lb:*aorb-rot-lune* 'real 0)
            al   (aorb:rad lb:*aorb-rot-lune*))))

  ;; ============================ TERRE ==========================================
  (princ "\n=== TERRE ===")
  (princ "\nSelectionnez la Terre :")
  (if (not (setq ss (ssget "_:L"))) (exit))
  (setq hs    (mapcar 'vla-get-Handle lune)
        terre (vl-remove-if '(lambda (o) (member (vla-get-Handle o) hs))
                            (aorb:ss->lst ss)))
  (if (not terre)
    (progn (princ "\nLa Terre doit contenir d'autres objets que la Lune.") (exit)))
  (setq bt  (aorb:base terre)
        axt (aorb:axe "Axe de la Terre" bt lb:*aorb-axe-terre* axl)
        lb:*aorb-axe-terre* axt
        lb:*aorb-mode-terre* (aorb:mode "L'axe de la Terre autour du Soleil" lb:*aorb-mode-terre*)
        suitt (= lb:*aorb-mode-terre* "Suit")
        lb:*aorb-rot-terre* (aorb:getnum "Rotation de la Terre sur son axe, par copie (degres, 0 = aucune)"
                                         lb:*aorb-rot-terre* 'real 0)
        at  (aorb:rad lb:*aorb-rot-terre*))

  ;; ============================ SOLEIL =========================================
  (princ "\n=== SOLEIL ===")
  (setq axs (aorb:axe "Axe du Soleil" nil lb:*aorb-axe-soleil* nil)
        lb:*aorb-axe-soleil* axs)

  (setq n (aorb:getnum "Nombre total Terre+Lune (original + copies)" lb:*aorb-nb* 'int 6))
  (while (> (* (1- n) (+ (length terre) (length lune))) 20000)
    (princ "\nTrop d'objets a creer (limite 20000).")
    (setq n (aorb:getnum "Nombre total Terre+Lune (original + copies)" 2 'int 6)))
  (setq lb:*aorb-nb* n)
  (princ (strcat "  -> original + " (itoa (1- n)) " copie(s)"))

  (setq lb:*aorb-fill* (aorb:getnum "Angle total autour du Soleil en degres (+ = trigo, - = horaire)"
                                    lb:*aorb-fill* 'real 2)
        lb:*aorb-fill* (max -360.0 (min 360.0 lb:*aorb-fill*))
        stp (cond ((= n 1) 0.0)
                  ((equal (abs lb:*aorb-fill*) 360.0 1e-9) (/ (aorb:rad lb:*aorb-fill*) n))
                  (T (/ (aorb:rad lb:*aorb-fill*) (1- n)))))

  ;; ============================ CONSTRUCTION ===================================
  ;; L'original (i = 0) ne bouge pas ; copies i = 1 ... n-1
  (setq i 1)
  (while (< i n)
    (setq cl (if lune (aorb:copie doc spc lune))
          ct (aorb:copie doc spc terre))
    ;; 1. la Lune tourne autour de la Terre
    (aorb:orbite cl bl axl (* i al) suitl)
    ;; 2. la Terre tourne sur son axe
    (foreach o ct (aorb:rot3 o (car axt) (cadr axt) (* i at)))
    ;; 3. l'ensemble Terre + Lune tourne autour du Soleil
    (aorb:orbite (append ct cl) bt axs (* i stp) suitt)
    (setq i (1+ i)))

  (princ (strcat "\n" (itoa n) " Terre(s)" (if lune "+Lune" "")
                 " autour du Soleil (original + " (itoa (1- n)) " copie(s))."))
  (lb:fin nil)
)

;;; --- Raccourci --------------------------------------------------------------
(defun c:AORB () (c:ARRAYORBIT))

(princ)
