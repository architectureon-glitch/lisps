;;; ===========================================================================
;;;  ARRAYORBIT.LSP  -  Reseau orbital 3D hierarchique (inspire de ARRAYROT)
;;;
;;;  Commande : ARRAYORBIT   (raccourci : AORB)
;;;
;;;  On construit du plus interieur vers le plus exterieur
;;;  (Lune -> Terre -> Soleil). A chaque NIVEAU :
;;;    1. objets qui orbitent  (niveau 1 : la Lune ; niveau 2 : la Terre ...)
;;;    2. axe de rotation      (niveau 1 : axe de la Terre ; niveau 2 : axe du
;;;                             Soleil ...) : X/Y/Z + point, 2 points, ligne
;;;                             existante, ou Memoire (dernier axe utilise)
;;;    3. l'axe : FIXE ou SUIT la rotation
;;;         Fixe : les objets gardent leur orientation (translation sur l'orbite,
;;;                comme l'inclinaison de la Terre face aux etoiles)
;;;         Suit : tout l'ensemble pivote avec l'orbite (la Lune montre toujours
;;;                la meme face a la Terre)
;;;    4. nombre TOTAL d'elements (original + copies : 11 = original + 10)
;;;    5. angle total sur lequel les elements sont repartis
;;;  Puis : ajouter un niveau superieur ? L'ensemble deja construit (copies
;;;  comprises) est alors copie d'un bloc autour du nouvel axe.
;;;
;;;  - Reseau non associatif ; les objets selectionnes restent le 1er element
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
(or lb:*aorb-mode* (setq lb:*aorb-mode* "Fixe"))
(or lb:*aorb-nb*   (setq lb:*aorb-nb*   11))
(or lb:*aorb-fill* (setq lb:*aorb-fill* 360.0))

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

;;; --- Saisie de l'axe (resultat en SCG dans lb:*aorb-c* / lb:*aorb-a*) --------
(defun aorb:axe (lab / ok r c p1 p2 e d)
  (while (not ok)
    (initget "X Y Z Points Objet Memoire")
    (setq r (getkword (strcat "\nAxe de rotation " lab " [X/Y/Z/Points/Objet/Memoire] <Z> : ")))
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

(defun aorb:getnum (msg def kind bits / r)
  (initget bits)
  (setq r (cond ((= kind 'int)  (getint  (strcat "\n" msg " <" (itoa def) "> : ")))
                ((= kind 'dist) (getdist (strcat "\n" msg " <" (rtos def 2 4) "> : ")))
                (T              (getreal (strcat "\n" msg " <" (rtos def 2 4) "> : ")))))
  (if r r def))

;;; --- Un niveau : copie ALL (n-1 fois) autour de l'axe (C, A) ---------------------
;;; Retourne la liste des objets d'origine + toutes les copies.
(defun aorb:niveau (doc spc all c a suit base n stp / i cp phi d new)
  (setq i 1)
  (while (< i n)
    (setq phi (* i stp)
          cp  (aorb:copie doc spc all))
    (if suit
      ;; l'ensemble pivote autour de l'axe
      (foreach o cp (aorb:rot3 o c a phi))
      ;; orientation conservee : simple translation du point de base
      (progn
        (setq d (aorb:v- (aorb:rotpt base c a phi) base))
        (foreach o cp
          (vla-Move o (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point d)))))
    (setq new (append new cp)
          i   (1+ i)))
  (append all new))

(defun aorb:nom-corps (k)
  (nth (min (1- k) 2) '("la Lune" "la Terre" "le Soleil")))

(defun aorb:nom-axe (k)
  (nth (min (1- k) 2) '("l'axe de la Terre" "l'axe du Soleil" "l'axe central")))

;;; ===========================================================================
;;;  COMMANDE PRINCIPALE
;;; ===========================================================================
(defun c:ARRAYORBIT (/ *error* doc spc grp body hs ss db base c a suit n stp
                       k encore r rayon v)

  (defun *error* (msg) (lb:fin msg))
  (lb:debut '("CMDECHO"))

  (setq doc (lb:doc)
        spc (if (= 1 (getvar "CVPORT"))
              (vla-get-PaperSpace doc)
              (vla-get-ModelSpace doc))
        k 0
        encore T)

  (while encore
    (setq k (1+ k))
    (princ (strcat "\n--- Niveau " (itoa k) " ---"))

    ;; --- 1. Objets qui orbitent -----------------------------------------------
    (princ (strcat "\nSelectionnez les objets qui orbitent (ex. " (aorb:nom-corps k) ") :"))
    (if (not (setq ss (ssget "_:L"))) (exit))
    (setq hs   (mapcar 'vla-get-Handle grp)
          body (vl-remove-if '(lambda (o) (member (vla-get-Handle o) hs))
                             (aorb:ss->lst ss)))
    (if (not body)
      (progn (princ "\nCes objets font deja partie du reseau.") (exit)))
    (setq db   (aorb:defbase body)
          base (car db))

    ;; --- 2. Axe -------------------------------------------------------------------
    (aorb:axe (strcat "(ex. " (aorb:nom-axe k) ")"))
    (setq c lb:*aorb-c*
          a lb:*aorb-a*
          v (aorb:v- base c)
          rayon (sqrt (aorb:dot (aorb:v- v (aorb:v* (aorb:dot v a) a))
                                (aorb:v- v (aorb:v* (aorb:dot v a) a)))))
    (if (< rayon 1e-9)
      (princ "\nAttention : les objets sont sur l'axe (rayon nul), la translation sera nulle."))

    ;; --- 3. Fixe / Suit ----------------------------------------------------------------
    (initget "Fixe Suit")
    (setq lb:*aorb-mode*
           (cond ((getkword (strcat "\nL'axe [Fixe/Suit la rotation] <" lb:*aorb-mode* "> : ")))
                 (lb:*aorb-mode*))
          suit (= lb:*aorb-mode* "Suit"))

    ;; --- 4. Nombre total (original + copies) ---------------------------------------
    (setq n (aorb:getnum "Nombre total d'elements (original + copies)" lb:*aorb-nb* 'int 6))
    (while (> (* n (+ (length grp) (length body))) 20000)
      (princ "\nTrop d'objets (limite 20000).")
      (setq n (aorb:getnum "Nombre total d'elements (original + copies)" 2 'int 6)))
    (setq lb:*aorb-nb* n)
    (princ (strcat "  -> " (itoa (1- n)) " copie(s)"))

    ;; --- 5. Angle total -------------------------------------------------------------
    (setq lb:*aorb-fill* (aorb:getnum "Angle total de repartition en degres (+ = trigo, - = horaire)"
                                      lb:*aorb-fill* 'real 2)
          lb:*aorb-fill* (max -360.0 (min 360.0 lb:*aorb-fill*))
          stp (cond ((= n 1) 0.0)
                    ((equal (abs lb:*aorb-fill*) 360.0 1e-9)
                     (/ (* pi (/ lb:*aorb-fill* 180.0)) n))
                    (T (/ (* pi (/ lb:*aorb-fill* 180.0)) (1- n)))))

    ;; --- Construction du niveau ------------------------------------------------------
    (setq grp (aorb:niveau doc spc (append grp body) c a suit base n stp))

    ;; --- Niveau superieur ? ------------------------------------------------------------
    (initget "Oui Non")
    (setq r (getkword (strcat "\nAjouter un niveau superieur (ex. " (aorb:nom-corps (1+ k))
                              " autour de " (aorb:nom-axe (1+ k)) ") [Oui/Non] <Non> : "))
          encore (= r "Oui")))

  (princ (strcat "\nReseau orbital termine : " (itoa (length grp)) " objet(s) au total."))
  (lb:fin nil)
)

;;; --- Raccourci --------------------------------------------------------------
(defun c:AORB () (c:ARRAYORBIT))

(princ)
