;;; ============================================================
;;; lb-core.lsp — Noyau de la bibliothèque
;;; Fonctions utilitaires partagées par toutes les commandes.
;;; Préfixe des fonctions : lb:   Variables globales : lb:*nom*
;;; ============================================================

(vl-load-com)

;; Séparateur décimal utilisé à l'affichage ("," ou ".")
(if (not lb:*sep-dec*) (setq lb:*sep-dec* ","))

;; ------------------------------------------------------------
;; Document actif
;; ------------------------------------------------------------
(defun lb:doc () (vla-get-ActiveDocument (vlax-get-acad-object)))

;; ------------------------------------------------------------
;; Début / fin de commande : sauvegarde des variables système,
;; marque d'annulation (un seul U annule toute la commande),
;; et gestion propre des erreurs / Échap.
;;
;; Utilisation type :
;;   (defun c:MACMD (/ *error*)
;;     (defun *error* (msg) (lb:fin msg))
;;     (lb:debut '("CMDECHO" "OSMODE"))
;;     ...
;;     (lb:fin nil))
;; ------------------------------------------------------------
(defun lb:debut (vars)
  (setq lb:*sysvars* (mapcar '(lambda (v) (cons v (getvar v))) vars))
  (vla-EndUndoMark (lb:doc))
  (vla-StartUndoMark (lb:doc))
  (princ)
)

(defun lb:fin (msg)
  (foreach p lb:*sysvars*
    (if (cdr p) (setvar (car p) (cdr p)))
  )
  (setq lb:*sysvars* nil)
  (vla-EndUndoMark (lb:doc))
  (if (and msg
           (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*,*ANNUL*,*QUIT*"))
      )
    (princ (strcat "\n** Erreur : " msg " **"))
  )
  (princ)
)

;; ------------------------------------------------------------
;; Registre des commandes (affiché par LBAIDE)
;; ------------------------------------------------------------
(defun lb:enregistrer (cmd desc)
  (setq lb:*commandes*
         (cons (cons cmd desc)
               (vl-remove-if '(lambda (x) (= (car x) cmd)) lb:*commandes*)
         )
  )
  cmd
)

(defun lb:pad (s n)
  (while (< (strlen s) n) (setq s (strcat s " ")))
  s
)

(defun c:LBAIDE (/)
  (princ "\n--- Bibliothèque LISP : commandes disponibles ---")
  (foreach c (vl-sort lb:*commandes* '(lambda (a b) (< (car a) (car b))))
    (princ (strcat "\n  " (lb:pad (car c) 10) (cdr c)))
  )
  (princ)
)

;; ------------------------------------------------------------
;; Nombres et unités
;; ------------------------------------------------------------

;; Nombre -> texte avec PREC décimales (zéros conservés) et
;; séparateur décimal lb:*sep-dec*.  (lb:fmt 12.5 2) -> "12,50"
(defun lb:fmt (n prec / dz s)
  (setq dz (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)
  (setq s (rtos n 2 prec))
  (setvar "DIMZIN" dz)
  (if (= s (strcat "-" (rtos 0.0 2 prec))) (setq s (substr s 2)))
  (if (/= lb:*sep-dec* ".") (setq s (vl-string-subst lb:*sep-dec* "." s)))
  s
)

;; Niveau architectural : +2,50 / -0,30 / ±0,00
(defun lb:fmt-niveau (z prec)
  (cond
    ((equal z 0.0 (* 0.5 (expt 10.0 (- prec)))) (strcat "%%p" (lb:fmt 0.0 prec)))
    ((> z 0.0) (strcat "+" (lb:fmt z prec)))
    (T (strcat "-" (lb:fmt (abs z) prec)))
  )
)

;; Facteur de conversion : unité du dessin -> mètre (d'après INSUNITS).
;; Si INSUNITS n'est pas une unité métrique/impériale connue,
;; on suppose que le dessin est en mètres.
(defun lb:facteur-m (/ f)
  (setq f (cdr (assoc (getvar "INSUNITS")
                      '((1 . 0.0254) (2 . 0.3048) (4 . 0.001)
                        (5 . 0.01) (6 . 1.0) (14 . 0.1))
               )
          )
  )
  (if (not f)
    (progn
      (princ "\n(INSUNITS non défini : unité du dessin supposée = mètre)")
      (setq f 1.0)
    )
  )
  f
)

;; Extrait le premier nombre d'une chaîne ("S = 12,35 m²" -> 12.35).
;; Accepte "," ou "." comme séparateur décimal. Renvoie nil si aucun.
(defun lb:texte->nombre (s / l c buf fini)
  (setq l (vl-string->list s) buf "")
  (while (and l (not fini))
    (setq c (chr (car l)))
    (cond
      ((wcmatch c "#") (setq buf (strcat buf c)))
      ((and (member c '("," "."))
            (/= buf "")
            (/= buf "-")
            (not (vl-string-search "." buf))
       )
       (setq buf (strcat buf "."))
      )
      ((and (= c "-") (= buf "")) (setq buf "-"))
      ((= buf "-") (setq buf ""))
      ((/= buf "") (setq fini T))
    )
    (setq l (cdr l))
  )
  (if (and (/= buf "") (/= buf "-")) (atof buf))
)

;; Retire les codes de mise en forme d'un texte MTEXT
;; ("{\\fArial|b0;12,5}\\Pm²" -> "12,5 m²")
(defun lb:mtext-brut (s / res i n c)
  (setq res "" i 1 n (strlen s))
  (while (<= i n)
    (setq c (substr s i 1))
    (cond
      ((= c "\\")
       (setq i (1+ i) c (substr s i 1))
       (cond
         ((wcmatch c "[fFcCHhTtQqWwAapS]")   ; codes avec argument terminé par ;
          (while (and (<= i n) (/= (substr s i 1) ";")) (setq i (1+ i)))
         )
         ((wcmatch c "[PN]") (setq res (strcat res " ")))
         ((wcmatch c "[LlOoKk]"))            ; soulignage, surlignage, barré
         (T (setq res (strcat res c)))       ; caractères échappés \\ \{ \}
       )
      )
      ((wcmatch c "[{}]"))
      (T (setq res (strcat res c)))
    )
    (setq i (1+ i))
  )
  res
)

;; Contenu texte "lisible" d'un TEXT / MTEXT / ATTRIB
(defun lb:contenu (e / o)
  (setq o (vlax-ename->vla-object e))
  (if (= (vla-get-ObjectName o) "AcDbMText")
    (lb:mtext-brut (vla-get-TextString o))
    (vla-get-TextString o)
  )
)

;; ------------------------------------------------------------
;; Saisies avec valeur par défaut (Entrée = défaut)
;; ------------------------------------------------------------
(defun lb:getdist-def (msg def / r)
  (setq r (getdist (strcat msg " <" (lb:fmt def 2) "> : ")))
  (if r r def)
)

(defun lb:getint-def (msg def / r)
  (setq r (getint (strcat msg " <" (itoa def) "> : ")))
  (if r r def)
)

(defun lb:getstring-def (msg def / r)
  (setq r (getstring T (strcat msg " <" def "> : ")))
  (if (= r "") def r)
)

;; ------------------------------------------------------------
;; Sélections
;; ------------------------------------------------------------
(defun lb:ss->liste (ss / i l)
  (if ss
    (repeat (setq i (sslength ss))
      (setq l (cons (ssname ss (setq i (1- i))) l))
    )
  )
  l
)

;; Longueur d'une courbe (nil si l'objet n'est pas une courbe)
(defun lb:longueur (e / r)
  (setq r (vl-catch-all-apply
            '(lambda () (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
          )
  )
  (if (not (vl-catch-all-error-p r)) r)
)

;; ------------------------------------------------------------
;; Création d'objets
;; ------------------------------------------------------------

;; Crée le calque s'il n'existe pas, renvoie son nom
(defun lb:calque (nom couleur)
  (if (not (tblsearch "LAYER" nom))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 nom)
                   '(70 . 0)
                   (cons 62 couleur)
                   '(6 . "Continuous")
             )
    )
  )
  nom
)

;; Texte sur une ligne. PT en coordonnées SCU courant.
;; CENTRE = T : centré milieu sur PT ; nil : aligné à gauche.
(defun lb:texte (pt txt haut calque centre / p)
  (setq p (trans pt 1 0))
  (entmakex
    (append
      (list '(0 . "TEXT")
            (cons 8 calque)
            (cons 10 p)
            (cons 40 haut)
            (cons 1 txt)
            (cons 7 (getvar "TEXTSTYLE"))
      )
      (if centre
        (list '(72 . 1) '(73 . 2) (cons 11 p))
        '((72 . 0) (73 . 0))
      )
    )
  )
)

(princ)
