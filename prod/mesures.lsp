;;; ============================================================
;;; mesures.lsp — Mesures rapides
;;; LONGT  : longueur totale des courbes sélectionnées
;;; SOMTXT : somme des valeurs numériques de textes
;;; ============================================================

(lb:enregistrer "LONGT" "Longueur totale des lignes/polylignes/arcs sélectionnés")
(lb:enregistrer "SOMTXT" "Somme des nombres contenus dans des textes")

(defun c:LONGT (/ *error* ss tot n l)
  (defun *error* (msg) (lb:fin msg))
  (lb:debut nil)
  (princ "\nSélectionner les objets à mesurer :")
  (if (setq ss (ssget '((0 . "LINE,ARC,CIRCLE,ELLIPSE,SPLINE,LWPOLYLINE,POLYLINE"))))
    (progn
      (setq tot 0.0 n 0)
      (foreach e (lb:ss->liste ss)
        (if (setq l (lb:longueur e))
          (setq tot (+ tot l) n (1+ n))
        )
      )
      (princ (strcat "\n" (itoa n) " objet(s) - longueur totale : "
                     (lb:fmt (* tot (lb:facteur-m)) 3) " m"
             )
      )
    )
  )
  (lb:fin nil)
)

(defun c:SOMTXT (/ *error* ss tot n ign v pt)
  (defun *error* (msg) (lb:fin msg))
  (lb:debut nil)
  (princ "\nSélectionner les textes à additionner :")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    (progn
      (setq tot 0.0 n 0 ign 0)
      (foreach e (lb:ss->liste ss)
        (if (setq v (lb:texte->nombre (lb:contenu e)))
          (setq tot (+ tot v) n (1+ n))
          (setq ign (1+ ign))
        )
      )
      (princ (strcat "\n" (itoa n) " valeur(s) - somme : " (lb:fmt tot 2)
                     (if (> ign 0) (strcat "  (" (itoa ign) " texte(s) sans nombre ignoré(s))") "")
             )
      )
      (if (setq pt (getpoint "\nPlacer le résultat <Non> : "))
        (lb:texte pt (lb:fmt tot 2) (getvar "TEXTSIZE") (getvar "CLAYER") T)
      )
    )
  )
  (lb:fin nil)
)

(princ)
