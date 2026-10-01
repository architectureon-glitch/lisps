;;; ============================================================
;;; topo.lsp — Altitudes et pentes
;;; ALTI  : cote d'altitude (Z) aux points cliqués (+12,35 / ±0,00)
;;; PENTE : pente en % entre deux points 3D
;;; ============================================================

(lb:enregistrer "ALTI" "Cote d'altitude Z aux points cliqués")
(lb:enregistrer "PENTE" "Pente en % entre deux points 3D")

(defun c:ALTI (/ *error* fac haut cal pt z)
  (defun *error* (msg) (lb:fin msg))
  (lb:debut nil)

  (setq fac  (lb:facteur-m)
        haut (lb:getdist-def "\nHauteur du texte"
                             (if lb:*alti-haut* lb:*alti-haut* (getvar "TEXTSIZE"))
             )
        lb:*alti-haut* haut
        cal  (lb:calque "T-ALTITUDES" 1)
  )

  (while (setq pt (getpoint "\nPoint à coter <Fin> : "))
    (setq z (* fac (caddr (trans pt 1 0))))
    (entmake (list '(0 . "POINT") (cons 8 cal) (cons 10 (trans pt 1 0))))
    (lb:texte (list (+ (car pt) (* 0.5 haut)) (+ (cadr pt) (* 0.5 haut)) (caddr pt))
              (lb:fmt-niveau z 2) haut cal nil
    )
  )
  (lb:fin nil)
)

(defun c:PENTE (/ *error* p1 p2 w1 w2 dh dz pc txt)
  (defun *error* (msg) (lb:fin msg))
  (lb:debut nil)

  (if (and (setq p1 (getpoint "\nPremier point : "))
           (setq p2 (getpoint p1 "\nSecond point : "))
      )
    (progn
      (setq w1 (trans p1 1 0)
            w2 (trans p2 1 0)
            dh (distance (list (car w1) (cadr w1)) (list (car w2) (cadr w2)))
            dz (- (caddr w2) (caddr w1))
      )
      (if (< dh 1e-9)
        (princ "\nPoints à la verticale l'un de l'autre : pente indéfinie.")
        (progn
          (setq pc  (* 100.0 (/ dz dh))
                txt (strcat (if (> pc 0.0) "+" "") (lb:fmt pc 2) " %")
          )
          (princ (strcat "\nPente : " txt))
          (if (setq p1 (getpoint "\nPlacer le texte <Non> : "))
            (lb:texte p1 txt (getvar "TEXTSIZE") (getvar "CLAYER") T)
          )
        )
      )
    )
  )
  (lb:fin nil)
)

(princ)
