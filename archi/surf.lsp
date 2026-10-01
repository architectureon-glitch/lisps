;;; ============================================================
;;; surf.lsp — Surfaces de pièces
;;; SURF : clic à l'intérieur des pièces -> texte de surface en m²
;;;        et total affiché en fin de commande.
;;; ============================================================

(lb:enregistrer "SURF" "Surface de pièce par clic intérieur (texte en m²)")

(defun c:SURF (/ *error* fac haut cal pt bord aire total n)
  (defun *error* (msg) (lb:fin msg))
  (lb:debut '("CMDECHO"))
  (setvar "CMDECHO" 0)

  (setq fac   (expt (lb:facteur-m) 2)
        haut  (lb:getdist-def "\nHauteur du texte"
                              (if lb:*surf-haut* lb:*surf-haut* (getvar "TEXTSIZE"))
              )
        lb:*surf-haut* haut
        cal   (lb:calque "A-SURFACES" 3)
        total 0.0
        n     0
  )

  (while (setq pt (getpoint "\nCliquer à l'intérieur d'une pièce <Fin> : "))
    (if (setq bord (bpoly pt))
      (progn
        (setq aire (* fac (vla-get-Area (vlax-ename->vla-object bord))))
        (entdel bord)
        (lb:texte pt (strcat (lb:fmt aire 2) " m\\U+00B2") haut cal T)
        (setq total (+ total aire)
              n     (1+ n)
        )
        (princ (strcat "\nSurface : " (lb:fmt aire 2) " m²"))
      )
      (princ "\nAucun contour fermé trouvé à cet endroit.")
    )
  )

  (if (> n 0)
    (princ (strcat "\n" (itoa n) " pièce(s) - total : " (lb:fmt total 2) " m²"))
  )
  (lb:fin nil)
)

(princ)
