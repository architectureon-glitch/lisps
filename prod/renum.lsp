;;; ============================================================
;;; renum.lsp — Numérotation incrémentale
;;; RENUM : place des numéros successifs (préfixe + numéro) par clics.
;;;         La numérotation reprend où elle s'était arrêtée.
;;; ============================================================

(lb:enregistrer "RENUM" "Numérotation incrémentale par clics (préfixe + n°)")

(defun c:RENUM (/ *error* pre num inc haut pt txt)
  (defun *error* (msg) (lb:fin msg))
  (lb:debut nil)

  (setq pre  (lb:getstring-def "\nPréfixe" (if lb:*renum-pre* lb:*renum-pre* ""))
        num  (lb:getint-def "\nNuméro de départ" (if lb:*renum-num* lb:*renum-num* 1))
        inc  (lb:getint-def "\nIncrément" (if lb:*renum-inc* lb:*renum-inc* 1))
        haut (lb:getdist-def "\nHauteur du texte"
                             (if lb:*renum-haut* lb:*renum-haut* (getvar "TEXTSIZE"))
             )
  )
  (setq lb:*renum-pre* pre lb:*renum-inc* inc lb:*renum-haut* haut)

  (while (setq pt (getpoint (strcat "\nPosition de " (setq txt (strcat pre (itoa num))) " <Fin> : ")))
    (lb:texte pt txt haut (getvar "CLAYER") T)
    (setq num (+ num inc))
  )
  (setq lb:*renum-num* num)
  (lb:fin nil)
)

(princ)
