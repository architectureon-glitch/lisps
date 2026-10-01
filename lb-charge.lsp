;;; ============================================================
;;; lb-charge.lsp — Chargeur de la bibliothèque
;;;
;;; Installation : dans AutoCAD, APPLOAD > Contenu > Ajouter
;;; ce fichier (lb-charge.lsp). Il charge ensuite tous les autres
;;; fichiers depuis son propre dossier à chaque démarrage.
;;;
;;; Pour ajouter un LISP : le déposer dans un des dossiers
;;; (archi, prod, topo, tools) puis l'ajouter à la liste ci-dessous.
;;; ============================================================

(setq lb:*racine*
       (vl-filename-directory (findfile "lb-charge.lsp"))
)

(defun lb:charger (rel / chemin r)
  (setq chemin (strcat lb:*racine* "/" rel))
  (if (findfile chemin)
    (progn
      (setq r (vl-catch-all-apply 'load (list chemin)))
      (if (vl-catch-all-error-p r)
        (princ (strcat "\n[lb] Erreur au chargement de " rel " : "
                       (vl-catch-all-error-message r)))
      )
    )
    (princ (strcat "\n[lb] Fichier introuvable : " rel))
  )
)

;; Le noyau d'abord, puis les modules
(foreach f '("core/lb-core.lsp"
             "archi/surf.lsp"
             "prod/mesures.lsp"
             "prod/renum.lsp"
             "prod/arrayorbit.lsp"
             "topo/topo.lsp"
            )
  (lb:charger f)
)

(princ "\nBibliothèque LISP chargée. Tapez LBAIDE pour la liste des commandes.")
(princ)
