# lisps

Bibliothèque de LISP pour AutoCAD (Windows, Visual LISP). Messages en français.

## Installation

1. Cloner / copier le dossier sur le poste.
2. Dans AutoCAD : `APPLOAD` > *Contenu* > *Ajouter* > `lb-charge.lsp`.
3. Ajouter le dossier aux *Chemins de support* (`OPTIONS` > Fichiers) pour que `findfile` retrouve le chargeur.
4. Taper `LBAIDE` pour lister les commandes.

## Organisation

| Dossier | Contenu |
|---|---|
| `core/` | Noyau : gestion d'erreurs/annulation, formats, saisies, création d'objets |
| `archi/` | Architecture / bâtiment |
| `prod/` | Productivité générale |
| `topo/` | Topo / VRD |
| `tools/` | Outils de développement (réservé) |

## Commandes

| Commande | Description |
|---|---|
| `SURF` | Surface de pièce par clic intérieur, texte en m² + total |
| `LONGT` | Longueur totale des courbes sélectionnées |
| `SOMTXT` | Somme des nombres contenus dans des textes |
| `RENUM` | Numérotation incrémentale par clics |
| `ALTI` | Cote d'altitude Z (`+12,35`, `±0,00`) |
| `PENTE` | Pente en % entre deux points |

## Conventions

- Fonctions : préfixe `lb:` ; variables globales : `lb:*nom*` ; commandes : `c:NOM` en majuscules, courtes.
- Chaque commande utilise `lb:debut` / `lb:fin` (restauration des variables système, un seul `U` annule tout).
- Chaque commande s'enregistre avec `lb:enregistrer` pour apparaître dans `LBAIDE`.
- Les surfaces/longueurs sont converties en m / m² d'après `INSUNITS`.
- Nouveau module : créer le `.lsp` dans le bon dossier, l'ajouter à la liste de `lb-charge.lsp`.

## État

Les fichiers n'ont pas encore été testés dans AutoCAD (pas d'AutoCAD dans l'environnement de développement) : à valider commande par commande.
