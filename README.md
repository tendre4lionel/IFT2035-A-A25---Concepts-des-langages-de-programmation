# IFT2035-A-A25---Concepts-des-langages-de-programmation
IFT2035-A-A25 - Concepts des langages de programmation

## instalation
Pour installer le fichier du projet, coder et rouller projet, il est reccomendé d'utiliser VSC et dune. 
Le processus d'instalation serat expliqué ci-desous. (L'instalation assume que VSC, git, ocaml et opam sont déjà installés.)
Cependant, il est possible de rouler les fichiers clés (bin/interpreter.ml et bin/toplevel.ml) directement avec utop en ligne de commande.

- Ouvrir une nouvelle fenêtre (file/new window)
- Sous start selectionner 'clone git repository...' et entrer l'url web trouvé sous code/https sur la page github.
- Dans vsc, en ligne de commande, entrer les commandes suivantes;
  - `opam init`
  - `eval $(opam env)`
  - `opam switch create 4.14.0`
  - `eval $(opam env)`
  - `opam install ocaml-lsp-server ocamlformat`
  - `opam install dune`
- installer [ocaml platform](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform)
