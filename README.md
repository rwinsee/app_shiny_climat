# App Shiny – Hackathon « Climat des données » – Défi 8 Dataviz distanciel

Application **R Shiny** développée dans le cadre du hackathon **« Climat des données »** (défi 8 – datavisualisation).  
L’app propose un tableau de bord interactif pour explorer et visualiser les données mises à disposition (indicateurs, comparaisons, filtres, etc.).

---

## 🎯 Objectifs

- Fournir une interface simple et ergonomique pour explorer les données du climat dans le cadre du défi 8.  
- Permettre aux membres de l’équipe de tester rapidement des idées de dataviz.  
- Préparer une app facilement **déployable sur le cluster Onyxia/SSPCloud**.

---

## 🧱 Structure du projet

- `app.R`  
  Fichier unique contenant les packages, le thème, l’interface utilisateur (UI) et la logique serveur.
- `R/`  
  Fonctions utilitaires supplémentaires (préparation des données, modules Shiny, etc.).
- `data/`  
  Données locales (ou échantillons pour le développement).
- `www/`  
  Ressources statiques : CSS, JS, images, logos.
- `renv/` & `renv.lock`  
  Gestion des dépendances R pour un environnement reproductible.


---

## 🔧 Prérequis

- R ≥ 4.2  
- RStudio (local ou via **Onyxia / SSPCloud**)
- Packages principaux (liste indicative) :
  - `shiny`
  - `shinydashboard` ou `bs4Dash`
  - `tidyverse`
  - `plotly`
  - `DT`
  - `readr`, `readxl`, etc. (selon les formats de données)
  - `renv` (si utilisé)

Installation des packages de base :

```r
install.packages(c(
  "shiny", "shinydashboard", "tidyverse",
  "plotly", "DT"
))
```

Pour `renv` :

```r
install.packages("renv")
renv::restore()
```

---

## ▶️ Lancer l’application en local

Depuis R ou RStudio :

```r
library(shiny)
runApp(".")
```

ou, si l’app est dans un fichier `app.R`, simplement cliquer sur **Run App** dans RStudio.

---

## 🚀 Déploiement

Le déploiement sur le **cluster Onyxia / SSPCloud** est géré dans un **projet séparé** (projet `deploy`), contenant :

* L’image Docker / configuration renv
* Les scripts et templates Onyxia
* Les variables d’environnement nécessaires (chemins de données, mode debug/production, etc.)

Ce dépôt **app** se concentre exclusivement sur :

* le **code Shiny**,
* la **logique métier**,
* et la **construction des visualisations**.

---

## 🤝 Contribution

* Les membres de l’équipe peuvent créer des branches pour tester de nouvelles visualisations ou modules.
* Les MR/PR sont bienvenues pour :

  * améliorer l’UX / UI du dashboard,
  * ajouter de nouveaux indicateurs,
  * optimiser les performances (chargement de données, réactivité, etc.).

---

## 📄 Licence

À définir selon les règles du hackathon et de l’équipe (par exemple : MIT, GPL, ou usage interne uniquement).
