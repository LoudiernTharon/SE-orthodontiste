<a id="readme-top"></a>

<div align="center">
  <h3 align="center">Système Expert Orthodontiste – Version Académique</h3>
  <p align="center">
    Système expert d’aide à la décision pour l’appareillage orthodontique – Projet universitaire
    <br />
    <a href="https://github.com/LoudiernTharon/SE-orthodontiste/tree/académique"><strong>Explorer les livrables »</strong></a>
  </p>
</div>

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table des matières</summary>
  <ol>
    <li>
      <a href="#about-the-project">À propos du projet</a>
      <ul>
        <li><a href="#built-with">Technologies utilisées</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Mise en route</a>
      <ul>
        <li><a href="#prerequisites">Prérequis</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Utilisation</a></li>
    <li><a href="#deliverables">Livrables</a></li>
    <li><a href="#license">Licence</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

## À propos du projet

Ce projet présente la réalisation d'un système expert d'ordre 0+ conçu pour assister les praticiens dans le choix d'appareillages orthodontiques. Il s'appuie sur une base de connaissances cliniques (classes d'Angle, mesures céphalométriques) pour déduire un diagnostic et proposer un traitement adapté.

### Points clés :
* **Formalisation** : Base de règles de production et arbre de déduction clinique.
* **Moteur d'inférences** : Implémentation en Lisp supportant le chaînage avant et arrière.
* **IA Générative** : Documentation de l'usage de Copilot et Gemini Pro dans le processus de développement.

<p align="right">(<a href="#readme-top">retour en haut</a>)</p>

### Technologies utilisées

* [![Lisp](https://img.shields.io/badge/Lisp-000000?style=for-the-badge&logo=commonlisp&logoColor=white)](https://common-lisp.net/)
* [![LaTeX](https://img.shields.io/badge/LaTeX-47A141?style=for-the-badge&logo=latex&logoColor=white)](https://www.latex-project.org/)

<p align="right">(<a href="#readme-top">retour en haut</a>)</p>

## Mise en route

### Prérequis
* Un interpréteur Common Lisp (ex: [SBCL](http://www.sbcl.org/))
* Un environnement LaTeX pour compiler les sources du rapport.

### Installation
1. Clonez le dépôt sur la branche académique
   ```sh
   git clone -b académique https://github.com/LoudiernTharon/SE-orthodontiste.git
   ```
2. Chargez le système expert dans votre REPL Lisp
   ```lisp
   (load "code-source/expert-orthodontiste.lisp")
   ```

<p align="right">(<a href="#readme-top">retour en haut</a>)</p>

## Utilisation

Pour lancer une session de diagnostic :
```lisp
(reinitialiser-base)
(lancer-expertise)
```
Le système vous posera une série de questions sur les mesures cliniques du patient (ex: relation molaire, overjet, âge) avant de proposer une recommandation.

<p align="right">(<a href="#readme-top">retour en haut</a>)</p>

## Livrables
- `rapport/rapport-projet.pdf` : Analyse théorique et justification des choix techniques.
- `presentation/presentation-slides.pdf` : Support visuel utilisé lors de la soutenance.
- `code-source/expert-orthodontiste.lisp` : Code source complet et commenté.

<p align="right">(<a href="#readme-top">retour en haut</a>)</p>

## Licence

Distribué sous licence MIT. Voir `LICENSE` pour plus d'informations.

<p align="right">(<a href="#readme-top">retour en haut</a>)</p>

## Contact

LoudiernTharon - [https://github.com/LoudiernTharon](https://github.com/LoudiernTharon)

Lien du projet: [https://github.com/LoudiernTharon/SE-orthodontiste](https://github.com/LoudiernTharon/SE-orthodontiste)

<p align="right">(<a href="#readme-top">retour en haut</a>)</p>

Distribué sous licence MIT.

<p align="right">(<a href="#readme-top">retour en haut</a>)</p>

## Contact

Loudiern Tharon - loudiern.tharon@etu.utc.fr et Lou Aubert-Debrue - lou.aubert-debrue@etu.utc.fr Lien du projet: https://github.com/LoudiernTharon/SE-orthodontiste

<p align="right">(<a href="#readme-top">retour en haut</a>)</p>
