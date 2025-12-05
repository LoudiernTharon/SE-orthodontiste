# Glossaire Technique - Système Expert Orthodontique

## Résumé de l'implémentation

Ce document résume l'ajout du glossaire technique au rapport et à la présentation du système expert orthodontique.

### ✅ Modifications effectuées

#### 1. Rapport LaTeX (`rapport-projet.tex`)
- **Ajout d'une section complète "Glossaire Technique"** (avant la conclusion)
- **3 sous-sections thématiques** :
  - Termes d'Orthodontie (10 termes)
  - Concepts d'Intelligence Artificielle Symbolique (8 termes)
  - Structures de Données et Implémentation Lisp (10 termes)

#### 2. Présentation Beamer (`presentation-slides.tex`)
- **Ajout de 3 slides de glossaire** :
  - Slide "Glossaire Technique - Orthodontie"
  - Slide "Glossaire Technique - IA Symbolique"
  - Slide "Glossaire Technique - Implémentation Lisp"
- Format concis avec les termes les plus importants pour la présentation orale

### 📋 Contenu du glossaire

#### Orthodontie (termes médicaux)
- Classes d'Angle (I, II, III)
- Overjet / Overbite
- ANB / Wits
- Rétromaxillie
- Encombrement dentaire
- Disjoncteur
- Masque de Delaire
- Contention

#### IA Symbolique (concepts théoriques)
- Système Expert d'Ordre 0+
- Base de Faits / Base de Règles
- Chaînage Avant / Chaînage Arrière
- Saturation
- Règle de Production
- Gestion des Conflits
- Évaluation de Conditions

#### Implémentation Lisp (aspects techniques)
- Structure `defstruct` (fait, regle)
- Fonction `valeur-fait`
- Fonction `ajouter-fait`
- Fonction `reinitialiser-base`
- Moteur d'Inférence `chainage-avant`
- Gestion du Flag `active`
- Tests Unitaires Automatisés
- Fonction `poser-questions-base`
- Opérateur `member`
- Facteur de Confiance (CF)

### 🎯 Points forts du glossaire

1. **Lien code-théorie** : Chaque terme renvoie aux lignes de code correspondantes
2. **Pédagogie** : Explications accessibles à un étudiant en informatique sans background médical
3. **Justifications** : Explication du "pourquoi" des choix techniques
4. **Défendabilité** : Permet de répondre aux questions d'un jury mixte (info + santé)

### ✅ Vérification de la compilation

Les deux documents se compilent correctement :

```bash
# Rapport (avec bibliographie)
pdflatex rapport-projet.tex
biber rapport-projet
pdflatex rapport-projet.tex
# ✓ Généré : rapport-projet.pdf (10 pages, 222 KB)

# Présentation
pdflatex presentation-slides.tex
# ✓ Généré : presentation-slides.pdf (12 pages, 143 KB)
```

**Warnings résiduels** : Uniquement des avertissements de mise en page (Overfull hbox) et symbole degré en mode math - **aucun impact sur le PDF final**.

### 📚 Exemples de définitions

**Exemple orthodontie** :
> **ANB** : Mesure céphalométrique fondamentale calculée sur une téléradiographie de profil. Angle formé par les points A (base du maxillaire), N (nasion) et B (base de la mandibule). Valeur normale : 2° à 4°. ANB >4° = Classe II squelettique. ANB <0° = Classe III squelettique (Règle R-A4, ligne 79).

**Exemple IA** :
> **Chaînage Avant** : Stratégie d'inférence dirigée par les données. Part des faits connus (symptômes) pour déduire de nouveaux faits (diagnostic, traitement). Implémenté dans `chainage-avant()` (lignes 200-226). Justification : mimétisme du raisonnement clinique médical.

**Exemple Lisp** :
> **Structure defstruct** : Macro Common Lisp créant un type de données structuré avec accesseurs automatiques. Génère `make-fait`, `fait-attribut`, `fait-valeur`, etc. Avantages : clarté sémantique, typage, performances O(1).

### 🎓 Usage pédagogique

Le glossaire permet de :
- ✅ Comprendre chaque ligne de code
- ✅ Expliquer les choix techniques
- ✅ Défendre l'architecture du système
- ✅ Présenter clairement à un jury mixte (informatique + santé)

### 📁 Fichiers modifiés

1. `rapport-projet.tex` - Section Glossaire ajoutée avant la conclusion
2. `presentation-slides.tex` - 3 slides de glossaire ajoutées avant la conclusion
3. Les deux PDFs se génèrent correctement sans erreurs

---

**Date de création** : 5 décembre 2025  
**Auteur** : GitHub Copilot (exécution du prompt prompt-copilot.txt)
