(defpackage :se-orthodontiste.rules;;; ============================================================================

  (:use :cl :se-orthodontiste.engine);;; MODULE 2 : BASE DE CONNAISSANCES

  (:export #:charger-regles));;; ============================================================================



(in-package :se-orthodontiste.rules)(defun charger-regles ()

  (setf *base-regles* nil)

;;; ============================================================================  

;;; MODULE 2 : BASE DE CONNAISSANCES  ;; --- MODULE A : DIAGNOSTIC ---

;;; ============================================================================  

  (push (make-regle 

(defun charger-regles ()         :id 'R-A1 

  (setf *base-regles* nil)         :premisses '((relation-molaire equal classe-1) (encombrement <= 4))

           :conclusions '((diagnostic classe-1-mineure))

  ;; --- MODULE A : DIAGNOSTIC ---         :cf 0.95 

           :description "Classe I avec encombrement mineur") *base-regles*)

  (push (make-regle 

         :id 'R-A1   (push (make-regle 

         :premisses '((relation-molaire equal classe-1) (encombrement <= 4))         :id 'R-A2 

         :conclusions '((diagnostic classe-1-mineure))         :premisses '((relation-molaire equal classe-2) (incisives-max equal proclinees) (overjet > 5))

         :cf 0.95          :conclusions '((diagnostic classe-2-div-1))

         :description "Classe I avec encombrement mineur") *base-regles*)         :cf 0.90 

         :description "Classe II division 1 classique") *base-regles*)

  (push (make-regle 

         :id 'R-A2   (push (make-regle 

         :premisses '((relation-molaire equal classe-2) (incisives-max equal proclinees) (overjet > 5))         :id 'R-A3

         :conclusions '((diagnostic classe-2-div-1))         :premisses '((relation-molaire equal classe-2) (incisives-centrales equal retroclinees) (overbite > 4))

         :cf 0.90          :conclusions '((diagnostic classe-2-div-2))

         :description "Classe II division 1 classique") *base-regles*)         :cf 0.90 

         :description "Classe II division 2 (Deep bite)") *base-regles*)

  (push (make-regle   

         :id 'R-A3  (push (make-regle 

         :premisses '((relation-molaire equal classe-2) (incisives-centrales equal retroclinees) (overbite > 4))         :id 'R-A4

         :conclusions '((diagnostic classe-2-div-2))         :premisses '((relation-molaire equal classe-3) (anb < 0) (wits < -2))

         :cf 0.90          :conclusions '((diagnostic classe-3-squelettique))

         :description "Classe II division 2 (Deep bite)") *base-regles*)         :cf 0.85 

           :description "Classe III vraie squelettique") *base-regles*)

  (push (make-regle 

         :id 'R-A4  (push (make-regle 

         :premisses '((relation-molaire equal classe-3) (anb < 0) (wits < -2))         :id 'R-A5

         :conclusions '((diagnostic classe-3-squelettique))         :premisses '((relation-molaire equal classe-3) (anb > 0) (incisives-mand equal proclinees))

         :cf 0.85          :conclusions '((diagnostic classe-3-dento-alveolaire))

         :description "Classe III vraie squelettique") *base-regles*)         :cf 0.80 

         :description "Pseudo Classe III") *base-regles*)

  (push (make-regle          

         :id 'R-A5  ;; Règle A6 avec gestion de l'OU via liste dans premisses (simplification)

         :premisses '((relation-molaire equal classe-3) (anb > 0) (incisives-mand equal proclinees))  (push (make-regle 

         :conclusions '((diagnostic classe-3-dento-alveolaire))         :id 'R-A6

         :cf 0.80          :premisses '((age < 10) (stade-croissance member (cs1 cs2 cs3)) (diagnostic member (classe-2-squelettique classe-3-squelettique)))

         :description "Pseudo Classe III") *base-regles*)         :conclusions '((traitement phase-orthopedique))

                  :cf 0.85 

  ;; Règle A6 avec gestion de l'OU via liste dans premisses (simplification)         :description "Indication traitement précoce") *base-regles*)

  (push (make-regle 

         :id 'R-A6  ;; --- MODULE B : APPAREILLAGE ---

         :premisses '((age < 10) (stade-croissance member (cs1 cs2 cs3)) (diagnostic member (classe-2-squelettique classe-3-squelettique)))

         :conclusions '((traitement phase-orthopedique))  (push (make-regle 

         :cf 0.85          :id 'R-B1

         :description "Indication traitement précoce") *base-regles*)         :premisses '((diagnostic equal classe-2-div-1) (age >= 8) (age <= 12) (cooperation equal bonne))

         :conclusions '((appareil fonctionnel))

  ;; --- MODULE B : APPAREILLAGE ---         :cf 0.80 

         :description "Twin Block ou Activateur pour enfant coopératif") *base-regles*)

  (push (make-regle 

         :id 'R-B1  (push (make-regle 

         :premisses '((diagnostic equal classe-2-div-1) (age >= 8) (age <= 12) (cooperation equal bonne))         :id 'R-B2

         :conclusions '((appareil fonctionnel))         :premisses '((diagnostic equal classe-2-div-1) (age > 12) (classe-squelettique equal moderee))

         :cf 0.80          :conclusions '((appareil headgear-multibagues))

         :description "Twin Block ou Activateur pour enfant coopératif") *base-regles*)         :cf 0.75 

         :description "FEO pour adolescent") *base-regles*)

  (push (make-regle 

         :id 'R-B2  (push (make-regle 

         :premisses '((diagnostic equal classe-2-div-1) (age > 12) (classe-squelettique equal moderee))         :id 'R-B3

         :conclusions '((appareil headgear-multibagues))         :premisses '((diagnostic equal classe-3-squelettique) (age >= 6) (age <= 9) (retromaxillie equal t))

         :cf 0.75          :conclusions '((appareil masque-delaire))

         :description "FEO pour adolescent") *base-regles*)         :cf 0.85 

         :description "Masque facial pour Cl III jeune") *base-regles*)

  (push (make-regle 

         :id 'R-B3  (push (make-regle 

         :premisses '((diagnostic equal classe-3-squelettique) (age >= 6) (age <= 9) (retromaxillie equal t))         :id 'R-B4

         :conclusions '((appareil masque-delaire))         :premisses '((diagnostic equal classe-1-mineure) (encombrement >= 2) (encombrement <= 6) (motivation-esthetique equal elevee) (age >= 15))

         :cf 0.85          :conclusions '((appareil gouttieres-alignement))

         :description "Masque facial pour Cl III jeune") *base-regles*)         :cf 0.90 

         :description "Invisalign/Aligners pour adulte esthétique") *base-regles*)

  (push (make-regle 

         :id 'R-B4  (push (make-regle 

         :premisses '((diagnostic equal classe-1-mineure) (encombrement >= 2) (encombrement <= 6) (motivation-esthetique equal elevee) (age >= 15))         :id 'R-B5

         :conclusions '((appareil gouttieres-alignement))         :premisses '((arcade-max equal etroite) (age < 14) (beance-transversale equal t))

         :cf 0.90          :conclusions '((appareil disjoncteur))

         :description "Invisalign/Aligners pour adulte esthétique") *base-regles*)         :cf 0.80 

         :description "Expansion rapide maxillaire") *base-regles*)

  (push (make-regle 

         :id 'R-B5  ;; Décomposition des OU pour R-B6

         :premisses '((arcade-max equal etroite) (age < 14) (beance-transversale equal t))  (push (make-regle :id 'R-B6a :premisses '((ancrage equal maximum)) :conclusions '((accessoire minivis)) :cf 0.75 :description "Minivis pour ancrage") *base-regles*)

         :conclusions '((appareil disjoncteur))  (push (make-regle :id 'R-B6b :premisses '((mouvement equal complexe)) :conclusions '((accessoire minivis)) :cf 0.75 :description "Minivis pour mvt complexe") *base-regles*)

         :cf 0.80 

         :description "Expansion rapide maxillaire") *base-regles*)  (push (make-regle 

         :id 'R-B7

  ;; Décomposition des OU pour R-B6         :premisses '((statut-traitement equal fini) (antecedent-encombrement equal t))

  (push (make-regle :id 'R-B6a :premisses '((ancrage equal maximum)) :conclusions '((accessoire minivis)) :cf 0.75 :description "Minivis pour ancrage") *base-regles*)         :conclusions '((contention-inf fil-colle-3-3))

  (push (make-regle :id 'R-B6b :premisses '((mouvement equal complexe)) :conclusions '((accessoire minivis)) :cf 0.75 :description "Minivis pour mvt complexe") *base-regles*)         :cf 0.85 

         :description "Contention fixe mandibulaire") *base-regles*)

  (push (make-regle 

         :id 'R-B7  (push (make-regle 

         :premisses '((statut-traitement equal fini) (antecedent-encombrement equal t))         :id 'R-B8

         :conclusions '((contention-inf fil-colle-3-3))         :premisses '((statut-traitement equal fini) (hygiene equal bonne) (cooperation equal suffisante))

         :cf 0.85          :conclusions '((contention-sup gouttiere-nuit))

         :description "Contention fixe mandibulaire") *base-regles*)         :cf 0.70 

         :description "Contention amovible maxillaire") *base-regles*)

  (push (make-regle 

         :id 'R-B8  ;; --- MODULE C : CONTRE-INDICATIONS ---

         :premisses '((statut-traitement equal fini) (hygiene equal bonne) (cooperation equal suffisante))

         :conclusions '((contention-sup gouttiere-nuit))  (push (make-regle :id 'R-C1a :premisses '((parodontite equal active)) :conclusions '((traitement contre-indique)) :cf 0.95 :description "CI Paro") *base-regles*)

         :cf 0.70   (push (make-regle :id 'R-C1b :premisses '((perte-attache > 50)) :conclusions '((traitement contre-indique)) :cf 0.95 :description "CI Perte osseuse") *base-regles*)

         :description "Contention amovible maxillaire") *base-regles*)  

  (push (make-regle :id 'R-C3

  ;; --- MODULE C : CONTRE-INDICATIONS ---         :premisses '((diagnostic equal classe-3-squelettique) (age > 14) (anb < -3))

         :conclusions '((traitement chirurgie-orthognathique))

  (push (make-regle :id 'R-C1a :premisses '((parodontite equal active)) :conclusions '((traitement contre-indique)) :cf 0.95 :description "CI Paro") *base-regles*)         :cf 0.80 

  (push (make-regle :id 'R-C1b :premisses '((perte-attache > 50)) :conclusions '((traitement contre-indique)) :cf 0.95 :description "CI Perte osseuse") *base-regles*)         :description "Indication chirurgicale Cl III sévère") *base-regles*)

    

  (push (make-regle :id 'R-C3  (push (make-regle 

         :premisses '((diagnostic equal classe-3-squelettique) (age > 14) (anb < -3))         :id 'R-C5

         :conclusions '((traitement chirurgie-orthognathique))         :premisses '((rotation-canine > 45))

         :cf 0.80          :conclusions '((appareil-interdit aligneurs))

         :description "Indication chirurgicale Cl III sévère") *base-regles*)         :cf 0.80 

           :description "Limite technique des aligneurs") *base-regles*)

  (push (make-regle 

         :id 'R-C5  ;; Inversion de la liste pour l'ordre de priorité

         :premisses '((rotation-canine > 45))  (setf *base-regles* (nreverse *base-regles*))

         :conclusions '((appareil-interdit aligneurs))  (format t "~&Base de règles chargée : ~d règles.~%" (length *base-regles*)))

         :cf 0.80 
         :description "Limite technique des aligneurs") *base-regles*)

  ;; Inversion de la liste pour l'ordre de priorité
  (setf *base-regles* (nreverse *base-regles*))
  (format t "~&Base de règles chargée : ~d règles.~%" (length *base-regles*)))
