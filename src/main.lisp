(defpackage :se-orthodontiste.main;;; ============================================================================

  (:use :cl :se-orthodontiste.engine :se-orthodontiste.rules);;; SYSTÈME EXPERT ORTHODONTIQUE

  (:export #:main));;; ============================================================================



(in-package :se-orthodontiste.main);;; MODULE 1 : STRUCTURES DE DONNÉES

(defstruct fait

;;; ============================================================================  "Représente un fait connu sur le patient"

;;; MODULE 4 : INTERFACE ET SCÉNARIOS  attribut   ; Symbole (ex: 'age)

;;; ============================================================================  valeur     ; Valeur (ex: 12)

  source     ; :utilisateur ou :deduit

(defun poser-questions-base ())

  "Pose les questions initiales essentielles"

  (format t "~%--- COLLECTE DES DONNÉES CLINIQUES ---~%")(defstruct regle

    "Représente une règle de production"

  (format t "Âge du patient (entier) : ")  id          ; Identifiant (ex: 'R-A1)

  (ajouter-fait 'age (read))  premisses   ; Liste de conditions ((attribut operateur valeur)...)

    conclusions ; Liste de nouveaux faits ((attribut valeur)...)

  (format t "Relation Molaire (classe-1, classe-2, classe-3) : ")  cf          ; Facteur de confiance (informatif)

  (ajouter-fait 'relation-molaire (read))  description ; Texte explicatif

    (active t)  ; Pour désactiver une règle après utilisation

  (format t "Valeur ANB (degrés, ex: 2 ou -4) : "))

  (ajouter-fait 'anb (read))

  ;; Base de faits (Working Memory)

  (format t "Encombrement (mm, ex: 3) : ")(defparameter *base-faits* nil)

  (ajouter-fait 'encombrement (read))

  ;; Base de règles

  (format t "Overjet (mm, ex: 6) : ")(defparameter *base-regles* nil)

  (ajouter-fait 'overjet (read))

  ;; Fonctions utilitaires pour la gestion des faits

  (format t "Incisives maxillaires (proclinees, retroclinees, normales) : ")

  (ajouter-fait 'incisives-max (read));; --- CORRECTION EFFECTUÉE ICI (Version non récursive) ---

  (defun valeur-fait (attribut)

  (format t "Coopération prévisible (bonne, moyenne, mauvaise) : ")  "Cherche si un attribut est déjà connu dans la base de faits"

  (ajouter-fait 'cooperation (read)))  (find attribut *base-faits* :key #'fait-attribut))



(defun lancer-diagnostic ()(defun ajouter-fait (attribut valeur &optional (source :utilisateur))

  (reinitialiser-base)  "Ajoute un fait s'il n'existe pas déjà"

  (charger-regles)  (unless (valeur-fait attribut)

  (format t "~%=== SYSTÈME EXPERT ORTHODONTIQUE ===~%")    (push (make-fait :attribut attribut :valeur valeur :source source) *base-faits*)

      (format t "~&[INFO] Nouveau fait établi : ~a = ~a (~a)~%" attribut valeur source)))

  (poser-questions-base)

  (defun reinitialiser-base ()

  (format t "~%--- DÉMARRAGE MOTEUR (CHAÎNAGE AVANT) ---~%")  "Vide la mémoire de travail et réactive les règles"

  (chainage-avant)  (setf *base-faits* nil)

    (dolist (r *base-regles*)

  (format t "~%--- RÉSULTATS DU DIAGNOSTIC ---~%")    (setf (regle-active r) t)))

  (let ((diag (valeur-fait 'diagnostic))

        (app (valeur-fait 'appareil));;; ============================================================================

        (ci (valeur-fait 'traitement)));;; MODULE 4 : INTERFACE ET SCÉNARIOS

    ;;; ============================================================================

    (if diag (format t "Diagnostic : ~a~%" (fait-valeur diag))

             (format t "Diagnostic : Indéterminé~%"))(defun poser-questions-base ()

      "Pose les questions initiales essentielles"

    (if app  (format t "Appareil Recommandé : ~a~%" (fait-valeur app))  (format t "~%--- COLLECTE DES DONNÉES CLINIQUES ---~%")

             (format t "Appareil : Aucun appareil spécifique identifié~%"))  

               (format t "Âge du patient (entier) : ")

    (if (and ci (equal (fait-valeur ci) 'contre-indique))  (ajouter-fait 'age (read))

        (format t "/!\\ ATTENTION : Traitement CONTRE-INDIQUÉ /!\\~%")))  

          (format t "Relation Molaire (classe-1, classe-2, classe-3) : ")

  (format t "~%--- AVERTISSEMENT ---~%")  (ajouter-fait 'relation-molaire (read))

  (format t "Ce système est une aide pédagogique. Consultez un spécialiste.~%"))  

  (format t "Valeur ANB (degrés, ex: 2 ou -4) : ")

;;; ============================================================================  (ajouter-fait 'anb (read))

;;; POINT D'ENTRÉE PRINCIPAL  

;;; ============================================================================  (format t "Encombrement (mm, ex: 3) : ")

  (ajouter-fait 'encombrement (read))

(defun main ()  

  "Fonction principale du système"  (format t "Overjet (mm, ex: 6) : ")

  (loop  (ajouter-fait 'overjet (read))

    (format t "=============================================~%")  

    (format t "  SYSTÈME EXPERT ORTHODONTIQUE v1.0~%")  (format t "Incisives maxillaires (proclinees, retroclinees, normales) : ")

    (format t "=============================================~%~%")  (ajouter-fait 'incisives-max (read))

    (format t "Options :~%")  

    (format t "  1. Diagnostic complet~%")  (format t "Coopération prévisible (bonne, moyenne, mauvaise) : ")

    (format t "  2. Quitter~%~%")  (ajouter-fait 'cooperation (read)))

    (format t "Choix (1-2) : ")

    (defun lancer-diagnostic ()

    (case (read)  (reinitialiser-base)

      (1 (lancer-diagnostic))  (charger-regles)

      (2 (progn  (format t "~%=== SYSTÈME EXPERT ORTHODONTIQUE ===~%")

           (format t "Au revoir!~%")  

           (return-from main)))  (poser-questions-base)

      (t (format t "Choix invalide~%")))))  

  (format t "~%--- DÉMARRAGE MOTEUR (CHAÎNAGE AVANT) ---~%")

;; Méthode pour générer l'exécutable  (chainage-avant)

(defun build-binary ()  

  (sb-ext:save-lisp-and-die "se-orthodontiste"  (format t "~%--- RÉSULTATS DU DIAGNOSTIC ---~%")

                            :toplevel #'main  (let ((diag (valeur-fait 'diagnostic))

                            :executable t))        (app (valeur-fait 'appareil))

        (ci (valeur-fait 'traitement)))
    
    (if diag (format t "Diagnostic : ~a~%" (fait-valeur diag))
             (format t "Diagnostic : Indéterminé~%"))
    
    (if app  (format t "Appareil Recommandé : ~a~%" (fait-valeur app))
             (format t "Appareil : Aucun appareil spécifique identifié~%"))
             
    (if (and ci (equal (fait-valeur ci) 'contre-indique))
        (format t "/!\\ ATTENTION : Traitement CONTRE-INDIQUÉ /!\\~%")))
        
  (format t "~%--- AVERTISSEMENT ---~%")
  (format t "Ce système est une aide pédagogique. Consultez un spécialiste.~%"))

(defun test-cas-classe-2 ()
  "Simule un patient Classe II Div 1 de 10 ans"
  (reinitialiser-base)
  (charger-regles)
  
  (ajouter-fait 'age 10)
  (ajouter-fait 'relation-molaire 'classe-2)
  (ajouter-fait 'incisives-max 'proclinees)
  (ajouter-fait 'overjet 6)
  (ajouter-fait 'cooperation 'bonne)
  
  (format t "~%--- LANCEMENT TEST CLASSE II ---~%")
  (chainage-avant)
  
  (format t "~%Vérification : ~%")
  (let ((diag (valeur-fait 'diagnostic))
        (app (valeur-fait 'appareil)))
    (if diag (format t "Diagnostic trouvé : ~a~%" (fait-valeur diag))
             (format t "Diagnostic : Indéterminé~%"))
    (if app  (format t "Appareil trouvé : ~a~%" (fait-valeur app))
             (format t "Appareil : Aucun~%"))))


;;; ============================================================================
;;; POINT D'ENTRÉE PRINCIPAL
;;; ============================================================================

(defun main ()
  "Fonction principale du système"
  (loop
    (format t "=============================================~%")
    (format t "  SYSTÈME EXPERT ORTHODONTIQUE v1.0~%")
    (format t "=============================================~%~%")
    (format t "Options :~%")
    (format t "  1. Diagnostic complet~%")
    (format t "  2. Test cas Classe II~%")
    (format t "  3. Tests de robustesse~%")
    (format t "  4. Quitter~%~%")
    (format t "Choix (1-4) : ")
    
    (case (read)
      (1 (lancer-diagnostic))
      (2 (test-cas-classe-2))
      (3 (menu-tests))
      (4 (progn
           (format t "Au revoir!~%")
           (return-from main)))
      (t (format t "Choix invalide~%")))))