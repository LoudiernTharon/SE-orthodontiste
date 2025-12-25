(defpackage :se-orthodontiste.engine;;; ============================================================================

  (:use :cl);;; MODULE 3 : MOTEUR D'INFÉRENCE

  (:export #:fait;;; ============================================================================

           #:make-fait

           #:fait-attribut;; --- Outils d'évaluation ---

           #:fait-valeur

           #:fait-source(defun evaluer-condition (condition)

             "Vérifie si une condition (attribut operateur valeur) est vraie"

           #:regle  (let* ((attr (first condition))

           #:make-regle         (op   (second condition))

           #:regle-id         (val-ref (third condition))

           #:regle-premisses         (fait-trouve (valeur-fait attr)))

           #:regle-conclusions    

           #:regle-cf    (if (null fait-trouve)

           #:regle-description        :inconnu ;; Le fait n'est pas dans la base

           #:regle-active        (let ((val-reelle (fait-valeur fait-trouve)))

                     (case op

           #:*base-faits*            (equal  (equal val-reelle val-ref))

           #:*base-regles*            (>      (> val-reelle val-ref))

                       (<      (< val-reelle val-ref))

           #:valeur-fait            (>=     (>= val-reelle val-ref))

           #:ajouter-fait            (<=     (<= val-reelle val-ref))

           #:reinitialiser-base            (member (member val-reelle val-ref))

           #:evaluer-condition            (t nil))))))

           #:chainage-avant

           #:verifier-but;; --- MOTEUR 1 : CHAÎNAGE AVANT (Saturateur) ---

           #:demander-utilisateur))

(defun chainage-avant ()

(in-package :se-orthodontiste.engine)  "Sature la base de faits en appliquant les règles en boucle"

  (let ((nouveau-fait-trouve t)

;;; ============================================================================        (cycle 0))

;;; MODULE 1 : STRUCTURES DE DONNÉES    (loop while nouveau-fait-trouve do

;;; ============================================================================      (setf nouveau-fait-trouve nil)

      (incf cycle)

(defstruct fait      (format t "~&--- Cycle ~d ---~%" cycle)

  "Représente un fait connu sur le patient"      

  attribut   ; Symbole (ex: 'age)      (dolist (r *base-regles*)

  valeur     ; Valeur (ex: 12)        (when (regle-active r)

  source     ; :utilisateur ou :deduit          ;; Vérifier si toutes les prémisses sont vraies

)          (let ((conditions-ok t)

                (donnees-manquantes nil))

(defstruct regle            

  "Représente une règle de production"            (dolist (p (regle-premisses r))

  id          ; Identifiant (ex: 'R-A1)              (let ((res (evaluer-condition p)))

  premisses   ; Liste de conditions ((attribut operateur valeur)...)                (cond

  conclusions ; Liste de nouveaux faits ((attribut valeur)...)                  ((eq res :inconnu) 

  cf          ; Facteur de confiance (informatif)                   (setf conditions-ok nil)

  description ; Texte explicatif                   (push (first p) donnees-manquantes))

  (active t)  ; Pour désactiver une règle après utilisation                  ((not res) 

)                   (setf conditions-ok nil)))))

            

;; Base de faits (Working Memory)            ;; Si tout est OK, on déclenche

(defparameter *base-faits* nil)            (when conditions-ok

              (format t "DÉCLENCHEMENT RÈGLE ~a : ~a~%" (regle-id r) (regle-description r))

;; Base de règles              (dolist (c (regle-conclusions r))

(defparameter *base-regles* nil)                (unless (valeur-fait (first c))

                  (ajouter-fait (first c) (second c) :deduit)

;; Fonctions utilitaires pour la gestion des faits                  (setf nouveau-fait-trouve t)))

              (setf (regle-active r) nil)) ;; Désactiver la règle

(defun valeur-fait (attribut)            ))))))

  "Cherche si un attribut est déjà connu dans la base de faits"

  (find attribut *base-faits* :key #'fait-attribut));; --- MOTEUR 2 : CHAÎNAGE ARRIÈRE (Vérification d'hypothèse) ---



(defun ajouter-fait (attribut valeur &optional (source :utilisateur))(defun verifier-but (attribut valeur)

  "Ajoute un fait s'il n'existe pas déjà"  "Essaie de prouver qu'un attribut a une certaine valeur"

  (unless (valeur-fait attribut)  ;; 1. Vérifier si déjà dans la base

    (push (make-fait :attribut attribut :valeur valeur :source source) *base-faits*)  (let ((fait (valeur-fait attribut)))

    (format t "~&[INFO] Nouveau fait établi : ~a = ~a (~a)~%" attribut valeur source)))    (when fait

      (return-from verifier-but (equal (fait-valeur fait) valeur))))

(defun reinitialiser-base ()  

  "Vide la mémoire de travail et réactive les règles"  ;; 2. Chercher des règles qui concluent sur cet attribut

  (setf *base-faits* nil)  (let ((regles-candidates 

  (dolist (r *base-regles*)         (remove-if-not 

    (setf (regle-active r) t)))          #'(lambda (r) 

              (assoc attribut (regle-conclusions r))) 

;;; ============================================================================          *base-regles*)))

;;; MODULE 3 : MOTEUR D'INFÉRENCE    

;;; ============================================================================    (if (null regles-candidates)

        ;; Pas de règle -> Demander à l'utilisateur (Simplifié)

;; --- Outils d'évaluation ---        (demander-utilisateur attribut)

        

(defun evaluer-condition (condition)        ;; Sinon, essayer de prouver les prémisses des règles candidates

  "Vérifie si une condition (attribut operateur valeur) est vraie"        (some #'(lambda (r)

  (let* ((attr (first condition))                  (every #'(lambda (p)

         (op   (second condition))                             (let ((p-attr (first p))

         (val-ref (third condition))                                   (p-op (second p))

         (fait-trouve (valeur-fait attr)))                                   (p-val (third p)))

                                   (verifier-but p-attr p-val)))

    (if (null fait-trouve)                         (regle-premisses r)))

        :inconnu ;; Le fait n'est pas dans la base              regles-candidates))))

        (let ((val-reelle (fait-valeur fait-trouve)))

          (case op(defun demander-utilisateur (attribut)

            (equal  (equal val-reelle val-ref))  "Simulation de demande utilisateur pour le chaînage arrière"

            (>      (> val-reelle val-ref))  (format t "~&[QUESTION] Quelle est la valeur pour '~a' ? " attribut)

            (<      (< val-reelle val-ref))  (let ((reponse (read)))

            (>=     (>= val-reelle val-ref))    (ajouter-fait attribut reponse)

            (<=     (<= val-reelle val-ref))    reponse))

            (member (member val-reelle val-ref))
            (t nil))))))

;; --- MOTEUR 1 : CHAÎNAGE AVANT (Saturateur) ---

(defun chainage-avant ()
  "Sature la base de faits en appliquant les règles en boucle"
  (let ((nouveau-fait-trouve t)
        (cycle 0))
    (loop while nouveau-fait-trouve do
      (setf nouveau-fait-trouve nil)
      (incf cycle)
      (format t "~&--- Cycle ~d ---~%" cycle)
      
      (dolist (r *base-regles*)
        (when (regle-active r)
          ;; Vérifier si toutes les prémisses sont vraies
          (let ((conditions-ok t)
                (donnees-manquantes nil))
            
            (dolist (p (regle-premisses r))
              (let ((res (evaluer-condition p)))
                (cond
                  ((eq res :inconnu) 
                   (setf conditions-ok nil)
                   (push (first p) donnees-manquantes))
                  ((not res) 
                   (setf conditions-ok nil)))))
            
            ;; Si tout est OK, on déclenche
            (when conditions-ok
              (format t "DÉCLENCHEMENT RÈGLE ~a : ~a~%" (regle-id r) (regle-description r))
              (dolist (c (regle-conclusions r))
                (unless (valeur-fait (first c))
                  (ajouter-fait (first c) (second c) :deduit)
                  (setf nouveau-fait-trouve t)))
              (setf (regle-active r) nil)) ;; Désactiver la règle
            ))))))

;; --- MOTEUR 2 : CHAÎNAGE ARRIÈRE (Vérification d'hypothèse) ---

(defun verifier-but (attribut valeur)
  "Essaie de prouver qu'un attribut a une certaine valeur"
  ;; 1. Vérifier si déjà dans la base
  (let ((fait (valeur-fait attribut)))
    (when fait
      (return-from verifier-but (equal (fait-valeur fait) valeur))))
  
  ;; 2. Chercher des règles qui concluent sur cet attribut
  (let ((regles-candidates 
         (remove-if-not 
          #'(lambda (r) 
              (assoc attribut (regle-conclusions r))) 
          *base-regles*)))
    
    (if (null regles-candidates)
        ;; Pas de règle -> Demander à l'utilisateur (Simplifié)
        (demander-utilisateur attribut)
        
        ;; Sinon, essayer de prouver les prémisses des règles candidates
        (some #'(lambda (r)
                  (every #'(lambda (p)
                             (let ((p-attr (first p))
                                   (p-op (second p))
                                   (p-val (third p)))
                               (verifier-but p-attr p-val)))
                         (regle-premisses r)))
              regles-candidates))))

(defun demander-utilisateur (attribut)
  "Simulation de demande utilisateur pour le chaînage arrière"
  (format t "~&[QUESTION] Quelle est la valeur pour '~a' ? " attribut)
  (let ((reponse (read)))
    (ajouter-fait attribut reponse)
    reponse))
