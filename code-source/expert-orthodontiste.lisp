;;; ============================================================================
;;; SYSTÈME EXPERT ORTHODONTIQUE
;;; ============================================================================

;;; MODULE 1 : STRUCTURES DE DONNÉES
(defstruct fait
  "Représente un fait connu sur le patient"
  attribut   ; Symbole (ex: 'age)
  valeur     ; Valeur (ex: 12)
  source     ; :utilisateur ou :deduit
)

(defstruct regle
  "Représente une règle de production"
  id          ; Identifiant (ex: 'R-A1)
  premisses   ; Liste de conditions ((attribut operateur valeur)...)
  conclusions ; Liste de nouveaux faits ((attribut valeur)...)
  cf          ; Facteur de confiance (informatif)
  description ; Texte explicatif
  (active t)  ; Pour désactiver une règle après utilisation
)

;; Base de faits (Working Memory)
(defparameter *base-faits* nil)

;; Base de règles
(defparameter *base-regles* nil)

;; Fonctions utilitaires pour la gestion des faits

;; --- CORRECTION EFFECTUÉE ICI (Version non récursive) ---
(defun valeur-fait (attribut)
  "Cherche si un attribut est déjà connu dans la base de faits"
  (find attribut *base-faits* :key #'fait-attribut))

(defun ajouter-fait (attribut valeur &optional (source :utilisateur))
  "Ajoute un fait s'il n'existe pas déjà"
  (unless (valeur-fait attribut)
    (push (make-fait :attribut attribut :valeur valeur :source source) *base-faits*)
    (format t "~&[INFO] Nouveau fait établi : ~a = ~a (~a)~%" attribut valeur source)))

(defun reinitialiser-base ()
  "Vide la mémoire de travail et réactive les règles"
  (setf *base-faits* nil)
  (dolist (r *base-regles*)
    (setf (regle-active r) t)))

;;; ============================================================================
;;; MODULE 2 : BASE DE CONNAISSANCES
;;; ============================================================================

(defun charger-regles ()
  (setf *base-regles* nil)
  
  ;; --- MODULE A : DIAGNOSTIC ---
  
  (push (make-regle 
         :id 'R-A1 
         :premisses '((relation-molaire equal classe-1) (encombrement <= 4))
         :conclusions '((diagnostic classe-1-mineure))
         :cf 0.95 
         :description "Classe I avec encombrement mineur") *base-regles*)

  (push (make-regle 
         :id 'R-A2 
         :premisses '((relation-molaire equal classe-2) (incisives-max equal proclinees) (overjet > 5))
         :conclusions '((diagnostic classe-2-div-1))
         :cf 0.90 
         :description "Classe II division 1 classique") *base-regles*)

  (push (make-regle 
         :id 'R-A3
         :premisses '((relation-molaire equal classe-2) (incisives-centrales equal retroclinees) (overbite > 4))
         :conclusions '((diagnostic classe-2-div-2))
         :cf 0.90 
         :description "Classe II division 2 (Deep bite)") *base-regles*)
  
  (push (make-regle 
         :id 'R-A4
         :premisses '((relation-molaire equal classe-3) (anb < 0) (wits < -2))
         :conclusions '((diagnostic classe-3-squelettique))
         :cf 0.85 
         :description "Classe III vraie squelettique") *base-regles*)

  (push (make-regle 
         :id 'R-A5
         :premisses '((relation-molaire equal classe-3) (anb > 0) (incisives-mand equal proclinees))
         :conclusions '((diagnostic classe-3-dento-alveolaire))
         :cf 0.80 
         :description "Pseudo Classe III") *base-regles*)
         
  ;; Règle A6 avec gestion de l'OU via liste dans premisses (simplification)
  (push (make-regle 
         :id 'R-A6
         :premisses '((age < 10) (stade-croissance member (cs1 cs2 cs3)) (diagnostic member (classe-2-squelettique classe-3-squelettique)))
         :conclusions '((traitement phase-orthopedique))
         :cf 0.85 
         :description "Indication traitement précoce") *base-regles*)

  ;; --- MODULE B : APPAREILLAGE ---

  (push (make-regle 
         :id 'R-B1
         :premisses '((diagnostic equal classe-2-div-1) (age >= 8) (age <= 12) (cooperation equal bonne))
         :conclusions '((appareil fonctionnel))
         :cf 0.80 
         :description "Twin Block ou Activateur pour enfant coopératif") *base-regles*)

  (push (make-regle 
         :id 'R-B2
         :premisses '((diagnostic equal classe-2-div-1) (age > 12) (classe-squelettique equal moderee))
         :conclusions '((appareil headgear-multibagues))
         :cf 0.75 
         :description "FEO pour adolescent") *base-regles*)

  (push (make-regle 
         :id 'R-B3
         :premisses '((diagnostic equal classe-3-squelettique) (age >= 6) (age <= 9) (retromaxillie equal t))
         :conclusions '((appareil masque-delaire))
         :cf 0.85 
         :description "Masque facial pour Cl III jeune") *base-regles*)

  (push (make-regle 
         :id 'R-B4
         :premisses '((diagnostic equal classe-1-mineure) (encombrement >= 2) (encombrement <= 6) (motivation-esthetique equal elevee) (age >= 15))
         :conclusions '((appareil gouttieres-alignement))
         :cf 0.90 
         :description "Invisalign/Aligners pour adulte esthétique") *base-regles*)

  (push (make-regle 
         :id 'R-B5
         :premisses '((arcade-max equal etroite) (age < 14) (beance-transversale equal t))
         :conclusions '((appareil disjoncteur))
         :cf 0.80 
         :description "Expansion rapide maxillaire") *base-regles*)

  ;; Décomposition des OU pour R-B6
  (push (make-regle :id 'R-B6a :premisses '((ancrage equal maximum)) :conclusions '((accessoire minivis)) :cf 0.75 :description "Minivis pour ancrage") *base-regles*)
  (push (make-regle :id 'R-B6b :premisses '((mouvement equal complexe)) :conclusions '((accessoire minivis)) :cf 0.75 :description "Minivis pour mvt complexe") *base-regles*)

  (push (make-regle 
         :id 'R-B7
         :premisses '((statut-traitement equal fini) (antecedent-encombrement equal t))
         :conclusions '((contention-inf fil-colle-3-3))
         :cf 0.85 
         :description "Contention fixe mandibulaire") *base-regles*)

  (push (make-regle 
         :id 'R-B8
         :premisses '((statut-traitement equal fini) (hygiene equal bonne) (cooperation equal suffisante))
         :conclusions '((contention-sup gouttiere-nuit))
         :cf 0.70 
         :description "Contention amovible maxillaire") *base-regles*)

  ;; --- MODULE C : CONTRE-INDICATIONS ---

  (push (make-regle :id 'R-C1a :premisses '((parodontite equal active)) :conclusions '((traitement contre-indique)) :cf 0.95 :description "CI Paro") *base-regles*)
  (push (make-regle :id 'R-C1b :premisses '((perte-attache > 50)) :conclusions '((traitement contre-indique)) :cf 0.95 :description "CI Perte osseuse") *base-regles*)
  
  (push (make-regle :id 'R-C3
         :premisses '((diagnostic equal classe-3-squelettique) (age > 14) (anb < -3))
         :conclusions '((traitement chirurgie-orthognathique))
         :cf 0.80 
         :description "Indication chirurgicale Cl III sévère") *base-regles*)
  
  (push (make-regle 
         :id 'R-C5
         :premisses '((rotation-canine > 45))
         :conclusions '((appareil-interdit aligneurs))
         :cf 0.80 
         :description "Limite technique des aligneurs") *base-regles*)

  ;; Inversion de la liste pour l'ordre de priorité
  (setf *base-regles* (nreverse *base-regles*))
  (format t "~&Base de règles chargée : ~d règles.~%" (length *base-regles*)))

;;; ============================================================================
;;; MODULE 3 : MOTEUR D'INFÉRENCE
;;; ============================================================================

;; --- Outils d'évaluation ---

(defun evaluer-condition (condition)
  "Vérifie si une condition (attribut operateur valeur) est vraie"
  (let* ((attr (first condition))
         (op   (second condition))
         (val-ref (third condition))
         (fait-trouve (valeur-fait attr)))
    
    (if (null fait-trouve)
        :inconnu ;; Le fait n'est pas dans la base
        (let ((val-reelle (fait-valeur fait-trouve)))
          (case op
            (equal  (equal val-reelle val-ref))
            (>      (> val-reelle val-ref))
            (<      (< val-reelle val-ref))
            (>=     (>= val-reelle val-ref))
            (<=     (<= val-reelle val-ref))
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

;;; ============================================================================
;;; MODULE 4 : INTERFACE ET SCÉNARIOS
;;; ============================================================================

(defun poser-questions-base ()
  "Pose les questions initiales essentielles"
  (format t "~%--- COLLECTE DES DONNÉES CLINIQUES ---~%")
  
  (format t "Âge du patient (entier) : ")
  (ajouter-fait 'age (read))
  
  (format t "Relation Molaire (classe-1, classe-2, classe-3) : ")
  (ajouter-fait 'relation-molaire (read))
  
  (format t "Valeur ANB (degrés, ex: 2 ou -4) : ")
  (ajouter-fait 'anb (read))
  
  (format t "Encombrement (mm, ex: 3) : ")
  (ajouter-fait 'encombrement (read))
  
  (format t "Overjet (mm, ex: 6) : ")
  (ajouter-fait 'overjet (read))
  
  (format t "Incisives maxillaires (proclinees, retroclinees, normales) : ")
  (ajouter-fait 'incisives-max (read))
  
  (format t "Coopération prévisible (bonne, moyenne, mauvaise) : ")
  (ajouter-fait 'cooperation (read)))

(defun lancer-diagnostic ()
  (reinitialiser-base)
  (charger-regles)
  (format t "~%=== SYSTÈME EXPERT ORTHODONTIQUE ===~%")
  
  (poser-questions-base)
  
  (format t "~%--- DÉMARRAGE MOTEUR (CHAÎNAGE AVANT) ---~%")
  (chainage-avant)
  
  (format t "~%--- RÉSULTATS DU DIAGNOSTIC ---~%")
  (let ((diag (valeur-fait 'diagnostic))
        (app (valeur-fait 'appareil))
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
;;; TESTS
;;; ============================================================================

(defun test-edge-cases ()
  "Série de tests pour les cas limites (Sécurisé NIL)"
  (format t "~%=== TESTS DE ROBUSTESSE ===~%")
  
  ;; Test 1 : Âges limites
  (format t "~%--- Test 1 : Âges limites ---")
  (dolist (age '(7 8 9 10 11 12 13 14 15 16 30))
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'age age)
    (ajouter-fait 'relation-molaire 'classe-2)
    (ajouter-fait 'incisives-max 'proclinees)
    (ajouter-fait 'overjet 6)
    (ajouter-fait 'cooperation 'bonne)
    (format t "~%Âge ~d : " age)
    (chainage-avant)
    
    (let* ((f-diag (valeur-fait 'diagnostic))
           (f-app  (valeur-fait 'appareil))
           (diag   (if f-diag (fait-valeur f-diag) "Indéterminé"))
           (app    (if f-app  (fait-valeur f-app)  "Aucun")))
      (format t "Diagnostic: ~a, Appareil: ~a" diag app)))
  
  ;; Test 2 : Valeurs numériques limites
  (format t "~%~%--- Test 2 : Valeurs numériques limites ---")
  (dolist (overjet '(4 5 6 7))
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'age 10)
    (ajouter-fait 'relation-molaire 'classe-2)
    (ajouter-fait 'incisives-max 'proclinees)
    (ajouter-fait 'overjet overjet)
    (ajouter-fait 'cooperation 'bonne)
    (format t "~%Overjet ~d : " overjet)
    (chainage-avant)
    (let* ((f-diag (valeur-fait 'diagnostic))
           (diag   (if f-diag (fait-valeur f-diag) "Indéterminé")))
      (format t "Diagnostic: ~a" diag)))
  
  ;; Test 3 : Données manquantes
  (format t "~%~%--- Test 3 : Données manquantes ---")
  (reinitialiser-base)
  (charger-regles)
  (ajouter-fait 'age 10)
  (ajouter-fait 'relation-molaire 'classe-2)
  (format t "~%Données partielles : ")
  (chainage-avant)
  (let* ((f-diag (valeur-fait 'diagnostic))
         (diag   (if f-diag (fait-valeur f-diag) "Indéterminé (OK)")))
    (format t "Diagnostic: ~a" diag))
  
  ;; Test 4 : Valeurs invalides
  (format t "~%~%--- Test 4 : Valeurs invalides ---")
  (reinitialiser-base)
  (charger-regles)
  (ajouter-fait 'age 10)
  (ajouter-fait 'relation-molaire 'classe-4) ; Valeur inexistante
  (ajouter-fait 'incisives-max 'proclinees)
  (ajouter-fait 'overjet 6)
  (format t "~%Relation molaire invalide : ")
  (chainage-avant)
  (let* ((f-diag (valeur-fait 'diagnostic))
         (diag   (if f-diag (fait-valeur f-diag) "Indéterminé (OK)")))
    (format t "Diagnostic: ~a" diag))
  
  ;; Test 5 : Conflits entre règles
  (format t "~%~%--- Test 5 : Conflits potentiels ---")
  (reinitialiser-base)
  (charger-regles)
  (ajouter-fait 'age 10)
  (ajouter-fait 'relation-molaire 'classe-3)
  (ajouter-fait 'anb -2)
  (ajouter-fait 'wits -3)
  (ajouter-fait 'incisives-mand 'proclinees)
  (ajouter-fait 'retromaxillie t)
  (format t "~%Données pour plusieurs diagnostics possibles : ")
  (chainage-avant)
  (let* ((f-diag (valeur-fait 'diagnostic))
         (f-app  (valeur-fait 'appareil))
         (f-trait (valeur-fait 'traitement))
         (diag   (if f-diag (fait-valeur f-diag) "N/A"))
         (app    (if f-app  (fait-valeur f-app)  "N/A"))
         (trait  (if f-trait (fait-valeur f-trait) "N/A")))
    (format t "Diagnostic: ~a, Appareil: ~a, Traitement: ~a" diag app trait))
  
  ;; Test 6 : Conditions multiples avec 'member'
  (format t "~%~%--- Test 6 : Opérateur MEMBER ---")
  (dolist (diag-test '(classe-2-squelettique classe-3-squelettique autre))
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'age 9)
    (ajouter-fait 'stade-croissance 'cs2)
    (ajouter-fait 'diagnostic diag-test)
    (format t "~%Diagnostic entrée ~a : " diag-test)
    (chainage-avant)
    (let* ((f-trait (valeur-fait 'traitement))
           (trait   (if f-trait (fait-valeur f-trait) "Aucun")))
      (format t "Traitement: ~a" trait)))
  
  (format t "~%~%=== FIN DES TESTS ===~%"))

(defun test-scenarios-complexes ()
  "Scénarios cliniques complexes pour tester le raisonnement"
  (format t "~%=== SCÉNARIOS CLINIQUES COMPLEXES ===~%")
  
  ;; Scénario 1
  (format t "~%--- Scénario 1 : Contre-indication ---")
  (reinitialiser-base)
  (charger-regles)
  (ajouter-fait 'age 35)
  (ajouter-fait 'relation-molaire 'classe-1)
  (ajouter-fait 'encombrement 3)
  (ajouter-fait 'parodontite 'active)
  (ajouter-fait 'perte-attache 30)
  (format t "Patient adulte avec parodontite active : ")
  (chainage-avant)
  (let* ((f-ci (valeur-fait 'traitement))
         (ci   (if f-ci (fait-valeur f-ci) nil)))
    (if (equal ci 'contre-indique)
        (format t "✓ Contre-indication correctement détectée")
        (format t "✗ Contre-indication non détectée: ~a" ci)))
  
  ;; Scénario 2
  (format t "~%~%--- Scénario 2 : Options multiples ---")
  (reinitialiser-base)
  (charger-regles)
  (ajouter-fait 'age 16)
  (ajouter-fait 'relation-molaire 'classe-2)
  (ajouter-fait 'incisives-max 'proclinees)
  (ajouter-fait 'overjet 6)
  (ajouter-fait 'classe-squelettique 'moderee)
  (ajouter-fait 'arcade-max 'etroite)
  (ajouter-fait 'beance-transversale t)
  (format t "Adolescent avec plusieurs problèmes : ")
  (chainage-avant)
  (let* ((f-diag (valeur-fait 'diagnostic))
         (f-app  (valeur-fait 'appareil))
         (f-acc  (valeur-fait 'accessoire))
         (diag   (if f-diag (fait-valeur f-diag) "N/A"))
         (app    (if f-app  (fait-valeur f-app)  "N/A"))
         (acc    (if f-acc  (fait-valeur f-acc)  "N/A")))
    (format t "Diagnostic: ~a~%Appareil: ~a~%Accessoire: ~a" diag app acc))
  
  ;; Scénario 3
  (format t "~%~%--- Scénario 3 : Limites techniques ---")
  (reinitialiser-base)
  (charger-regles)
  (ajouter-fait 'age 25)
  (ajouter-fait 'relation-molaire 'classe-1)
  (ajouter-fait 'encombrement 5)
  (ajouter-fait 'motivation-esthetique 'elevee)
  (ajouter-fait 'rotation-canine 50)
  (format t "Adulte souhaitant des aligneurs avec rotation canine sévère : ")
  (chainage-avant)
  (let* ((f-app (valeur-fait 'appareil))
         (f-int (valeur-fait 'appareil-interdit))
         (app   (if f-app (fait-valeur f-app) "N/A"))
         (interdit (if f-int (fait-valeur f-int) "N/A")))
    (format t "Appareil recommandé: ~a~%Appareil interdit: ~a" app interdit))
  
  ;; Scénario 4
  (format t "~%~%--- Scénario 4 : Fin de traitement ---")
  (reinitialiser-base)
  (charger-regles)
  (ajouter-fait 'statut-traitement 'fini)
  (ajouter-fait 'antecedent-encombrement t)
  (ajouter-fait 'hygiene 'bonne)
  (ajouter-fait 'cooperation 'suffisante)
  (format t "Patient en fin de traitement : ")
  (chainage-avant)
  (let* ((f-inf (valeur-fait 'contention-inf))
         (f-sup (valeur-fait 'contention-sup))
         (inf   (if f-inf (fait-valeur f-inf) "N/A"))
         (sup   (if f-sup (fait-valeur f-sup) "N/A")))
    (format t "Contention inférieure: ~a~%Contention supérieure: ~a" inf sup))
  
  (format t "~%~%=== FIN DES SCÉNARIOS ===~%"))

(defun test-performance-stabilite ()
  "Tests de performance et vérification de la stabilité"
  (format t "~%=== TESTS DE PERFORMANCE ET STABILITÉ ===~%")
  
  ;; Test 1 : Boucle infinie potentielle
  (format t "~%--- Test 1 : Vérification absence de boucle infinie ---")
  (reinitialiser-base)
  (charger-regles)
  (ajouter-fait 'age 10)
  (ajouter-fait 'relation-molaire 'classe-2)
  (ajouter-fait 'incisives-max 'proclinees)
  (ajouter-fait 'overjet 6)
  (ajouter-fait 'cooperation 'bonne)
  (ajouter-fait 'diagnostic 'classe-2-div-1) ; Déjà présent
  (time (chainage-avant))
  (format t "✓ Chaînage avant terminé sans boucle infinie")
  
  ;; Test 2 : Règles mutuellement exclusives
  (format t "~%~%--- Test 2 : Règles mutuellement exclusives ---")
  (reinitialiser-base)
  (charger-regles)
  (ajouter-fait 'age 10)
  (ajouter-fait 'relation-molaire 'classe-1)
  (ajouter-fait 'encombrement 3)
  (ajouter-fait 'motivation-esthetique 'elevee)
  (time (chainage-avant))
  
  (let* ((f-diag (valeur-fait 'diagnostic))
         (f-app  (valeur-fait 'appareil))
         (diag   (if f-diag (fait-valeur f-diag) "N/A"))
         (app    (if f-app  (fait-valeur f-app)  "Aucun"))
         )
    (format t "Résultat unique? Diagnostic: ~a, Appareil: ~a" diag app))
  
  ;; Test 3 : Base de faits vide
  (format t "~%~%--- Test 3 : Base de faits vide ---")
  (reinitialiser-base)
  (charger-regles)
  (time (chainage-avant))
  (format t "✓ Système gère correctement une base vide")
  
  ;; Test 4 : Multiple exécutions
  (format t "~%~%--- Test 4 : Répétabilité ---")
  (dotimes (i 3)
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'age 10)
    (ajouter-fait 'relation-molaire 'classe-2)
    (ajouter-fait 'incisives-max 'proclinees)
    (ajouter-fait 'overjet 6)
    (chainage-avant)
    (let* ((f-diag (valeur-fait 'diagnostic))
           (diag   (if f-diag (fait-valeur f-diag) "N/A")))
      (format t "Exécution ~d: Diagnostic: ~a~%" (1+ i) diag)))
  
  (format t "~%~%=== FIN DES TESTS PERFORMANCE ===~%"))

(defun test-integration-complet ()
  "Test d'intégration complet avec vérification des sorties"
  (format t "~%=== TEST D'INTÉGRATION COMPLET ===~%")
  
  ;; Test toutes les règles une par une
  (let ((tests-reussis 0)
        (tests-total 0))
    
    ;; R-A1 : Classe I avec encombrement mineur
    (incf tests-total)
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'relation-molaire 'classe-1)
    (ajouter-fait 'encombrement 3)
    (chainage-avant)
    (let ((f (valeur-fait 'diagnostic)))
      (when (and f (equal (fait-valeur f) 'classe-1-mineure))
        (incf tests-reussis)
        (format t "✓ R-A1 OK~%")))
    
    ;; R-A2 : Classe II division 1
    (incf tests-total)
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'relation-molaire 'classe-2)
    (ajouter-fait 'incisives-max 'proclinees)
    (ajouter-fait 'overjet 6)
    (chainage-avant)
    (let ((f (valeur-fait 'diagnostic)))
      (when (and f (equal (fait-valeur f) 'classe-2-div-1))
        (incf tests-reussis)
        (format t "✓ R-A2 OK~%")))
    
    ;; R-A3 : Classe II division 2
    (incf tests-total)
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'relation-molaire 'classe-2)
    (ajouter-fait 'incisives-centrales 'retroclinees)
    (ajouter-fait 'overbite 5)
    (chainage-avant)
    (let ((f (valeur-fait 'diagnostic)))
      (when (and f (equal (fait-valeur f) 'classe-2-div-2))
        (incf tests-reussis)
        (format t "✓ R-A3 OK~%")))
    
    ;; R-A4 : Classe III squelettique
    (incf tests-total)
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'relation-molaire 'classe-3)
    (ajouter-fait 'anb -1)
    (ajouter-fait 'wits -3)
    (chainage-avant)
    (let ((f (valeur-fait 'diagnostic)))
      (when (and f (equal (fait-valeur f) 'classe-3-squelettique))
        (incf tests-reussis)
        (format t "✓ R-A4 OK~%")))
    
    ;; R-A5 : Pseudo Classe III
    (incf tests-total)
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'relation-molaire 'classe-3)
    (ajouter-fait 'anb 1)
    (ajouter-fait 'incisives-mand 'proclinees)
    (chainage-avant)
    (let ((f (valeur-fait 'diagnostic)))
      (when (and f (equal (fait-valeur f) 'classe-3-dento-alveolaire))
        (incf tests-reussis)
        (format t "✓ R-A5 OK~%")))
    
    ;; R-B1 : Appareil fonctionnel
    (incf tests-total)
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'diagnostic 'classe-2-div-1)
    (ajouter-fait 'age 10)
    (ajouter-fait 'cooperation 'bonne)
    (chainage-avant)
    (let ((f (valeur-fait 'appareil)))
      (when (and f (equal (fait-valeur f) 'fonctionnel))
        (incf tests-reussis)
        (format t "✓ R-B1 OK~%")))
    
    ;; R-B3 : Masque facial
    (incf tests-total)
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'diagnostic 'classe-3-squelettique)
    (ajouter-fait 'age 8)
    (ajouter-fait 'retromaxillie t)
    (chainage-avant)
    (let ((f (valeur-fait 'appareil)))
      (when (and f (equal (fait-valeur f) 'masque-delaire))
        (incf tests-reussis)
        (format t "✓ R-B3 OK~%")))
    
    ;; R-C1a : Contre-indication parodontite
    (incf tests-total)
    (reinitialiser-base)
    (charger-regles)
    (ajouter-fait 'parodontite 'active)
    (chainage-avant)
    (let ((f (valeur-fait 'traitement)))
      (when (and f (equal (fait-valeur f) 'contre-indique))
        (incf tests-reussis)
        (format t "✓ R-C1a OK~%")))
    
    ;; Calcul du score
    (let ((pourcentage (* 100 (/ tests-reussis tests-total))))
      (format t "~%=== RÉSULTATS ===~%")
      (format t "Tests réussis: ~d/~d (~,1f%)~%" 
              tests-reussis tests-total pourcentage)
      (when (= tests-reussis tests-total)
        (format t "✓ TOUS LES TESTS ONT RÉUSSI !~%")))))

(defun menu-tests ()
  "Menu principal pour exécuter les différents tests"
  (loop
    (format t "~%=== MENU DE TESTS DE ROBUSTESSE ===~%")
    (format t "1. Tests de cas limites~%")
    (format t "2. Scénarios cliniques complexes~%")
    (format t "3. Tests de performance et stabilité~%")
    (format t "4. Test d'intégration complet~%")
    (format t "5. Tous les tests~%")
    (format t "6. Retour au menu principal~%")
    (format t "7. Quitter~%~%")
    (format t "Choix (1-7) : ")
    
    (case (read)
      (1 (test-edge-cases))
      (2 (test-scenarios-complexes))
      (3 (test-performance-stabilite))
      (4 (test-integration-complet))
      (5 (progn
           (test-edge-cases)
           (test-scenarios-complexes)
           (test-performance-stabilite)
           (test-integration-complet)))
      (6 (return-from menu-tests))
      (7 (progn
           (format t "Au revoir!~%")
           (quit)))
      (otherwise (format t "Choix invalide!~%")))))


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
