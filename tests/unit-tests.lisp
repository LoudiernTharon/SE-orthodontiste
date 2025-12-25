(defpackage :se-orthodontiste.tests;;; ============================================================================

  (:use :cl :se-orthodontiste.engine :se-orthodontiste.rules);;; TESTS

  (:export #:menu-tests #:test-cas-classe-2));;; ============================================================================



(in-package :se-orthodontiste.tests)(defun test-edge-cases ()

  "Série de tests pour les cas limites (Sécurisé NIL)"

;;; ============================================================================  (format t "~%=== TESTS DE ROBUSTESSE ===~%")

;;; TESTS  

;;; ============================================================================  ;; Test 1 : Âges limites

  (format t "~%--- Test 1 : Âges limites ---")

(defun test-cas-classe-2 ()  (dolist (age '(7 8 9 10 11 12 13 14 15 16 30))

  "Simule un patient Classe II Div 1 de 10 ans"    (reinitialiser-base)

  (reinitialiser-base)    (charger-regles)

  (charger-regles)    (ajouter-fait 'age age)

      (ajouter-fait 'relation-molaire 'classe-2)

  (ajouter-fait 'age 10)    (ajouter-fait 'incisives-max 'proclinees)

  (ajouter-fait 'relation-molaire 'classe-2)    (ajouter-fait 'overjet 6)

  (ajouter-fait 'incisives-max 'proclinees)    (ajouter-fait 'cooperation 'bonne)

  (ajouter-fait 'overjet 6)    (format t "~%Âge ~d : " age)

  (ajouter-fait 'cooperation 'bonne)    (chainage-avant)

      

  (format t "~%--- LANCEMENT TEST CLASSE II ---~%")    (let* ((f-diag (valeur-fait 'diagnostic))

  (chainage-avant)           (f-app  (valeur-fait 'appareil))

             (diag   (if f-diag (fait-valeur f-diag) "Indéterminé"))

  (format t "~%Vérification : ~%")           (app    (if f-app  (fait-valeur f-app)  "Aucun")))

  (let ((diag (valeur-fait 'diagnostic))      (format t "Diagnostic: ~a, Appareil: ~a" diag app)))

        (app (valeur-fait 'appareil)))  

    (if diag (format t "Diagnostic trouvé : ~a~%" (fait-valeur diag))  ;; Test 2 : Valeurs numériques limites

             (format t "Diagnostic : Indéterminé~%"))  (format t "~%~%--- Test 2 : Valeurs numériques limites ---")

    (if app  (format t "Appareil trouvé : ~a~%" (fait-valeur app))  (dolist (overjet '(4 5 6 7))

             (format t "Appareil : Aucun~%"))))    (reinitialiser-base)

    (charger-regles)

(defun test-edge-cases ()    (ajouter-fait 'age 10)

  "Série de tests pour les cas limites (Sécurisé NIL)"    (ajouter-fait 'relation-molaire 'classe-2)

  (format t "~%=== TESTS DE ROBUSTESSE ===~%")    (ajouter-fait 'incisives-max 'proclinees)

      (ajouter-fait 'overjet overjet)

  ;; Test 1 : Âges limites    (ajouter-fait 'cooperation 'bonne)

  (format t "~%--- Test 1 : Âges limites ---")    (format t "~%Overjet ~d : " overjet)

  (dolist (age '(7 8 9 10 11 12 13 14 15 16 30))    (chainage-avant)

    (reinitialiser-base)    (let* ((f-diag (valeur-fait 'diagnostic))

    (charger-regles)           (diag   (if f-diag (fait-valeur f-diag) "Indéterminé")))

    (ajouter-fait 'age age)      (format t "Diagnostic: ~a" diag)))

    (ajouter-fait 'relation-molaire 'classe-2)  

    (ajouter-fait 'incisives-max 'proclinees)  ;; Test 3 : Données manquantes

    (ajouter-fait 'overjet 6)  (format t "~%~%--- Test 3 : Données manquantes ---")

    (ajouter-fait 'cooperation 'bonne)  (reinitialiser-base)

    (format t "~%Âge ~d : " age)  (charger-regles)

    (chainage-avant)  (ajouter-fait 'age 10)

      (ajouter-fait 'relation-molaire 'classe-2)

    (let* ((f-diag (valeur-fait 'diagnostic))  (format t "~%Données partielles : ")

           (f-app  (valeur-fait 'appareil))  (chainage-avant)

           (diag   (if f-diag (fait-valeur f-diag) "Indéterminé"))  (let* ((f-diag (valeur-fait 'diagnostic))

           (app    (if f-app  (fait-valeur f-app)  "Aucun")))         (diag   (if f-diag (fait-valeur f-diag) "Indéterminé (OK)")))

      (format t "Diagnostic: ~a, Appareil: ~a" diag app)))    (format t "Diagnostic: ~a" diag))

    

  ;; Test 2 : Valeurs numériques limites  ;; Test 4 : Valeurs invalides

  (format t "~%~%--- Test 2 : Valeurs numériques limites ---")  (format t "~%~%--- Test 4 : Valeurs invalides ---")

  (dolist (overjet '(4 5 6 7))  (reinitialiser-base)

    (reinitialiser-base)  (charger-regles)

    (charger-regles)  (ajouter-fait 'age 10)

    (ajouter-fait 'age 10)  (ajouter-fait 'relation-molaire 'classe-4) ; Valeur inexistante

    (ajouter-fait 'relation-molaire 'classe-2)  (ajouter-fait 'incisives-max 'proclinees)

    (ajouter-fait 'incisives-max 'proclinees)  (ajouter-fait 'overjet 6)

    (ajouter-fait 'overjet overjet)  (format t "~%Relation molaire invalide : ")

    (ajouter-fait 'cooperation 'bonne)  (chainage-avant)

    (format t "~%Overjet ~d : " overjet)  (let* ((f-diag (valeur-fait 'diagnostic))

    (chainage-avant)         (diag   (if f-diag (fait-valeur f-diag) "Indéterminé (OK)")))

    (let* ((f-diag (valeur-fait 'diagnostic))    (format t "Diagnostic: ~a" diag))

           (diag   (if f-diag (fait-valeur f-diag) "Indéterminé")))  

      (format t "Diagnostic: ~a" diag)))  ;; Test 5 : Conflits entre règles

    (format t "~%~%--- Test 5 : Conflits potentiels ---")

  ;; Test 3 : Données manquantes  (reinitialiser-base)

  (format t "~%~%--- Test 3 : Données manquantes ---")  (charger-regles)

  (reinitialiser-base)  (ajouter-fait 'age 10)

  (charger-regles)  (ajouter-fait 'relation-molaire 'classe-3)

  (ajouter-fait 'age 10)  (ajouter-fait 'anb -2)

  (ajouter-fait 'relation-molaire 'classe-2)  (ajouter-fait 'wits -3)

  (format t "~%Données partielles : ")  (ajouter-fait 'incisives-mand 'proclinees)

  (chainage-avant)  (ajouter-fait 'retromaxillie t)

  (format t "Terminé (pas de crash attendu)~%"))  (format t "~%Données pour plusieurs diagnostics possibles : ")

  (chainage-avant)

;; Placeholder functions for other tests mentioned in menu-tests  (let* ((f-diag (valeur-fait 'diagnostic))

(defun test-scenarios-complexes () (format t "Test scénarios complexes non implémenté~%"))         (f-app  (valeur-fait 'appareil))

(defun test-performance-stabilite () (format t "Test performance non implémenté~%"))         (f-trait (valeur-fait 'traitement))

(defun test-integration-complet () (format t "Test intégration non implémenté~%"))         (diag   (if f-diag (fait-valeur f-diag) "N/A"))

         (app    (if f-app  (fait-valeur f-app)  "N/A"))

(defun menu-tests ()         (trait  (if f-trait (fait-valeur f-trait) "N/A")))

  "Menu principal pour exécuter les différents tests"    (format t "Diagnostic: ~a, Appareil: ~a, Traitement: ~a" diag app trait))

  (loop  

    (format t "~%=== MENU DE TESTS DE ROBUSTESSE ===~%")  ;; Test 6 : Conditions multiples avec 'member'

    (format t "1. Tests de cas limites~%")  (format t "~%~%--- Test 6 : Opérateur MEMBER ---")

    (format t "2. Scénarios cliniques complexes~%")  (dolist (diag-test '(classe-2-squelettique classe-3-squelettique autre))

    (format t "3. Tests de performance et stabilité~%")    (reinitialiser-base)

    (format t "4. Test d'intégration complet~%")    (charger-regles)

    (format t "5. Tous les tests~%")    (ajouter-fait 'age 9)

    (format t "6. Retour au menu principal~%")    (ajouter-fait 'stade-croissance 'cs2)

    (format t "7. Quitter~%~%")    (ajouter-fait 'diagnostic diag-test)

    (format t "Choix (1-7) : ")    (format t "~%Diagnostic entrée ~a : " diag-test)

        (chainage-avant)

    (case (read)    (let* ((f-trait (valeur-fait 'traitement))

      (1 (test-edge-cases))           (trait   (if f-trait (fait-valeur f-trait) "Aucun")))

      (2 (test-scenarios-complexes))      (format t "Traitement: ~a" trait)))

      (3 (test-performance-stabilite))  

      (4 (test-integration-complet))  (format t "~%~%=== FIN DES TESTS ===~%"))

      (5 (progn

           (test-edge-cases)(defun test-scenarios-complexes ()

           (test-scenarios-complexes)  "Scénarios cliniques complexes pour tester le raisonnement"

           (test-performance-stabilite)  (format t "~%=== SCÉNARIOS CLINIQUES COMPLEXES ===~%")

           (test-integration-complet)))  

      (6 (return-from menu-tests))  ;; Scénario 1

      (7 (progn  (format t "~%--- Scénario 1 : Contre-indication ---")

           (format t "Au revoir!~%")  (reinitialiser-base)

           (quit)))  (charger-regles)

      (otherwise (format t "Choix invalide!~%")))))  (ajouter-fait 'age 35)

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

