;;; Civil 3D Label Debug Tool
;;; This script helps identify Civil 3D label types in the drawing
;;; Usage: Load the script and run the command C3D-LABEL-DEBUG

(defun c:C3D-LABEL-DEBUG ( / ss ent entdata acadObj)
  (princ "\n=== Civil 3D Environment Check ===")
  
  ;; Check if we're in Civil 3D
  (setq acadObj (vlax-get-acad-object))
  (if (and acadObj 
           (vlax-property-available-p acadObj 'ActiveDocument)
           (vlax-property-available-p (vlax-get-property acadObj 'ActiveDocument) 'ModelSpace))
    (progn
      (princ "\nAutoCAD/Civil 3D detected.")
      (if (vl-position "AeccXUiLand.dll" (arx))
        (princ "\nCivil 3D extensions are loaded.")
        (princ "\nWARNING: Civil 3D extensions not detected! Are you running this in Civil 3D?")
      )
    )
    (princ "\nWARNING: Could not verify AutoCAD/Civil 3D environment!")
  )
  
  (princ "\n\n=== Entity Analysis ===")
  (princ "\nSearching for entities...")
  
  ;; Try to find any entity
  (setq ss (ssget "_X"))
  (if ss
    (progn
      (setq count (sslength ss))
      (princ (strcat "\nFound " (itoa count) " total entities."))
      (princ "\nAnalyzing entity types...")
      
      (setq entity-types '())
      (setq detailed-info '())
      
      ;; First pass - collect all entity types and details
      (repeat count
        (setq count (1- count))
        (setq ent (ssname ss count))
        (if (and ent (setq entdata (entget ent)))
          (progn
            (setq etype (cdr (assoc 0 entdata)))
            (if (not (member etype entity-types))
              (progn
                (setq entity-types (cons etype entity-types))
                ;; Try to get more info about the entity
                (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
                (if (and obj (not (vl-catch-all-error-p obj)))
                  (setq detailed-info 
                    (cons 
                      (list 
                        etype 
                        (vl-catch-all-apply 'vla-get-objectname (list obj))
                      )
                      detailed-info
                    )
                  )
                )
              )
            )
          )
        )
      )
      
      ;; Sort entity types alphabetically
      (setq entity-types (vl-sort entity-types '<))
      
      (princ "\n\nFound the following entity types:")
      (foreach type entity-types
        (if (or (wcmatch type "*LABEL*")
                (wcmatch type "AECC*")
                (wcmatch type "TEXT")
                (wcmatch type "*TEXT*")
                (wcmatch type "*ANNO*")
                (wcmatch type "*PIPE*")
                (wcmatch type "*STRUCT*"))
          ;; Highlight potential label-related types
          (progn
            (princ (strcat "\n ** " type " **"))
            ;; Try to show additional info
            (foreach info detailed-info
              (if (= (car info) type)
                (if (and (cadr info) (not (vl-catch-all-error-p (cadr info))))
                  (princ (strcat " - Object Name: " (cadr info)))
                )
              )
            )
          )
          (princ (strcat "\n    " type))
        )
      )
      
      (princ "\n\n=== Troubleshooting Info ===")
      (princ "\n1. Entity types marked with ** might be related to labels")
      (princ "\n2. If no Civil 3D types are found, check if:")
      (princ "\n   - You are running this in Civil 3D (not regular AutoCAD)")
      (princ "\n   - The drawing contains Civil 3D objects")
      (princ "\n   - Civil 3D extensions are properly loaded")
      (princ "\n3. Common Civil 3D prefixes:")
      (princ "\n   - AeccDb: Civil 3D database objects")
      (princ "\n   - Aecc: General Civil 3D objects")
      
    )
    (princ "\nNo entities found in drawing.")
  )
  (princ)
)

(princ "\nEnhanced Civil 3D Label Debug Tool loaded. Type C3D-LABEL-DEBUG to analyze entities.")
(princ) 