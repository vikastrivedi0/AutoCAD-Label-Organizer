;;; Civil 3D Label Debug Tool
;;; This script helps identify Civil 3D label types in the drawing
;;; Usage: Load the script and run the command C3D-LABEL-DEBUG

(defun c:C3D-LABEL-DEBUG ( / ss ent entdata )
  (princ "\nSearching for Civil 3D labels...")
  
  ;; Try to find any entity that might be a label
  (setq ss (ssget "_X"))
  (if ss
    (progn
      (setq count (sslength ss))
      (princ (strcat "\nFound " (itoa count) " total entities."))
      (princ "\nAnalyzing entities for label types...")
      
      (setq label-types '())
      
      (repeat count
        (setq count (1- count))
        (setq ent (ssname ss count))
        (if (and ent (setq entdata (entget ent)))
          (progn
            (setq etype (cdr (assoc 0 entdata)))
            (if (wcmatch etype "AeccDb*Label*")
              (if (not (member etype label-types))
                (setq label-types (cons etype label-types))
              )
            )
          )
        )
      )
      
      (if label-types
        (progn
          (princ "\n\nFound the following Civil 3D label types:")
          (foreach type label-types
            (princ (strcat "\n - " type))
          )
        )
        (princ "\n\nNo Civil 3D label types found in drawing.")
      )
      
      (princ "\n\nCommon Civil 3D label types to look for:")
      (princ "\n - AeccDbPipeLabel (Pipe Labels)")
      (princ "\n - AeccDbStructureLabel (Structure Labels)")
      (princ "\n - AeccDbAlignmentLabel (Alignment Labels)")
      (princ "\n - AeccDbSurfaceLabel (Surface Labels)")
      (princ "\n - AeccDbPointLabel (Point Labels)")
    )
    (princ "\nNo entities found in drawing.")
  )
  (princ)
)

(princ "\nCivil 3D Label Debug Tool loaded. Type C3D-LABEL-DEBUG to analyze labels.")
(princ) 