;; Civil 3D Label Overlap Detector using Entity Data
;; This script detects overlapping Civil 3D labels by accessing entity data directly

(vl-load-com)

;; Function to get entity properties using DXF group codes
(defun get-entity-data (ent / entdata)
  (setq entdata (entget ent))
  (if entdata
    (list
      (cons "Handle" (cdr (assoc 5 entdata)))
      (cons "EntityName" (cdr (assoc 0 entdata)))
      (cons "Layer" (cdr (assoc 8 entdata)))
      (cons "Position" (cdr (assoc 10 entdata)))
    )
    nil
  )
)

;; Function to get label extents using zoom
(defun get-label-extents (ent / bbox)
  (command "._zoom" "_object" ent "")
  (setq bbox (list (getvar "viewctr")
                   (getvar "viewsize")))
  (command "._zoom" "_previous")
  bbox
)

;; Function to process Civil 3D label
(defun process-c3d-label (ent / props extents)
  (setq props (get-entity-data ent))
  (if props
    (progn
      (princ (strcat "\nProcessing " (cdr (assoc "EntityName" props)) "..."))
      (princ (strcat "\n  Handle: " (cdr (assoc "Handle" props))))
      (princ (strcat "\n  Layer: " (cdr (assoc "Layer" props))))
      (if (cdr (assoc "Position" props))
        (princ (strcat "\n  Position: " 
                      (vl-princ-to-string (cdr (assoc "Position" props)))))
      )
      (setq extents (get-label-extents ent))
      (if extents
        (progn
          (princ "\n  Extents:")
          (princ (strcat "\n    Center: " (vl-princ-to-string (car extents))))
          (princ (strcat "\n    Size: " (rtos (cadr extents) 2 2)))
        )
      )
    )
  ))

;; Main command to analyze Civil 3D labels
(defun c:C3D-ANALYZE-LABELS ( / ss-pipe ss-struct label-count)
  (princ "\n=== Civil 3D Label Analysis ===")
  
  ;; Get selection sets for both label types
  (setq ss-pipe (ssget "_X" '((0 . "AECC_PIPE_LABEL")))
        ss-struct (ssget "_X" '((0 . "AECC_STRUCTURE_LABEL")))
        label-count 0)
  
  (if (or ss-pipe ss-struct)
    (progn
      ;; Process Pipe Labels
      (if ss-pipe
        (progn
          (princ (strcat "\n\nProcessing " (itoa (sslength ss-pipe)) " Pipe Labels..."))
          (setq count (sslength ss-pipe))
          (while (> count 0)
            (setq count (1- count))
            (process-c3d-label (ssname ss-pipe count))
            (setq label-count (1+ label-count))
          )
        )
      )

      ;; Process Structure Labels
      (if ss-struct
        (progn
          (princ (strcat "\n\nProcessing " (itoa (sslength ss-struct)) " Structure Labels..."))
          (setq count (sslength ss-struct))
          (while (> count 0)
            (setq count (1- count))
            (process-c3d-label (ssname ss-struct count))
            (setq label-count (1+ label-count))
          )
        )
      )

      (princ (strcat "\n\nTotal labels processed: " (itoa label-count)))
    )
    (princ "\nNo Civil 3D labels found in drawing.")
  )
  
  (princ "\n=== Analysis Complete ===")
  (princ)
)

;; Function to check if two labels overlap
(defun check-label-overlap (ent1 ent2 / ext1 ext2)
  (setq ext1 (get-label-extents ent1)
        ext2 (get-label-extents ent2))
  (if (and ext1 ext2)
    (progn
      (setq ctr1 (car ext1)
            size1 (cadr ext1)
            ctr2 (car ext2)
            size2 (cadr ext2))
      ;; Check if centers are closer than combined sizes
      (< (distance ctr1 ctr2) (* 0.5 (+ size1 size2)))
    )
    nil
  )
)

;; Function to calculate new position for overlapping label
(defun calculate-new-position (pos1 pos2 offset-distance / angle dx dy)
  (setq angle (angle pos1 pos2))
  (setq dx (* offset-distance (cos (+ angle (/ pi 2)))))
  (setq dy (* offset-distance (sin (+ angle (/ pi 2)))))
  (list (+ (car pos1) dx)
        (+ (cadr pos1) dy)
        (caddr pos1))
)

;; Function to move label to new position
(defun move-label (ent new-pos / old-pos)
  (setq old-pos (cdr (assoc 10 (entget ent))))
  (if (and old-pos new-pos)
    (command "._move" ent "" old-pos new-pos))
)

;; Function to resolve overlap between two labels
(defun resolve-overlap (ent1 ent2 / obj1 obj2 pos1 pos2 new-pos)
  (princ "\nDebug: Attempting to resolve overlap...")
  
  ;; Convert entities to ActiveX objects
  (setq obj1 (vlax-ename->vla-object ent1)
        obj2 (vlax-ename->vla-object ent2))
  
  (if (and obj1 obj2)
    (progn
      ;; Print available properties and methods
      (princ "\nDebug: Available properties for Label 1:")
      (vlax-for prop (vlax-invoke obj1 'GetPropertyNames)
        (princ (strcat "\n  " (vl-princ-to-string prop)))
      )
      
      ;; Get positions using Civil 3D API
      (setq pos1 (vlax-invoke obj1 'GetPosition)
            pos2 (vlax-invoke obj2 'GetPosition))
      
      (princ (strcat "\nDebug: Label 1 position: " (vl-princ-to-string pos1)))
      (princ (strcat "\nDebug: Label 2 position: " (vl-princ-to-string pos2)))
      
      (if (and pos1 pos2)
        (progn
          ;; Calculate new position for first label
          (setq new-pos (calculate-new-position pos1 pos2 15.0))
          (princ (strcat "\nDebug: New position: " (vl-princ-to-string new-pos)))
          
          ;; Move the first label using Civil 3D API
          (vlax-invoke obj1 'SetPosition new-pos)
          (princ "\nDebug: Move command executed")
          
          T  ; Return success
        )
        (progn
          (princ "\nDebug: Could not get valid positions for labels")
          nil  ; Return failure
        )
      )
    )
    (progn
      (princ "\nDebug: Could not convert entities to ActiveX objects")
      nil  ; Return failure
    )
  )
)

;; Function to find first overlapping pair
(defun find-first-overlap (ss1 ss2 / i j found)
  (setq found nil)
  (if (and ss1 ss2)
    (progn
      (setq i 0)
      (while (and (< i (sslength ss1)) (not found))
        (setq j 0)
        (while (and (< j (sslength ss2)) (not found))
          ;; Skip if comparing the same label with itself
          (if (and (not (eq (ssname ss1 i) (ssname ss2 j)))
                   (check-label-overlap (ssname ss1 i) (ssname ss2 j)))
            (setq found (list (ssname ss1 i) (ssname ss2 j)))
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )
      found
    )
    nil
  )
)

;; Command to find and fix one overlapping pair at a time
(defun c:C3D-FIX-ONE-OVERLAP ( / ss-pipe ss-struct overlap-pair)
  (princ "\n=== Finding Next Overlapping Labels ===")
  
  ;; Get selection sets for both label types
  (setq ss-pipe (ssget "_X" '((0 . "AECC_PIPE_LABEL")))
        ss-struct (ssget "_X" '((0 . "AECC_STRUCTURE_LABEL"))))
  
  (if (or ss-pipe ss-struct)
    (progn
      ;; Find first overlap between pipe labels
      (if ss-pipe
        (progn
          (princ (strcat "\nChecking " (itoa (sslength ss-pipe)) " Pipe Labels..."))
          (setq overlap-pair (find-first-overlap ss-pipe ss-pipe))
        )
      )

      ;; If no pipe overlap found, check structure labels
      (if (and (not overlap-pair) ss-struct)
        (progn
          (princ (strcat "\nChecking " (itoa (sslength ss-struct)) " Structure Labels..."))
          (setq overlap-pair (find-first-overlap ss-struct ss-struct))
        )
      )

      ;; If still no overlap found, check pipe vs structure
      (if (and (not overlap-pair) ss-pipe ss-struct)
        (progn
          (princ "\nChecking Pipe Labels against Structure Labels...")
          (setq overlap-pair (find-first-overlap ss-pipe ss-struct))
        )
      )

      ;; If overlap found, show details and ask for confirmation
      (if overlap-pair
        (progn
          (princ "\nFound overlapping labels:")
          (princ (strcat "\nLabel 1: " (vl-princ-to-string (car overlap-pair))))
          (princ (strcat "\nLabel 2: " (vl-princ-to-string (cadr overlap-pair))))
          
          ;; Highlight the overlapping labels
          (command "._select" (car overlap-pair) (cadr overlap-pair) "")
          
          ;; Ask for confirmation
          (initget "Yes No")
          (setq response (getkword "\nFix this overlap? [Yes/No] <Yes>: "))
          
          (if (or (not response) (= response "Yes"))
            (progn
              (if (resolve-overlap (car overlap-pair) (cadr overlap-pair))
                (princ "\nOverlap fixed.")
                (princ "\nCould not fix overlap.")
              )
            )
            (princ "\nSkipped fixing overlap.")
          )
        )
        (princ "\nNo overlapping labels found.")
      )
    )
    (princ "\nNo Civil 3D labels found in drawing.")
  )
  
  (princ "\n=== Overlap Check Complete ===")
  (princ)
)

;; Command to move a pipe label by offset
(defun c:C3D-MOVE-PIPE-LABEL ( / ss ent entdata pos new-pos offset)
  (princ "\n=== Moving Pipe Label ===")
  (vl-load-com)
  
  (setq offset 20.0)  ; Set the offset value
  
  ;; Get selection set for pipe labels
  (setq ss (ssget '((0 . "AECC_PIPE_LABEL"))))
  
  (if ss
    (progn
      (setq ent (ssname ss 0))
      (setq entdata (entget ent))
      
      (if entdata
        (progn
          ;; Print all DXF codes for debugging
          (princ "\nAll DXF codes:")
          (foreach pair entdata
            (princ (strcat "\n  " (vl-princ-to-string (car pair)) ": " (vl-princ-to-string (cdr pair))))
          )
          
          ;; Try to get position from different sources
          (princ "\n\nTrying to get position:")
          
          ;; Try different DXF codes that might contain position data
          (setq pos nil)
          
          ;; Try DXF 10 (primary position)
          (if (not pos)
            (progn
              (setq pos (cdr (assoc 10 entdata)))
              (if pos (princ (strcat "\n  Position from DXF 10: " (vl-princ-to-string pos))))
            )
          )
          
          ;; Try DXF 11 (secondary position)
          (if (not pos)
            (progn
              (setq pos (cdr (assoc 11 entdata)))
              (if pos (princ (strcat "\n  Position from DXF 11: " (vl-princ-to-string pos))))
            )
          )
          
          ;; Try DXF 12 (tertiary position)
          (if (not pos)
            (progn
              (setq pos (cdr (assoc 12 entdata)))
              (if pos (princ (strcat "\n  Position from DXF 12: " (vl-princ-to-string pos))))
            )
          )
          
          ;; Try DXF 13 (fourth position)
          (if (not pos)
            (progn
              (setq pos (cdr (assoc 13 entdata)))
              (if pos (princ (strcat "\n  Position from DXF 13: " (vl-princ-to-string pos))))
            )
          )
          
          ;; If no position found in main entity, try to get it from the parent entity
          (if (not pos)
            (progn
              (setq parent-ref (cdr (assoc 330 entdata)))
              (if parent-ref
                (progn
                  (princ (strcat "\n  Parent entity reference: " (vl-princ-to-string parent-ref)))
                  ;; Try to get the parent entity's data directly
                  (setq parent-data (entget parent_ref))
                  (if parent-data
                    (progn
                      (princ "\n  Parent entity data:")
                      (foreach pair parent-data
                        (princ (strcat "\n    " (vl-princ-to-string (car pair)) ": " (vl-princ-to-string (cdr pair))))
                      )
                      ;; Try to get position from parent entity's DXF codes
                      (setq pos (cdr (assoc 10 parent-data)))
                      (if pos (princ (strcat "\n  Position from parent DXF 10: " (vl-princ-to-string pos))))
                    )
                    (princ "\n  Could not get parent entity data")
                  )
                )
                (princ "\n  No parent entity reference found")
              )
            )
          )
          
          ;; If still no position found, try to get it from the label's extents
          (if (not pos)
            (progn
              (princ "\nTrying to get position from label extents:")
              (setq extents (get-label-extents ent))
              (if extents
                (progn
                  (setq pos (car extents))
                  (princ (strcat "\n  Position from extents: " (vl-princ-to-string pos)))
                )
                (princ "\n  Could not get label extents")
              )
            )
          )
          
          (if pos
            (progn
              (princ (strcat "\nUsing position: " (vl-princ-to-string pos)))
              
              ;; Calculate new position (offset in X direction)
              (setq new-pos (list (+ (car pos) offset)
                                 (cadr pos)
                                 (caddr pos)))
              
              ;; Move the label using the MOVE command
              (command "._move" ent "" pos new-pos)
              (princ (strcat "\nLabel moved by " (rtos offset) " units."))
            )
            (princ "\nCould not find any valid position for the label.")
          )
        )
        (princ "\nCould not get entity data.")
      )
    )
    (princ "\nNo pipe label selected.")
  )
  
  (princ)
)

;; Function to get common Civil 3D label properties
(defun get-label-properties (obj / props)
  (setq props (list
    (cons "ObjectName" (vlax-get obj 'ObjectName))
    (cons "Layer" (vlax-get obj 'Layer))
    (cons "Handle" (vlax-get obj 'Handle))
    (cons "Visible" (vlax-get obj 'Visible))
    (cons "Locked" (vlax-get obj 'Locked))
    (cons "Color" (vlax-get obj 'Color))
    (cons "Linetype" (vlax-get obj 'Linetype))
    (cons "LinetypeScale" (vlax-get obj 'LinetypeScale))
    (cons "Lineweight" (vlax-get obj 'Lineweight))
    (cons "PlotStyleName" (vlax-get obj 'PlotStyleName))
    (cons "Transparency" (vlax-get obj 'Transparency))
  ))
  (princ "\nBasic properties:")
  (foreach pair props
    (princ (strcat "\n  " (car pair) ": " (vl-princ-to-string (cdr pair))))
  )
  props
)

;; Command to explore Civil 3D object properties
(defun c:C3D-EXPLORE-OBJECT ( / ss ent obj)
  (princ "\n=== Exploring Civil 3D Object ===")
  
  ;; Get user selection
  (setq ss (ssget))
  
  (if ss
    (progn
      (setq ent (ssname ss 0))
      (setq obj (vlax-ename->vla-object ent))
      
      (if obj
        (progn
          (princ (strcat "\nObject type: " (vlax-get obj 'ObjectName)))
          
          ;; Try to get basic properties
          (get-label-properties obj)
          
          ;; Try to get position using different methods
          (princ "\n\nTrying to get position:")
          
          ;; Method 1: GetPosition
          (princ "\nMethod 1 - GetPosition:")
          (setq pos1 (vl-catch-all-apply 'vlax-invoke (list obj 'GetPosition)))
          (if (vl-catch-all-error-p pos1)
            (princ "\n  GetPosition method not available")
            (princ (strcat "\n  Position: " (vl-princ-to-string pos1)))
          )
          
          ;; Method 2: GetLocation
          (princ "\nMethod 2 - GetLocation:")
          (setq pos2 (vl-catch-all-apply 'vlax-invoke (list obj 'GetLocation)))
          (if (vl-catch-all-error-p pos2)
            (princ "\n  GetLocation method not available")
            (princ (strcat "\n  Position: " (vl-princ-to-string pos2)))
          )
          
          ;; Method 3: GetInsertionPoint
          (princ "\nMethod 3 - GetInsertionPoint:")
          (setq pos3 (vl-catch-all-apply 'vlax-invoke (list obj 'GetInsertionPoint)))
          (if (vl-catch-all-error-p pos3)
            (princ "\n  GetInsertionPoint method not available")
            (princ (strcat "\n  Position: " (vl-princ-to-string pos3)))
          )
          
          ;; Try to get text content
          (princ "\n\nTrying to get text content:")
          (setq text (vl-catch-all-apply 'vlax-get (list obj 'Text)))
          (if (vl-catch-all-error-p text)
            (princ "\n  Text property not available")
            (princ (strcat "\n  Text: " (vl-princ-to-string text)))
          )
          
          ;; Try to get style
          (princ "\n\nTrying to get style:")
          (setq style (vl-catch-all-apply 'vlax-get (list obj 'Style)))
          (if (vl-catch-all-error-p style)
            (princ "\n  Style property not available")
            (princ (strcat "\n  Style: " (vl-princ-to-string style)))
          )
        )
        (princ "\nCould not convert entity to ActiveX object.")
      )
    )
    (princ "\nNo object selected.")
  )
  
  (princ)
)

;; Load the functions
(princ "\nCivil 3D Label Analysis Tools loaded.")
(princ "\nCommands available:")
(princ "\n  C3D-ANALYZE-LABELS : Analyze all Civil 3D labels")
(princ "\n  C3D-FIX-ONE-OVERLAP : Find and fix one overlapping pair at a time")
(princ "\n  C3D-MOVE-PIPE-LABEL : Move first pipe label by 20 units")
(princ "\n  C3D-EXPLORE-OBJECT : Explore Civil 3D object properties and methods")
(princ) 