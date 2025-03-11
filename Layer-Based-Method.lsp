;;; Civil 3D Label Overlap Detector - Layer-Based Method
;;; This script detects overlapping Civil 3D labels using layer isolation and AutoCAD's native overlap detection.
;;; Usage: Load the script and run the command C3D-LAYER-OVERLAP

;; Function to save current layer state
(defun save-layer-state ( / result)
  (setq result (getvar "CLAYER"))  ; Save current layer
  (command "._layer" "save-state" "*temp*" "")  ; Save all layer states
  result
)

;; Function to restore layer state
(defun restore-layer-state (original-layer)
  (command "._layer" "restore-state" "*temp*" "")  ; Restore all layer states
  (command "._layer" "set" original-layer "")      ; Restore current layer
  (command "._layer" "delete-state" "*temp*" "")   ; Clean up temporary state
)

;; Function to isolate labels on temporary layer
(defun isolate-labels (labels temp-layer / ent)
  (command "._layer" "make" temp-layer "")  ; Create temporary layer
  (foreach ent labels
    (command "._change" ent "" "p" "la" temp-layer "")  ; Move entity to temp layer
  )
)

;; Function to restore labels to original layers
(defun restore-labels (labels original-layers / i)
  (setq i 0)
  (foreach ent labels
    (command "._change" ent "" "p" "la" (nth i original-layers) "")
    (setq i (1+ i))
  )
)

;; Function to get entity's layer
(defun get-entity-layer (ent)
  (cdr (assoc 8 (entget ent)))
)

;; Function to collect all labels and their layers
(defun collect-labels (ss / i max labels layers ent)
  (setq i 0
        max (sslength ss)
        labels '()
        layers '())
  
  (while (< i max)
    (setq ent (ssname ss i))
    (setq labels (cons ent labels))
    (setq layers (cons (get-entity-layer ent) layers))
    (setq i (1+ i))
  )
  (list labels layers)
)

;; Function to find overlapping labels using selection
(defun find-overlaps (temp-layer / ss overlaps)
  (command "._layer" "freeze" "*" "")          ; Freeze all layers
  (command "._layer" "thaw" temp-layer "")     ; Thaw temporary layer
  (command "._zoom" "e" "")                    ; Zoom extents
  
  ;; Use crossing window to find overlapping objects
  (setq ss (ssget "X" (list (cons 410 temp-layer))))  ; Get all entities on temp layer
  
  (if ss
    (progn
      (setq overlaps '())
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq overlaps (cons ent overlaps))
        (setq i (1+ i))
      )
      overlaps
    )
    nil
  )
)

(defun c:C3D-LAYER-OVERLAP (/ ss1 ss2 original-layer temp-layer label-data labels layers overlaps)
  (princ "\nSearching for Civil 3D labels...")
  
  ;; Get both Pipe Labels and Structure Labels
  (setq ss1 (ssget "_X" '((0 . "AECC_PIPE_LABEL")))
        ss2 (ssget "_X" '((0 . "AECC_STRUCTURE_LABEL"))))
  
  (if (or ss1 ss2)
    (progn
      ;; Save current layer state
      (setq original-layer (save-layer-state))
      
      ;; Create temporary layer for overlap detection
      (setq temp-layer "TEMP_OVERLAP_CHECK")
      
      ;; Process labels
      (if ss1
        (progn
          (princ (strcat "\nProcessing " (itoa (sslength ss1)) " Pipe Labels..."))
          (setq label-data (collect-labels ss1))
          (setq labels (car label-data))
          (setq layers (cadr label-data))
        )
      )
      
      (if ss2
        (progn
          (princ (strcat "\nProcessing " (itoa (sslength ss2)) " Structure Labels..."))
          (setq label-data (collect-labels ss2))
          (setq labels (append labels (car label-data)))
          (setq layers (append layers (cadr label-data)))
        )
      )
      
      ;; Move labels to temporary layer
      (isolate-labels labels temp-layer)
      
      ;; Find overlapping labels
      (princ "\nChecking for overlaps...")
      (setq overlaps (find-overlaps temp-layer))
      
      ;; Restore labels to original layers
      (restore-labels labels layers)
      
      ;; Restore original layer state
      (restore-layer-state original-layer)
      
      ;; Report results
      (if overlaps
        (progn
          (print (strcat "\nFound " (itoa (length overlaps)) " overlapping Civil 3D labels."))
          (princ "\nPress Enter to delete overlapping labels, or Esc to cancel...")
          (setq user-input (getstring))
          (if (= user-input "")  ; Check for empty string (Enter key)
            (progn
              (setq deleted-count 0)
              (foreach ent overlaps
                (if (and ent (entget ent))  ; Verify entity still exists
                  (progn
                    (entdel ent)
                    (setq deleted-count (1+ deleted-count))
                  )
                )
              )
              (print (strcat "\nCompleted. Successfully deleted " (itoa deleted-count) " overlapping Civil 3D labels."))
            )
            (print "\nOperation cancelled by user.")
          )
        )
        (print "\nNo overlapping labels found.")
      )
    )
    (print "\nNo Civil 3D Pipe or Structure labels found in the drawing.")
  )
  (princ)
)

;; Load the function
(princ "\nCivil 3D Label Overlap Detector (Layer-Based Method) loaded. Type C3D-LAYER-OVERLAP to run.")
(princ) 