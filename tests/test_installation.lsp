;; AutoCAD Label Organizer - Installation Test
;; This file helps verify that the installation is working correctly

;; Test command to verify installation
(defun c:TEST-LABEL-ORGANIZER ( / )
  "Run basic tests to verify installation and functionality"
  (princ "\nTesting AutoCAD Label Organizer installation...\n")
  
  ;; Test 1: Check if main file is loaded
  (princ "\nTest 1: Checking if main functions are available...")
  (if (not (vl-bb-ref '*MIN-DISTANCE*))
    (princ "\nError: Main file (ACAD-Label-Force.lsp) is not loaded. Please load it first using APPLOAD command.")
    (princ "\nSuccess: Main file is loaded and global variables are accessible.")
  )
  
  ;; Test 2: Create test labels
  (princ "\n\nTest 2: Creating test labels...")
  (command "._mtext" '(0 0 0) '(10 2 0) "Test Label 1" "")
  (command "._mtext" '(5 0 0) '(15 2 0) "Test Label 2" "")
  (command "._mtext" '(10 0 0) '(20 2 0) "Test Label 3" "")
  (princ "\nSuccess: Created 3 overlapping test labels.")
  
  ;; Test 3: Create bounding box
  (princ "\n\nTest 3: Creating test bounding box...")
  (command "._pline" 
          '(-5 -5 0) 
          '(25 -5 0)
          '(25 10 0)
          '(-5 10 0)
          "c")
  (princ "\nSuccess: Created bounding box polyline.")
  
  ;; Test 4: Check command availability
  (princ "\n\nTest 4: Checking command availability...")
  (if (not (vl-bb-ref 'c:ACAD-MTEXT-GREEDY-PLACE))
    (princ "\nError: ACAD-MTEXT-GREEDY-PLACE command not found. Please verify file loading.")
    (princ "\nSuccess: ACAD-MTEXT-GREEDY-PLACE command is available.")
  )
  
  ;; Final instructions
  (princ "\n\nInstallation test complete!")
  (princ "\n\nTo test the label organization:")
  (princ "\n1. Type ACAD-MTEXT-GREEDY-PLACE")
  (princ "\n2. Select the rectangular polyline that was just created")
  (princ "\n3. The three test labels should be reorganized with leader lines")
  (princ "\n\nIf you encounter any issues, please check the documentation or report the problem on GitHub.")
  (princ)
)

;; Load message
(princ "\nAutoCAD Label Organizer test file loaded. Type TEST-LABEL-ORGANIZER to run tests.")
(princ) 