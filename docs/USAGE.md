# AutoCAD Label Organizer - Usage Guide

## Overview

The AutoCAD Label Organizer is a collection of tools designed to help you manage and organize MTEXT labels in your AutoCAD drawings. This guide covers the main features and how to use them effectively.

## Installation

1. Copy all `.lsp` files from the `src/lisp` directory to your AutoCAD support directory or a directory in your AutoCAD search path.
2. In AutoCAD, use the `APPLOAD` command to load the desired LISP file:
   - `ACAD-Label-Force.lsp` for the main label organization functionality
   - Other LISP files for additional features

## Main Commands

### ACAD-MTEXT-GREEDY-PLACE (Recommended)
This command uses a greedy algorithm to organize labels efficiently.

1. Run the command by typing `ACAD-MTEXT-GREEDY-PLACE` in the AutoCAD command line
2. Select a polyline that defines the bounding box for label placement
3. The algorithm will:
   - Process all MTEXT labels in the drawing
   - Sort them by priority (based on size and overlap count)
   - Move overlapping labels to optimal positions
   - Create leader lines connecting original and new positions

### Configuration

The behavior of the label organization can be customized by modifying these parameters in `ACAD-Label-Force.lsp`:

```lisp
*MIN-DISTANCE*     ; Minimum distance between labels (default: 8.0)
*MARGIN*           ; Margin around labels for overlap detection (default: 4.0)
*MAX-PASSES*       ; Number of placement attempts (default: 3)
*MAX-ATTEMPTS*     ; Maximum attempts to find position (default: 1000)
*OVERLAP-WEIGHT*   ; Priority weight for overlaps (default: 0.9)
*SIZE-WEIGHT*      ; Priority weight for label size (default: 0.1)
```

## Additional Features

### Leader Lines
- Automatically created between original and new label positions
- Red color for visibility
- Uses continuous line type
- Can be modified after creation using standard AutoCAD commands

### Overlap Detection
The algorithm uses an expanded bounding box around each label to detect overlaps:
- Includes configurable margin around actual text
- Considers both horizontal and vertical overlap
- Prioritizes moving labels with more overlaps

### Position Finding
When moving labels, the algorithm:
1. Starts from the original position
2. Searches in an expanding spiral pattern
3. Checks each potential position for:
   - Overlap with other labels
   - Minimum distance requirements
   - Containment within bounding box

## Best Practices

1. **Bounding Box Selection**
   - Use a simple closed polyline
   - Ensure enough space for all labels
   - Consider the workflow direction (left-to-right, top-to-bottom)

2. **Label Preparation**
   - Use consistent text styles
   - Keep labels at a similar scale
   - Remove unnecessary formatting

3. **Performance Optimization**
   - Work with smaller sets of labels when possible
   - Use appropriate margin and minimum distance values
   - Adjust MAX_PASSES based on label density

## Troubleshooting

1. **Labels Still Overlap**
   - Increase *MIN-DISTANCE* and *MARGIN* values
   - Increase *MAX-PASSES* for more placement attempts
   - Check if bounding box is large enough

2. **Unexpected Label Positions**
   - Verify bounding box selection
   - Check for invalid MTEXT entities
   - Review priority weights

3. **Performance Issues**
   - Reduce number of labels being processed
   - Decrease *MAX-ATTEMPTS* value
   - Optimize bounding box size

## Support

For issues, bug reports, or feature requests:
1. Check the GitHub repository issues section
2. Provide detailed information about the problem
3. Include sample drawings if possible 