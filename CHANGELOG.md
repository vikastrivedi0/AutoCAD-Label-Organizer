# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2024-03-19

### Added
- Initial release of AutoCAD Label Organizer
- Greedy label placement algorithm implementation
- Leader line creation between original and new positions
- Configurable parameters for label placement
- Comprehensive documentation
- Example drawings
- Python utility scripts for data processing

### Features
- ACAD-MTEXT-GREEDY-PLACE command for label organization
- Intelligent overlap detection and resolution
- Priority-based label processing
- Spiral search pattern for optimal positions
- Bounding box constraints
- Red leader lines for visual tracking
- Multiple passes for better results

### Technical Details
- Expanded bounding box calculations for accurate overlap detection
- Configurable margins and minimum distances
- Priority scoring based on label size and overlap count
- Efficient position search algorithm
- Robust error handling and user feedback

## [0.2.0] - 2024-03-14

### Added
- Force-directed placement algorithm (alternative approach)
- Additional utility functions for label processing
- Support for different label types
- Improved error handling

### Changed
- Enhanced overlap detection algorithm
- Better performance for large numbers of labels
- Updated documentation

## [0.1.0] - 2024-03-11

### Added
- Initial prototype
- Basic label movement functionality
- Simple overlap detection
- Command-line interface
- Basic documentation 