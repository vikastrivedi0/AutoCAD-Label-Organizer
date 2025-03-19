# AutoCAD Label Organizer

A powerful tool for automatically organizing and resolving overlapping MTEXT labels in AutoCAD drawings using advanced placement algorithms.

## Features

- **Intelligent Label Placement**: Uses a greedy algorithm to efficiently resolve label overlaps
- **Visual Tracking**: Creates leader lines between original and new positions
- **Configurable Parameters**: Customize behavior to suit your needs
- **Multiple Algorithms**: Choose between greedy and force-directed placement
- **Comprehensive Documentation**: Detailed guides for users and developers
- **Example Drawings**: Sample files to demonstrate functionality
- **Python Utilities**: Additional tools for data processing

## Quick Start

1. **Installation**
   - Clone this repository or download the latest release
   - Copy `.lsp` files from `src/lisp` to your AutoCAD support directory
   - Load `ACAD-Label-Force.lsp` using AutoCAD's `APPLOAD` command

2. **Basic Usage**
   ```
   Command: ACAD-MTEXT-GREEDY-PLACE
   Select bounding box polyline: (select a closed polyline)
   ```

3. **Results**
   - Labels will be repositioned to minimize overlaps
   - Red leader lines show original positions
   - Multiple passes ensure optimal placement

## Project Structure

```
AutoCAD-Label-Organizer/
├── src/
│   ├── lisp/           # LISP source files
│   └── python/         # Python utility scripts
├── examples/           # Example drawings
├── docs/              # Documentation
│   ├── USAGE.md       # User guide
│   └── CONTRIBUTING.md # Developer guide
├── tests/             # Test files
├── .gitignore         # Git ignore rules
├── CHANGELOG.md       # Version history
├── LICENSE            # MIT License
└── README.md         # This file
```

## Documentation

- [User Guide](docs/USAGE.md): Detailed instructions for using the tool
- [Contributing Guide](docs/CONTRIBUTING.md): Information for developers
- [Changelog](CHANGELOG.md): Version history and updates

## Configuration

Key parameters in `ACAD-Label-Force.lsp`:

```lisp
*MIN-DISTANCE*     ; Minimum distance between labels (default: 8.0)
*MARGIN*           ; Margin around labels for overlap detection (default: 4.0)
*MAX-PASSES*       ; Number of placement attempts (default: 3)
*MAX-ATTEMPTS*     ; Maximum attempts to find position (default: 1000)
*OVERLAP-WEIGHT*   ; Priority weight for overlaps (default: 0.9)
*SIZE-WEIGHT*      ; Priority weight for label size (default: 0.1)
```

## Requirements

- AutoCAD 2018 or later
- Windows operating system
- Python 3.7+ (for utility scripts)

## Contributing

We welcome contributions! Please see our [Contributing Guide](docs/CONTRIBUTING.md) for details on:
- Setting up the development environment
- Code style guidelines
- Pull request process
- Release procedures

## Support

- **Issues**: Use GitHub Issues for bug reports and feature requests
- **Questions**: Check the [User Guide](docs/USAGE.md) or open a discussion
- **Examples**: See the `examples` directory for sample drawings

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Thanks to all contributors who have helped improve this tool
- Inspired by force-directed graph drawing algorithms
- Built with AutoCAD's powerful LISP API 