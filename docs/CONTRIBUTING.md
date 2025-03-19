# Contributing to AutoCAD Label Organizer

Thank you for your interest in contributing to the AutoCAD Label Organizer project! This guide will help you get started with development and explain our contribution process.

## Development Setup

1. **Prerequisites**
   - AutoCAD (2018 or later recommended)
   - Git
   - A text editor with LISP support (Visual Studio Code with LISP extension recommended)
   - Python 3.7+ (for utility scripts)

2. **Repository Setup**
   ```bash
   git clone https://github.com/[your-username]/AutoCAD-Label-Organizer.git
   cd AutoCAD-Label-Organizer
   ```

3. **Project Structure**
   ```
   AutoCAD-Label-Organizer/
   ├── src/
   │   ├── lisp/           # LISP source files
   │   └── python/         # Python utility scripts
   ├── examples/           # Example drawings
   ├── docs/              # Documentation
   ├── tests/             # Test files
   ├── .gitignore
   ├── LICENSE
   └── README.md
   ```

## Development Workflow

1. **Create a Branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Code Style Guidelines**

   ### LISP Files
   - Use descriptive function and variable names
   - Add comments for complex logic
   - Keep functions focused and small
   - Use consistent indentation (2 spaces)
   - Prefix global variables with *
   - Add docstrings to functions

   Example:
   ```lisp
   (defun get-label-size (entdata)
     "Calculate the size of a label from its entity data"
     (setq width (cdr (assoc 41 entdata))
           height (cdr (assoc 43 entdata)))
     (* width height)
   )
   ```

   ### Python Files
   - Follow PEP 8 guidelines
   - Use type hints
   - Add docstrings to functions and classes
   - Keep functions focused and small

3. **Testing**
   - Test your changes in AutoCAD with various drawings
   - Verify functionality with different label configurations
   - Check edge cases (empty drawings, invalid inputs)
   - Document any limitations or known issues

4. **Documentation**
   - Update relevant documentation files
   - Add comments to explain complex logic
   - Document any new parameters or features
   - Include examples where appropriate

## Making Changes

1. **Adding New Features**
   - Discuss major changes in GitHub Issues first
   - Keep changes focused and atomic
   - Add appropriate error handling
   - Update documentation

2. **Fixing Bugs**
   - Reference the issue number in commits
   - Add regression tests if possible
   - Document the root cause
   - Update error messages if needed

3. **Commit Guidelines**
   - Write clear commit messages
   - Use present tense ("Add feature" not "Added feature")
   - Reference issues and pull requests
   - Keep commits focused and atomic

   Example:
   ```
   feat: Add spiral search pattern for label placement

   - Implement expanding spiral search for new positions
   - Add configurable step size and max attempts
   - Update documentation with new parameters
   
   Fixes #123
   ```

## Pull Request Process

1. **Before Submitting**
   - Update documentation
   - Test your changes thoroughly
   - Ensure code follows style guidelines
   - Rebase on latest main branch

2. **Pull Request Description**
   - Describe the changes made
   - Explain the motivation
   - List any limitations or known issues
   - Include screenshots if relevant
   - Reference related issues

3. **Review Process**
   - Address review comments promptly
   - Keep discussions focused
   - Be open to suggestions
   - Test changes after modifications

## Release Process

1. **Version Numbers**
   - Follow semantic versioning (MAJOR.MINOR.PATCH)
   - Document changes in CHANGELOG.md
   - Tag releases in Git

2. **Release Checklist**
   - Update version numbers
   - Update documentation
   - Test with latest AutoCAD version
   - Create release notes
   - Tag the release

## Getting Help

- Check existing issues and documentation
- Join discussions in GitHub Issues
- Be clear and specific in questions
- Provide example drawings when possible

## Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Help others learn and grow
- Follow project guidelines

Thank you for contributing to AutoCAD Label Organizer! 