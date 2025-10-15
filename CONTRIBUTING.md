# Contributing to bfi-wholegrain

Thank you for your interest in contributing to this research project! While this repository represents a completed master's thesis, we welcome improvements and suggestions.

## Code of Conduct

Please be respectful and constructive in all interactions.

## How to Contribute

1. **Fork the Repository**: Create your own fork of the project
2. **Create a Branch**: Make your changes in a new branch
   ```bash
   git checkout -b feature/your-feature-name
   ```
3. **Make Changes**: Follow the coding standards below
4. **Test**: Ensure your changes don't break existing functionality
5. **Commit**: Write clear, descriptive commit messages
6. **Push**: Push your branch to your fork
7. **Pull Request**: Submit a PR with a clear description

## Coding Standards

### R Code Style

We follow the [tidyverse style guide](https://style.tidyverse.org/):

- Use `<-` for assignment, not `=`
- Use snake_case for variable and function names
- Limit lines to 80 characters
- Use spaces around operators and after commas
- Use explicit `return()` statements in functions
- Add roxygen2 documentation for functions

Example:

```r
#' Calculate Peak Area
#'
#' @param mz Numeric vector of m/z values
#' @param intensity Numeric vector of intensity values
#' @return Numeric value representing peak area
#' @export
calculate_peak_area <- function(mz, intensity) {
  if (length(mz) != length(intensity)) {
    stop("mz and intensity must have the same length")
  }
  
  area <- sum(intensity * diff(c(0, mz)))
  return(area)
}
```

### MATLAB Code Style

- Use descriptive variable names
- Add comments for complex logic
- Use consistent indentation (2 or 4 spaces)
- Include function documentation headers
- Use lowercase with underscores for function names

Example:

```matlab
function [selected_vars, scores] = variable_selection(data, y, method)
% VARIABLE_SELECTION Perform variable selection on metabolomics data
%
% Inputs:
%   data   - n x p matrix of metabolite intensities
%   y      - n x 1 vector of class labels
%   method - string specifying method ('pls', 'random_forest', etc.)
%
% Outputs:
%   selected_vars - indices of selected variables
%   scores       - importance scores for each variable

% Input validation
if nargin < 3
    method = 'pls';
end

% Implementation
...

end
```

### General Guidelines

- **Documentation**: Document your functions and complex code sections
- **Dependencies**: Minimize new dependencies; justify if necessary
- **Error Handling**: Add input validation and informative error messages
- **Reproducibility**: Ensure code produces consistent results
- **Comments**: Explain *why*, not just *what*

### Deprecated Functions

Avoid using deprecated R functions:
- ❌ `data_frame()` → ✅ `tibble()`
- ❌ `funs()` → ✅ `list()` or anonymous functions
- ❌ String functions without namespace → ✅ `stringr::` prefix

## Testing

If adding new functionality:
- Add unit tests using `testthat` (R) or built-in test framework (MATLAB)
- Test with example data
- Document expected behavior

## Documentation

When adding features:
- Update README.md if necessary
- Add inline comments for complex logic
- Update function documentation

## Questions?

Open an issue for:
- Bug reports
- Feature requests
- Questions about the code
- Suggestions for improvements

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
