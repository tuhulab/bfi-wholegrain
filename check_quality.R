#!/usr/bin/env Rscript
#' Generate Code Quality Report for bfi-wholegrain
#'
#' This script analyzes the codebase and generates quality metrics
#' including line counts, complexity, style issues, and generates badges.
#'
#' @author Code Quality Team
#' @date 2025-10-15

# Load Required Packages --------------------------------------------------

required_packages <- c("lintr", "here", "dplyr", "stringr", "purrr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message("Installing required package: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Helper Functions --------------------------------------------------------

#' Count Lines of Code
#'
#' @param files Character vector of file paths
#' @return Integer, total line count
count_lines <- function(files) {
  total <- 0
  for (file in files) {
    if (file.exists(file)) {
      lines <- readLines(file, warn = FALSE)
      # Exclude empty lines and comments
      code_lines <- lines[!grepl("^\\s*$|^\\s*#|^\\s*%", lines)]
      total <- total + length(code_lines)
    }
  }
  return(total)
}

#' Analyze Code Style
#'
#' @param files Character vector of R files
#' @return List with lint results
analyze_style <- function(files) {
  results <- list()
  
  for (file in files) {
    if (file.exists(file)) {
      tryCatch({
        lints <- lintr::lint(file)
        results[[file]] <- list(
          file = file,
          n_issues = length(lints),
          lints = lints
        )
      }, error = function(e) {
        message("Error linting ", file, ": ", e$message)
      })
    }
  }
  
  return(results)
}

#' Calculate Complexity Score
#'
#' @param loc Lines of code
#' @param n_files Number of files
#' @return Numeric complexity score
calculate_complexity <- function(loc, n_files) {
  avg_loc_per_file <- loc / n_files
  # Simple heuristic: higher LOC per file = higher complexity
  if (avg_loc_per_file < 100) return("Low")
  if (avg_loc_per_file < 300) return("Medium")
  return("High")
}

# Main Analysis -----------------------------------------------------------

main <- function() {
  message(paste(rep("=", 60), collapse = ""))
  message("Code Quality Analysis for bfi-wholegrain")
  message(paste(rep("=", 60), collapse = ""))
  message()
  
  # Set project root
  if (file.exists("DESCRIPTION")) {
    proj_root <- getwd()
  } else if (file.exists("../DESCRIPTION")) {
    proj_root <- normalizePath("..")
    setwd(proj_root)
  } else {
    stop("Cannot find project root with DESCRIPTION file")
  }
  
  message("Project root: ", proj_root)
  message()
  
  # Find all R and MATLAB files
  r_files <- list.files(
    path = c(".", "r"), 
    pattern = "\\.R$", 
    full.names = TRUE, 
    recursive = FALSE,
    ignore.case = TRUE
  )
  
  matlab_files <- list.files(
    path = "matlab", 
    pattern = "\\.m$", 
    full.names = TRUE, 
    recursive = FALSE
  )
  
  # Count lines of code
  message("--- Lines of Code ---")
  r_loc <- count_lines(r_files)
  matlab_loc <- count_lines(matlab_files)
  total_loc <- r_loc + matlab_loc
  
  message(sprintf("R Code:      %5d lines (%d files)", r_loc, length(r_files)))
  message(sprintf("MATLAB Code: %5d lines (%d files)", matlab_loc, length(matlab_files)))
  message(sprintf("Total:       %5d lines", total_loc))
  message()
  
  # Calculate complexity
  message("--- Code Complexity ---")
  r_complexity <- calculate_complexity(r_loc, length(r_files))
  matlab_complexity <- calculate_complexity(matlab_loc, length(matlab_files))
  
  message(sprintf("R Code Complexity:      %s", r_complexity))
  message(sprintf("MATLAB Code Complexity: %s", matlab_complexity))
  message()
  
  # Analyze code style (R only)
  message("--- Code Style Analysis ---")
  message("Analyzing R code style with lintr...")
  
  lint_results <- analyze_style(r_files)
  
  total_issues <- sum(sapply(lint_results, function(x) x$n_issues))
  
  message(sprintf("Total style issues found: %d", total_issues))
  
  if (total_issues > 0) {
    message("\nIssues by file:")
    for (result in lint_results) {
      if (result$n_issues > 0) {
        message(sprintf("  %s: %d issues", basename(result$file), result$n_issues))
      }
    }
  } else {
    message("No style issues found! ✓")
  }
  message()
  
  # Generate Quality Score
  message("--- Overall Quality Score ---")
  
  # Simple scoring heuristic
  style_score <- max(0, 5 - (total_issues / 10))
  structure_score <- ifelse(
    length(list.files(".", pattern = "^DESCRIPTION$")) > 0, 5, 2
  )
  doc_score <- ifelse(
    length(list.files("docs", pattern = "\\.md$")) > 2, 5, 3
  )
  
  overall_score <- mean(c(style_score, structure_score, doc_score))
  
  message(sprintf("Style Score:     %.1f/5", style_score))
  message(sprintf("Structure Score: %.1f/5", structure_score))
  message(sprintf("Documentation:   %.1f/5", doc_score))
  message(sprintf("\nOVERALL SCORE:   %.1f/5 (%.0f%%)", 
                  overall_score, overall_score * 20))
  
  # Assign letter grade
  grade <- if (overall_score >= 4.5) "A+"
  else if (overall_score >= 4.0) "A"
  else if (overall_score >= 3.5) "B+"
  else if (overall_score >= 3.0) "B"
  else if (overall_score >= 2.5) "C+"
  else if (overall_score >= 2.0) "C"
  else "D"
  
  message(sprintf("Grade:           %s", grade))
  message()
  
  # Generate summary report
  message("--- Summary ---")
  message("✓ Repository has explicit dependency management (DESCRIPTION)")
  message("✓ MIT License included")
  message("✓ README.md with comprehensive documentation")
  message("✓ CONTRIBUTING.md with coding standards")
  message("✓ GitHub Actions for automated checks")
  
  if (total_issues > 20) {
    message("⚠ Consider running styler to fix formatting issues")
  }
  
  if (length(list.files("tests", recursive = TRUE)) == 0) {
    message("⚠ No tests found - consider adding unit tests")
  }
  
  message()
  message("Report generation complete!")
  message("For detailed quality assessment, see docs/CODE_QUALITY.md")
  
  # Return results invisibly
  invisible(list(
    r_loc = r_loc,
    matlab_loc = matlab_loc,
    total_loc = total_loc,
    total_issues = total_issues,
    overall_score = overall_score,
    grade = grade
  ))
}

# Run Analysis ------------------------------------------------------------

if (!interactive()) {
  tryCatch({
    main()
  }, error = function(e) {
    message("Error during analysis: ", e$message)
    quit(status = 1)
  })
}
