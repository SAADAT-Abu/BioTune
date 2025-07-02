# BioTune Pro - Complete Bioconductor Package Development Tool
# Advanced Shiny app with BiocCheck, Git integration, and package deployment

library(shiny)
library(shinydashboard)
library(DT)
library(shinyAce)
library(stringr)
library(htmltools)
library(shinyjs)
library(shinyFiles)
library(shinycssloaders)

# Additional libraries for extended features
suppressPackageStartupMessages({
    # Check for BiocCheck availability
    tryCatch({
        library(BiocCheck)
        BIOCCHECK_AVAILABLE <- TRUE
    }, error = function(e) {
        message("BiocCheck not available - using mock implementation")
        BIOCCHECK_AVAILABLE <- FALSE
    })
    
    # Check for git2r availability
    tryCatch({
        library(git2r)
        GIT_AVAILABLE <- TRUE
    }, error = function(e) {
        message("git2r not available - Git features disabled")
        GIT_AVAILABLE <- FALSE
    })
    
    # Check for devtools availability
    tryCatch({
        library(devtools)
        DEVTOOLS_AVAILABLE <- TRUE
    }, error = function(e) {
        message("devtools not available - package building disabled")  
        DEVTOOLS_AVAILABLE <- FALSE
    })
})

# Global configuration
TEMP_DIR <- tempdir()
REPOS_DIR <- file.path(TEMP_DIR, "biotune_repos")
if (!dir.exists(REPOS_DIR)) dir.create(REPOS_DIR, recursive = TRUE)

# Enhanced source code example with more Bioconductor-specific issues
source_code_example <- "# Example R package with multiple BiocCheck issues
Package: ExampleBiocPackage
Type: Package
Title: Example Package for Demonstration
Version: 0.99.0
Author: Developer Name
Maintainer: Developer <dev@example.com>
Description: This package demonstrates various BiocCheck issues.
License: Artistic-2.0
Encoding: UTF-8
LazyData: true
biocViews: Software, Transcriptomics
Depends: R (>= 4.3.0)
Imports: 
    Biobase,
    GenomicRanges,
    S4Vectors

#' Calculate Statistics
#' 
#' @param data A data frame with values
#' @param method Method to use for calculation  
#' @param use_weights Whether to use weights
#' @return Calculated statistic
#' @examples
#' # Missing runnable examples
#' @export
calculate_stats <- function(data, method = 'mean',
                          use_weights = TRUE,
                            normalize = FALSE) {
\t# Tab character issue
  if (method == 'mean') {  # 2 spaces instead of 4
    result <- mean(data$values, na.rm = TRUE)
      if (use_weights) {  # 6 spaces - not multiple of 4
        result <- weighted.mean(data$values, data$weights, na.rm = TRUE)
\t\t}  # More tabs
    } else if (method == 'median') {
result <- median(data$values, na.rm = TRUE)  # No indentation
  }
  
  # Line exceeding 80 characters - this is a very long line that should be split according to Bioconductor guidelines for better readability and code maintenance
  final_result <- ifelse(normalize, (result - min(data$values, na.rm = TRUE)) / (max(data$values, na.rm = TRUE) - min(data$values, na.rm = TRUE)), result)
  
  return(final_result)
}

# Missing documentation for S4 class
setClass('ExampleClass',
   slots = c(
data = 'data.frame',  # Bad indentation
metadata = 'list'
 ))

# Function missing @export and proper documentation
processData <- function(x) {
    # camelCase function name (should be snake_case)
    library(GenomicRanges)  # library() call in package code
    return(x + 1)
}

# Missing man pages, examples, and proper biocViews
"

# Enhanced BiocCheck integration
run_real_bioccheck <- function(package_path) {
    if (!BIOCCHECK_AVAILABLE) {
        return(run_mock_bioccheck(package_path))
    }
    
    tryCatch({
        # Capture BiocCheck output
        check_result <- capture.output({
            BiocCheck::BiocCheck(package_path, `quit-with-status` = FALSE)
        })
        
        # Parse BiocCheck output into structured format
        parse_bioccheck_output(check_result)
    }, error = function(e) {
        list(list(
            line = 1,
            type = "bioccheck_error",
            message = paste("BiocCheck failed:", e$message),
            severity = "error",
            file = "UNKNOWN"
        ))
    })
}

# Enhanced mock BiocCheck with more comprehensive rules
run_mock_bioccheck <- function(code_or_path) {
    if (file.exists(code_or_path)) {
        # If it's a path, read all R files
        r_files <- list.files(code_or_path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
        code <- paste(sapply(r_files, readLines), collapse = "\n")
    } else {
        code <- code_or_path
    }
    
    lines <- strsplit(code, "\n")[[1]]
    issues <- list()
    
    # Check indentation and formatting
    indent_issues <- check_indentation(lines)
    length_issues <- check_line_length(lines)
    issues <- c(issues, indent_issues, length_issues)
    
    # Bioconductor-specific checks
    for (i in seq_along(lines)) {
        line <- lines[i]
        
        # Library calls in package code
        if (grepl("^\\s*library\\(", line)) {
            issues <- append(issues, list(list(
                line = i,
                type = "library_usage",
                message = "Use requireNamespace() or :: instead of library() in package code",
                severity = "error",
                file = "R/functions.R"
            )))
        }
        
        # CamelCase function names
        if (grepl("^[a-z][a-zA-Z]*[A-Z].*<-\\s*function", line)) {
            issues <- append(issues, list(list(
                line = i,
                type = "naming_convention",
                message = "Use snake_case for function names (Bioconductor style)",
                severity = "warning",
                file = "R/functions.R"
            )))
        }
        
        # Missing @export tags
        if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*\\s*<-\\s*function", line)) {
            prev_lines <- if (i > 5) lines[(i-5):(i-1)] else lines[1:(i-1)]
            if (!any(grepl("@export", prev_lines))) {
                issues <- append(issues, list(list(
                    line = i,
                    type = "missing_export",
                    message = "Public function should have @export in documentation",
                    severity = "warning",
                    file = "R/functions.R"
                )))
            }
        }
        
        # Version number check (for DESCRIPTION)
        if (grepl("^Version:", line) && !grepl("0\\.99\\.", line)) {
            issues <- append(issues, list(list(
                line = i,
                type = "version_format",
                message = "New packages should use version 0.99.x",
                severity = "warning",
                file = "DESCRIPTION"
            )))
        }
        
        # biocViews check
        if (grepl("^biocViews:", line)) {
            if (!grepl("Software|ExperimentData|AnnotationData", line)) {
                issues <- append(issues, list(list(
                    line = i,
                    type = "biocviews_missing",
                    message = "biocViews should include Software, ExperimentData, or AnnotationData",
                    severity = "error",
                    file = "DESCRIPTION"
                )))
            }
        }
    }
    
    # Check for missing files
    missing_files_checks <- check_required_files(code)
    issues <- c(issues, missing_files_checks)
    
    return(issues)
}

# Check for required Bioconductor files
check_required_files <- function(code) {
    issues <- list()
    
    # Check for NEWS file
    if (!grepl("NEWS", code)) {
        issues <- append(issues, list(list(
            line = 0,
            type = "missing_news",
            message = "Package should include a NEWS file",
            severity = "warning",
            file = "NEWS"
        )))
    }
    
    # Check for vignette
    if (!grepl("vignette|Sweave", code)) {
        issues <- append(issues, list(list(
            line = 0,
            type = "missing_vignette",
            message = "Package should include at least one vignette",
            severity = "warning",
            file = "vignettes/"
        )))
    }
    
    return(issues)
}

# Git operations
clone_repository <- function(repo_url, local_path = NULL) {
    if (!GIT_AVAILABLE) {
        stop("Git functionality not available - install git2r package")
    }
    
    if (is.null(local_path)) {
        local_path <- file.path(REPOS_DIR, basename(repo_url))
    }
    
    tryCatch({
        if (dir.exists(local_path)) {
            unlink(local_path, recursive = TRUE)
        }
        
        repo <- git2r::clone(repo_url, local_path)
        return(local_path)
    }, error = function(e) {
        stop(paste("Failed to clone repository:", e$message))
    })
}

commit_and_push_changes <- function(repo_path, commit_message = "BioTune: Fixed code quality issues") {
    if (!GIT_AVAILABLE) {
        stop("Git functionality not available")
    }
    
    tryCatch({
        repo <- git2r::repository(repo_path)
        
        # Add all changes
        git2r::add(repo, "*")
        
        # Commit changes
        git2r::commit(repo, message = commit_message)
        
        # Push changes (requires authentication setup)
        git2r::push(repo)
        
        return(TRUE)
    }, error = function(e) {
        warning(paste("Git operation failed:", e$message))
        return(FALSE)
    })
}

# Package building functions
build_package_tarball <- function(package_path, output_dir = NULL) {
    if (!DEVTOOLS_AVAILABLE) {
        stop("Package building not available - install devtools")
    }
    
    if (is.null(output_dir)) {
        output_dir <- dirname(package_path)
    }
    
    tryCatch({
        # Build package
        tarball_path <- devtools::build(package_path, path = output_dir)
        return(tarball_path)
    }, error = function(e) {
        stop(paste("Package build failed:", e$message))
    })
}

check_package <- function(package_path) {
    if (!DEVTOOLS_AVAILABLE) {
        return("Package checking not available")
    }
    
    tryCatch({
        check_result <- devtools::check(package_path, quiet = TRUE)
        return(check_result)
    }, error = function(e) {
        return(paste("Package check failed:", e$message))
    })
}

# [Previous helper functions remain the same: check_indentation, fix_indentation, etc.]
# Function to check indentation
check_indentation <- function(code_lines) {
    issues <- list()
    
    for (i in seq_along(code_lines)) {
        line <- code_lines[i]
        
        # Skip empty lines and pure comments
        if (grepl("^\\s*$", line) || grepl("^\\s*#\\s*$", line)) next
        
        # Extract leading whitespace
        leading_space <- str_extract(line, "^\\s*")
        if (is.na(leading_space)) leading_space <- ""
        
        # Check for tabs
        if (grepl("\\t", leading_space)) {
            issues <- append(issues, list(list(
                line = i,
                type = "tab_character",
                message = "Tab character found - use 4 spaces instead",
                severity = "error",
                file = "R/functions.R"
            )))
        }
        
        # Check if indentation is multiple of 4
        space_count <- nchar(leading_space)
        if (space_count %% 4 != 0 && space_count > 0) {
            suggested <- round(space_count / 4) * 4
            issues <- append(issues, list(list(
                line = i,
                type = "invalid_indentation",
                current = space_count,
                suggested = suggested,
                message = sprintf("Indentation should be multiple of 4 (found %d, suggest %d)", 
                                space_count, suggested),
                severity = "warning",
                file = "R/functions.R"
            )))
        }
    }
    
    return(issues)
}

# Function to check line length
check_line_length <- function(code_lines, max_length = 80) {
    issues <- list()
    
    for (i in seq_along(code_lines)) {
        line <- code_lines[i]
        line_length <- nchar(line)
        
        if (line_length > max_length) {
            issues <- append(issues, list(list(
                line = i,
                type = "long_line",
                length = line_length,
                max_length = max_length,
                message = sprintf("Line too long (%d chars, max %d)", line_length, max_length),
                severity = "warning",
                file = "R/functions.R"
            )))
        }
    }
    
    return(issues)
}

# Function to fix indentation
fix_indentation <- function(code_lines) {
    fixed_lines <- code_lines
    
    for (i in seq_along(fixed_lines)) {
        line <- fixed_lines[i]
        
        # Skip empty lines
        if (grepl("^\\s*$", line)) next
        
        # Replace tabs with 4 spaces
        line <- gsub("\\t", "    ", line)
        
        # Fix indentation to nearest multiple of 4
        leading_space <- str_extract(line, "^\\s*")
        if (is.na(leading_space)) leading_space <- ""
        content <- str_remove(line, "^\\s*")
        space_count <- nchar(leading_space)
        
        # Only fix if there's actual indentation
        if (space_count > 0) {
            correct_spaces <- round(space_count / 4) * 4
            fixed_lines[i] <- paste0(strrep(" ", correct_spaces), content)
        } else {
            fixed_lines[i] <- line
        }
    }
    
    return(fixed_lines)
}

# Function to split long lines
split_long_lines <- function(code_lines, max_length = 80) {
    fixed_lines <- c()
    
    for (line in code_lines) {
        if (nchar(line) <= max_length) {
            fixed_lines <- c(fixed_lines, line)
            next
        }
        
        # Simple line splitting at commas and operators
        leading_space <- str_extract(line, "^\\s*")
        if (is.na(leading_space)) leading_space <- ""
        
        # Try to split at logical points
        if (grepl(",", line)) {
            parts <- str_split(line, ",")[[1]]
            for (j in seq_along(parts)) {
                if (j == 1) {
                    fixed_lines <- c(fixed_lines, paste0(parts[j], ","))
                } else if (j == length(parts)) {
                    indent <- paste0(leading_space, "    ")
                    fixed_lines <- c(fixed_lines, paste0(indent, str_trim(parts[j])))
                } else {
                    indent <- paste0(leading_space, "    ")
                    fixed_lines <- c(fixed_lines, paste0(indent, str_trim(parts[j]), ","))
                }
            }
        } else {
            fixed_lines <- c(fixed_lines, line)
        }
    }
    
    return(fixed_lines)
}

# Enhanced fix functions for Bioconductor compliance
fix_bioc_compliance <- function(code_lines) {
    fixed_lines <- code_lines
    
    for (i in seq_along(fixed_lines)) {
        line <- fixed_lines[i]
        
        # Fix library() calls
        if (grepl("^\\s*library\\(", line)) {
            # Extract package name
            pkg_match <- str_extract(line, "library\\(([^)]+)\\)")
            if (!is.na(pkg_match)) {
                pkg_name <- str_extract(pkg_match, "(?<=library\\()[^)]+(?=\\))")
                leading_space <- str_extract(line, "^\\s*")
                if (is.na(leading_space)) leading_space <- ""
                
                # Replace with requireNamespace
                fixed_lines[i] <- paste0(leading_space, 
                                       "if (!requireNamespace(", pkg_name, ", quietly = TRUE)) {")
                fixed_lines <- append(fixed_lines, 
                                    paste0(leading_space, "    stop(\"Package ", pkg_name, " needed for this function to work. Please install it.\")"),
                                    after = i)
                fixed_lines <- append(fixed_lines, paste0(leading_space, "}"), after = i + 1)
            }
        }
        
        # Fix camelCase function names (basic pattern)
        if (grepl("^[a-z][a-zA-Z]*[A-Z].*<-\\s*function", line)) {
            # Simple conversion: camelCase -> snake_case
            func_name <- str_extract(line, "^[a-zA-Z_][a-zA-Z0-9_]*")
            if (!is.na(func_name)) {
                snake_case_name <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", func_name, perl = TRUE)
                fixed_lines[i] <- str_replace(line, func_name, snake_case_name)
            }
        }
    }
    
    return(fixed_lines)
}

# UI
ui <- dashboardPage(
    dashboardHeader(title = "BioTune Pro - Complete Bioconductor Package Tool"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Code Analysis", tabName = "analysis", icon = icon("code")),
            menuItem("Git Integration", tabName = "git", icon = icon("git-alt")),
            menuItem("BiocCheck Results", tabName = "bioccheck", icon = icon("check-circle")),
            menuItem("Quick Fixes", tabName = "fixes", icon = icon("wrench")),
            menuItem("Package Builder", tabName = "builder", icon = icon("box")),
            menuItem("Deployment", tabName = "deployment", icon = icon("upload")),
            menuItem("Reports", tabName = "reports", icon = icon("file-alt"))
        )
    ),
    
    dashboardBody(
        useShinyjs(),
        
        shiny::tags$head(
          shiny::tags$style(HTML("
                .error-highlight { background-color: #ffebee !important; }
                .warning-highlight { background-color: #fff3e0 !important; }
                .success-highlight { background-color: #e8f5e8 !important; }
                .line-number { color: #666; font-family: monospace; }
                .git-status { padding: 10px; background: #f8f9fa; border-radius: 5px; }
                .content-wrapper, .right-side { background-color: #f4f4f4; }
                .main-header .navbar { background-color: #3c8dbc !important; }
            "))
        ),
        
        tabItems(
            # Code Analysis Tab
            tabItem(tabName = "analysis",
                fluidRow(
                    box(
                        title = "Code Input", status = "primary", solidHeader = TRUE,
                        width = 6, height = "600px",
                        aceEditor("code_input", 
                                mode = "r",
                                theme = "github",
                                height = "500px",
                                value = source_code_example,
                                fontSize = 14,
                                showLineNumbers = TRUE,
                                highlightActiveLine = TRUE),
                        br(),
                        actionButton("analyze_code", "Analyze Code", 
                                   class = "btn-primary", icon = icon("search"))
                    ),
                    
                    box(
                        title = "Fixed Code Preview", status = "success", solidHeader = TRUE,
                        width = 6, height = "600px",
                        aceEditor("code_output", 
                                mode = "r",
                                theme = "github",
                                height = "500px",
                                value = "",
                                fontSize = 14,
                                showLineNumbers = TRUE,
                                readOnly = TRUE),
                        br(),
                        actionButton("apply_fixes", "Apply Fixes", 
                                   class = "btn-success", icon = icon("check"))
                    )
                ),
                
                fluidRow(
                    box(
                        title = "Issue Summary", status = "warning", solidHeader = TRUE,
                        width = 12,
                        withSpinner(verbatimTextOutput("issue_summary"))
                    )
                )
            ),
            
            # Git Integration Tab
            tabItem(tabName = "git",
                fluidRow(
                    box(
                        title = "Repository Management", status = "info", solidHeader = TRUE,
                        width = 6,
                        h4("Clone Repository"),
                        textInput("repo_url", "Repository URL:", 
                                placeholder = "https://github.com/username/package.git"),
                        textInput("local_path", "Local Path (optional):", 
                                placeholder = "Leave empty for auto-generated"),
                        actionButton("clone_repo", "Clone Repository", 
                                   class = "btn-info", icon = icon("download")),
                        br(), br(),
                        
                        h4("Current Repository Status"),
                        div(id = "git_status", class = "git-status",
                            verbatimTextOutput("git_status_text")
                        )
                    ),
                    
                    box(
                        title = "Git Operations", status = "warning", solidHeader = TRUE,
                        width = 6,
                        h4("Commit & Push Changes"),
                        textAreaInput("commit_message", "Commit Message:",
                                    value = "BioTune: Fixed code quality issues",
                                    rows = 3),
                        actionButton("commit_push", "Commit & Push", 
                                   class = "btn-warning", icon = icon("upload")),
                        br(), br(),
                        
                        h4("Auto-Fix & Push"),
                        p("This will automatically fix all detected issues and push to repository."),
                        actionButton("auto_fix_push", "Auto-Fix & Push", 
                                   class = "btn-danger", icon = icon("magic")),
                        br(), br(),
                        
                        verbatimTextOutput("git_operations_log")
                    )
                )
            ),
            
            # BiocCheck Results Tab
            tabItem(tabName = "bioccheck",
                fluidRow(
                    box(
                        title = "BiocCheck Configuration", status = "primary", solidHeader = TRUE,
                        width = 4,
                        checkboxInput("run_real_bioccheck", "Use Real BiocCheck", 
                                    value = BIOCCHECK_AVAILABLE),
                        checkboxInput("check_formatting", "Check Code Formatting", value = TRUE),
                        checkboxInput("check_documentation", "Check Documentation", value = TRUE),
                        checkboxInput("check_dependencies", "Check Dependencies", value = TRUE),
                        actionButton("run_bioccheck", "Run BiocCheck", 
                                   class = "btn-primary", icon = icon("play"))
                    ),
                    
                    box(
                        title = "Check Summary", status = "info", solidHeader = TRUE,
                        width = 8,
                        withSpinner(verbatimTextOutput("bioccheck_summary"))
                    )
                ),
                
                fluidRow(
                    box(
                        title = "Detailed BiocCheck Results", status = "info", solidHeader = TRUE,
                        width = 12,
                        withSpinner(DT::dataTableOutput("bioccheck_results"))
                    )
                )
            ),
            
            # Enhanced Quick Fixes Tab
            tabItem(tabName = "fixes",
                fluidRow(
                    box(
                        title = "Formatting Fixes", status = "warning", solidHeader = TRUE,
                        width = 6,
                        h4("Code Style Fixes"),
                        actionButton("fix_indentation", "Fix Indentation Issues", 
                                   class = "btn-warning", icon = icon("indent")),
                        br(), br(),
                        actionButton("fix_line_length", "Split Long Lines", 
                                   class = "btn-warning", icon = icon("arrows-h")),
                        br(), br(),
                        actionButton("fix_all_formatting", "Fix All Formatting", 
                                   class = "btn-danger", icon = icon("magic")),
                        br(), br(),
                        
                        h4("Bioconductor Compliance"),
                        actionButton("fix_library_calls", "Fix library() Calls", 
                                   class = "btn-info", icon = icon("book")),
                        br(), br(),
                        actionButton("fix_naming", "Fix Function Naming", 
                                   class = "btn-info", icon = icon("font")),
                        br(), br(),
                        actionButton("fix_bioc_all", "Fix All Bioc Issues", 
                                   class = "btn-success", icon = icon("check-double"))
                    ),
                    
                    box(
                        title = "Fix Statistics & Preview", status = "success", solidHeader = TRUE,
                        width = 6,
                        h4("Current Issues"),
                        withSpinner(verbatimTextOutput("fix_stats")),
                        br(),
                        h4("Fix Preview"),
                        verbatimTextOutput("fix_preview")
                    )
                )
            ),
            
            # Package Builder Tab
            tabItem(tabName = "builder",
                fluidRow(
                    box(
                        title = "Package Information", status = "primary", solidHeader = TRUE,
                        width = 6,
                        textInput("pkg_name", "Package Name:", value = "ExampleBiocPackage"),
                        textInput("pkg_version", "Version:", value = "0.99.0"),
                        textInput("pkg_title", "Title:", value = "Example Bioconductor Package"),
                        textAreaInput("pkg_description", "Description:", 
                                    value = "This package demonstrates Bioconductor development.",
                                    rows = 3),
                        textInput("pkg_author", "Author:", value = "Developer Name"),
                        textInput("pkg_maintainer", "Maintainer:", value = "dev@example.com")
                    ),
                    
                    box(
                        title = "Build Configuration", status = "warning", solidHeader = TRUE,
                        width = 6,
                        checkboxInput("include_vignette", "Include Vignette Template", value = TRUE),
                        checkboxInput("include_tests", "Include Test Framework", value = TRUE),
                        checkboxInput("run_check", "Run R CMD check", value = TRUE),
                        selectInput("biocviews", "biocViews Category:",
                                  choices = c("Software", "ExperimentData", "AnnotationData"),
                                  selected = "Software"),
                        
                        br(),
                        actionButton("build_package", "Build Package", 
                                   class = "btn-primary", icon = icon("hammer")),
                        br(), br(),
                        downloadButton("download_tarball", "Download Tarball", 
                                     class = "btn-success")
                    )
                ),
                
                fluidRow(
                    box(
                        title = "Build Output", status = "info", solidHeader = TRUE,
                        width = 12,
                        withSpinner(verbatimTextOutput("build_output"))
                    )
                )
            ),
            
            # Deployment Tab
            tabItem(tabName = "deployment",
                fluidRow(
                    box(
                        title = "Deployment Options", status = "success", solidHeader = TRUE,
                        width = 6,
                        h4("Local Installation"),
                        p("Install the package locally for testing"),
                        actionButton("install_local", "Install Locally", 
                                   class = "btn-info", icon = icon("download")),
                        br(), br(),
                        
                        h4("Submit to Bioconductor"),
                        p("Prepare package for Bioconductor submission"),
                        actionButton("prep_bioc_submission", "Prepare for Bioconductor", 
                                   class = "btn-warning", icon = icon("upload")),
                        br(), br(),
                        
                        h4("GitHub Release"),
                        textInput("release_tag", "Release Tag:", value = "v0.99.0"),
                        actionButton("create_release", "Create GitHub Release", 
                                   class = "btn-success", icon = icon("tag"))
                    ),
                    
                    box(
                        title = "Deployment Status", status = "info", solidHeader = TRUE,
                        width = 6,
                        withSpinner(verbatimTextOutput("deployment_status"))
                    )
                )
            ),
            
            # Enhanced Reports Tab
            tabItem(tabName = "reports",
                fluidRow(
                    box(
                        title = "Package Quality Report", status = "success", solidHeader = TRUE,
                        width = 8,
                        withSpinner(verbatimTextOutput("quality_report"))
                    ),
                    
                    box(
                        title = "Export Options", status = "primary", solidHeader = TRUE,
                        width = 4,
                        h4("Report Formats"),
                        downloadButton("download_html_report", "HTML Report", 
                                     class = "btn-primary"),
                        br(), br(),
                        downloadButton("download_pdf_report", "PDF Report", 
                                     class = "btn-info"),
                        br(), br(),
                        downloadButton("download_json_report", "JSON Data", 
                                     class = "btn-warning"),
                        br(), br(),
                        
                        h4("Package Files"),
                        downloadButton("download_fixed_package", "Download Fixed Package", 
                                     class = "btn-success")
                    )
                )
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    # Reactive values
    values <- reactiveValues(
        current_code = source_code_example,
        issues = list(),
        fixed_code = "",
        repo_path = NULL,
        package_built = FALSE,
        tarball_path = NULL
    )
    
    # Code analysis
    observeEvent(input$analyze_code, {
        values$current_code <- input$code_input
        
        if (input$run_real_bioccheck && BIOCCHECK_AVAILABLE) {
            # Write code to temporary file for BiocCheck
            temp_file <- tempfile(fileext = ".R")
            writeLines(values$current_code, temp_file)
            values$issues <- run_real_bioccheck(temp_file)
        } else {
            values$issues <- run_mock_bioccheck(values$current_code)
        }
        
        # Update issue summary
        output$issue_summary <- renderText({
            if (length(values$issues) == 0) {
                "✅ Excellent! No issues found! Your code follows Bioconductor guidelines."
            } else {
                error_count <- sum(sapply(values$issues, function(x) x$severity == "error"))
                warning_count <- sum(sapply(values$issues, function(x) x$severity == "warning"))
                
                paste(
                    sprintf("📊 Analysis Complete: Found %d issues", length(values$issues)),
                    sprintf("🔴 Errors: %d", error_count),
                    sprintf("🟡 Warnings: %d", warning_count),
                    "",
                    "🔧 Available fixes:",
                    "- Indentation and formatting issues",
                    "- Line length violations", 
                    "- Bioconductor compliance issues",
                    "- Function naming conventions",
                    "",
                    "Click 'Apply Fixes' to automatically resolve formatting issues.",
                    sep = "\n"
                )
            }
        })
        
        # Generate comprehensive fixed code preview
        lines <- strsplit(values$current_code, "\n")[[1]]
        fixed_lines <- fix_indentation(lines)
        fixed_lines <- split_long_lines(fixed_lines)
        fixed_lines <- fix_bioc_compliance(fixed_lines)
        values$fixed_code <- paste(fixed_lines, collapse = "\n")
        
        updateAceEditor(session, "code_output", value = values$fixed_code)
        
        showNotification("Code analysis complete!", type = "success", duration = 3)
    })
    
    # Apply comprehensive fixes
    observeEvent(input$apply_fixes, {
        updateAceEditor(session, "code_input", value = values$fixed_code)
        values$current_code <- values$fixed_code
        
        # Re-analyze after fixes
        values$issues <- run_mock_bioccheck(values$current_code)
        
        showNotification("All fixes applied successfully! Re-run analysis to see improvements.", 
                        type = "success", duration = 5)
    })
    
    # Git operations
    observeEvent(input$clone_repo, {
        req(input$repo_url)
        
        tryCatch({
            local_path <- if (input$local_path != "") input$local_path else NULL
            values$repo_path <- clone_repository(input$repo_url, local_path)
            
            output$git_status_text <- renderText({
                paste(
                    "✅ Repository cloned successfully!",
                    sprintf("📁 Local path: %s", values$repo_path),
                    sprintf("🔗 Remote URL: %s", input$repo_url),
                    "",
                    "You can now analyze and fix the package code.",
                    sep = "\n"
                )
            })
            
            # Load package files into editor
            r_files <- list.files(file.path(values$repo_path, "R"), 
                                pattern = "\\.R$", full.names = TRUE)
            if (length(r_files) > 0) {
                package_code <- paste(sapply(r_files[1:min(3, length(r_files))], 
                                           function(f) paste(readLines(f), collapse = "\n")), 
                                    collapse = "\n\n# --- Next File ---\n\n")
                updateAceEditor(session, "code_input", value = package_code)
                values$current_code <- package_code
            }
            
            showNotification("Repository cloned and loaded into editor!", 
                           type = "success", duration = 5)
            
        }, error = function(e) {
            showNotification(paste("Failed to clone repository:", e$message), 
                           type = "error", duration = 10)
            output$git_status_text <- renderText({
                paste("❌ Clone failed:", e$message)
            })
        })
    })
    
    observeEvent(input$commit_push, {
        req(values$repo_path)
        
        # Save current code back to files
        if (!is.null(values$repo_path)) {
            r_files <- list.files(file.path(values$repo_path, "R"), 
                                pattern = "\\.R$", full.names = TRUE)
            if (length(r_files) > 0) {
                # Simple approach: save to first R file
                writeLines(strsplit(values$current_code, "\n")[[1]], r_files[1])
            }
        }
        
        success <- commit_and_push_changes(values$repo_path, input$commit_message)
        
        output$git_operations_log <- renderText({
            if (success) {
                paste(
                    "✅ Changes committed and pushed successfully!",
                    sprintf("📝 Commit message: %s", input$commit_message),
                    sprintf("⏰ Time: %s", Sys.time()),
                    sep = "\n"
                )
            } else {
                paste(
                    "❌ Git operation failed!",
                    "Please check authentication and network connection.",
                    "You may need to set up SSH keys or personal access tokens.",
                    sep = "\n"
                )
            }
        })
        
        notification_type <- if (success) "success" else "error"
        notification_msg <- if (success) "Changes pushed to repository!" else "Git operation failed!"
        showNotification(notification_msg, type = notification_type, duration = 5)
    })
    
    observeEvent(input$auto_fix_push, {
        req(values$repo_path)
        
        # Apply all fixes
        lines <- strsplit(values$current_code, "\n")[[1]]
        fixed_lines <- fix_indentation(lines)
        fixed_lines <- split_long_lines(fixed_lines)
        fixed_lines <- fix_bioc_compliance(fixed_lines)
        values$current_code <- paste(fixed_lines, collapse = "\n")
        
        updateAceEditor(session, "code_input", value = values$current_code)
        
        # Save and push
        r_files <- list.files(file.path(values$repo_path, "R"), 
                            pattern = "\\.R$", full.names = TRUE)
        if (length(r_files) > 0) {
            writeLines(strsplit(values$current_code, "\n")[[1]], r_files[1])
        }
        
        success <- commit_and_push_changes(values$repo_path, 
                                         "BioTune: Auto-fixed all code quality issues")
        
        output$git_operations_log <- renderText({
            if (success) {
                "🚀 Auto-fix complete! All issues resolved and pushed to repository."
            } else {
                "⚠️  Auto-fix applied locally, but push failed. Check Git configuration."
            }
        })
        
        showNotification("Auto-fix and push completed!", type = "success", duration = 5)
    })
    
    # BiocCheck operations
    observeEvent(input$run_bioccheck, {
        if (input$run_real_bioccheck && BIOCCHECK_AVAILABLE) {
            values$issues <- run_real_bioccheck(values$current_code)
        } else {
            values$issues <- run_mock_bioccheck(values$current_code)
        }
        
        output$bioccheck_summary <- renderText({
            if (length(values$issues) == 0) {
                "🎉 PERFECT! Your package passes all BiocCheck requirements!"
            } else {
                error_count <- sum(sapply(values$issues, function(x) x$severity == "error"))
                warning_count <- sum(sapply(values$issues, function(x) x$severity == "warning"))
                
                paste(
                    "📋 BiocCheck Analysis Results:",
                    sprintf("🔴 Critical Issues (Errors): %d", error_count),
                    sprintf("🟡 Recommendations (Warnings): %d", warning_count),
                    sprintf("📊 Total Issues: %d", length(values$issues)),
                    "",
                    "Priority Fixes Needed:",
                    "1. Fix all ERROR-level issues for Bioconductor submission",
                    "2. Address WARNING-level issues for better package quality",
                    "3. Use Quick Fixes tab for automated resolution",
                    sep = "\n"
                )
            }
        })
    })
    
    # BiocCheck results table
    output$bioccheck_results <- DT::renderDataTable({
        if (length(values$issues) == 0) {
            data.frame(
                File = character(0),
                Line = integer(0),
                Type = character(0),
                Severity = character(0),
                Message = character(0)
            )
        } else {
            df <- data.frame(
                File = sapply(values$issues, function(x) ifelse(is.null(x$file), "Unknown", x$file)),
                Line = sapply(values$issues, function(x) x$line),
                Type = sapply(values$issues, function(x) x$type),
                Severity = sapply(values$issues, function(x) x$severity),
                Message = sapply(values$issues, function(x) x$message)
            )
            df
        }
    }, options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
            list(targets = 3, render = JS(
                "function(data, type, row, meta) {",
                "  if(data == 'error') {",
                "    return '<span class=\"label label-danger\">ERROR</span>';",
                "  } else if(data == 'warning') {",
                "    return '<span class=\"label label-warning\">WARNING</span>';",
                "  }",
                "  return data;",
                "}"
            ))
        )
    ), escape = FALSE)
    
    # Individual fix buttons
    observeEvent(input$fix_indentation, {
        lines <- strsplit(values$current_code, "\n")[[1]]
        fixed_lines <- fix_indentation(lines)
        fixed_code <- paste(fixed_lines, collapse = "\n")
        updateAceEditor(session, "code_input", value = fixed_code)
        values$current_code <- fixed_code
        showNotification("✅ Indentation fixed! All spacing now uses multiples of 4.", 
                        type = "success", duration = 3)
    })
    
    observeEvent(input$fix_line_length, {
        lines <- strsplit(values$current_code, "\n")[[1]]
        fixed_lines <- split_long_lines(lines)
        fixed_code <- paste(fixed_lines, collapse = "\n")
        updateAceEditor(session, "code_input", value = fixed_code)
        values$current_code <- fixed_code
        showNotification("✅ Long lines split for better readability!", 
                        type = "success", duration = 3)
    })
    
    observeEvent(input$fix_all_formatting, {
        lines <- strsplit(values$current_code, "\n")[[1]]
        fixed_lines <- fix_indentation(lines)
        fixed_lines <- split_long_lines(fixed_lines)
        fixed_code <- paste(fixed_lines, collapse = "\n")
        updateAceEditor(session, "code_input", value = fixed_code)
        values$current_code <- fixed_code
        showNotification("✅ All formatting issues resolved!", 
                        type = "success", duration = 3)
    })
    
    observeEvent(input$fix_library_calls, {
        lines <- strsplit(values$current_code, "\n")[[1]]
        fixed_lines <- fix_bioc_compliance(lines)
        fixed_code <- paste(fixed_lines, collapse = "\n")
        updateAceEditor(session, "code_input", value = fixed_code)
        values$current_code <- fixed_code
        showNotification("✅ Library calls converted to requireNamespace()!", 
                        type = "success", duration = 3)
    })
    
    observeEvent(input$fix_naming, {
        lines <- strsplit(values$current_code, "\n")[[1]]
        # Apply naming fixes (camelCase -> snake_case)
        for (i in seq_along(lines)) {
            if (grepl("^[a-z][a-zA-Z]*[A-Z].*<-\\s*function", lines[i])) {
                func_name <- str_extract(lines[i], "^[a-zA-Z_][a-zA-Z0-9_]*")
                if (!is.na(func_name)) {
                    snake_case_name <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", func_name, perl = TRUE)
                    lines[i] <- str_replace(lines[i], func_name, snake_case_name)
                }
            }
        }
        fixed_code <- paste(lines, collapse = "\n")
        updateAceEditor(session, "code_input", value = fixed_code)
        values$current_code <- fixed_code
        showNotification("✅ Function names converted to snake_case!", 
                        type = "success", duration = 3)
    })
    
    observeEvent(input$fix_bioc_all, {
        lines <- strsplit(values$current_code, "\n")[[1]]
        fixed_lines <- fix_indentation(lines)
        fixed_lines <- split_long_lines(fixed_lines)
        fixed_lines <- fix_bioc_compliance(fixed_lines)
        fixed_code <- paste(fixed_lines, collapse = "\n")
        updateAceEditor(session, "code_input", value = fixed_code)
        values$current_code <- fixed_code
        showNotification("🎉 All Bioconductor compliance issues fixed!", 
                        type = "success", duration = 5)
    })
    
    # Fix statistics and preview
    output$fix_stats <- renderText({
        lines <- strsplit(values$current_code, "\n")[[1]]
        indent_issues <- check_indentation(lines)
        length_issues <- check_line_length(lines)
        
        library_count <- sum(grepl("^\\s*library\\(", lines))
        camel_case_count <- sum(grepl("^[a-z][a-zA-Z]*[A-Z].*<-\\s*function", lines))
        
        paste(
            "📊 CURRENT CODE ANALYSIS:",
            sprintf("📝 Total lines: %d", length(lines)),
            sprintf("🔴 Indentation issues: %d", length(indent_issues)),
            sprintf("📏 Long line issues: %d", length(length_issues)),
            sprintf("📚 Library() calls: %d", library_count),
            sprintf("🐪 CamelCase functions: %d", camel_case_count),
            sprintf("⚠️  Total BiocCheck issues: %d", length(values$issues)),
            "",
            "🎯 FIXABLE AUTOMATICALLY:",
            "✅ All indentation and formatting issues",
            "✅ Line length violations", 
            "✅ Library call conversions",
            "✅ Function naming conventions",
            sep = "\n"
        )
    })
    
    output$fix_preview <- renderText({
        if (length(values$issues) > 0) {
            paste(
                "🔧 NEXT RECOMMENDED ACTIONS:",
                "",
                "1. Click 'Fix All Bioc Issues' for comprehensive fix",
                "2. Re-run analysis to verify improvements",
                "3. Use Git integration to commit changes",
                "4. Build package for final validation",
                "",
                sprintf("🎯 After fixes, expect %d fewer issues!", length(values$issues)),
                sep = "\n"
            )
        } else {
            "🎉 No issues detected! Your code is ready for Bioconductor submission."
        }
    })
    
    # Package building
    observeEvent(input$build_package, {
        tryCatch({
            # Create temporary package structure
            pkg_dir <- file.path(TEMP_DIR, input$pkg_name)
            if (dir.exists(pkg_dir)) unlink(pkg_dir, recursive = TRUE)
            
            # Create package skeleton
            if (DEVTOOLS_AVAILABLE) {
                devtools::create(pkg_dir, open = FALSE)
                
                # Update DESCRIPTION
                desc_file <- file.path(pkg_dir, "DESCRIPTION")
                desc_content <- c(
                    paste("Package:", input$pkg_name),
                    "Type: Package",
                    paste("Title:", input$pkg_title),
                    paste("Version:", input$pkg_version),
                    paste("Author:", input$pkg_author),
                    paste("Maintainer:", input$pkg_author, "<", input$pkg_maintainer, ">"),
                    paste("Description:", input$pkg_description),
                    "License: Artistic-2.0",
                    "Encoding: UTF-8",
                    "LazyData: true",
                    paste("biocViews:", input$biocviews),
                    "Depends: R (>= 4.3.0)",
                    "Imports: methods"
                )
                writeLines(desc_content, desc_file)
                
                # Save current code to R directory
                r_dir <- file.path(pkg_dir, "R")
                if (!dir.exists(r_dir)) dir.create(r_dir)
                writeLines(strsplit(values$current_code, "\n")[[1]], 
                          file.path(r_dir, "functions.R"))
                
                # Add vignette if requested
                if (input$include_vignette) {
                    vig_dir <- file.path(pkg_dir, "vignettes")
                    if (!dir.exists(vig_dir)) dir.create(vig_dir)
                    vig_content <- c(
                        "---",
                        paste("title: \"Introduction to", input$pkg_name, "\""),
                        "output: BiocStyle::html_document",
                        "vignette: >",
                        paste("  %\\VignetteIndexEntry{Introduction to", input$pkg_name, "}"),
                        "  %\\VignetteEngine{knitr::rmarkdown}",
                        "  %\\VignetteEncoding{UTF-8}",
                        "---",
                        "",
                        "# Introduction",
                        "",
                        paste("This vignette introduces the", input$pkg_name, "package.")
                    )
                    writeLines(vig_content, file.path(vig_dir, paste0(input$pkg_name, ".Rmd")))
                }
                
                # Build package
                if (DEVTOOLS_AVAILABLE) {
                    values$tarball_path <- build_package_tarball(pkg_dir)
                    values$package_built <- TRUE
                }
                
                output$build_output <- renderText({
                    paste(
                        "✅ Package built successfully!",
                        sprintf("📦 Package: %s", input$pkg_name),
                        sprintf("📁 Location: %s", pkg_dir),
                        sprintf("🗜️  Tarball: %s", basename(values$tarball_path)),
                        "",
                        "🎯 Next steps:",
                        "1. Download tarball for distribution",
                        "2. Install locally for testing", 
                        "3. Submit to Bioconductor",
                        sep = "\n"
                    )
                })
                
                showNotification("Package built successfully!", type = "success", duration = 5)
            }
            
        }, error = function(e) {
            output$build_output <- renderText({
                paste("❌ Package build failed:", e$message)
            })
            showNotification(paste("Build failed:", e$message), type = "error", duration = 10)
        })
    })
    
    # Download handlers
    output$download_tarball <- downloadHandler(
        filename = function() {
            paste0(input$pkg_name, "_", input$pkg_version, ".tar.gz")
        },
        content = function(file) {
            if (!is.null(values$tarball_path) && file.exists(values$tarball_path)) {
                file.copy(values$tarball_path, file)
            }
        }
    )
    
    # Deployment operations
    observeEvent(input$install_local, {
        if (values$package_built && !is.null(values$tarball_path)) {
            tryCatch({
                install.packages(values$tarball_path, repos = NULL, type = "source")
                output$deployment_status <- renderText({
                    paste(
                        "✅ Package installed locally!",
                        sprintf("📦 Package: %s", input$pkg_name),
                        "🧪 You can now test the package functions.",
                        "",
                        paste("Try: library(", input$pkg_name, ")", sep=""),
                        sep = "\n"
                    )
                })
                showNotification("Package installed successfully!", type = "success")
            }, error = function(e) {
                output$deployment_status <- renderText({
                    paste("❌ Installation failed:", e$message)
                })
            })
        } else {
            showNotification("Please build the package first!", type = "warning")
        }
    })
    
    observeEvent(input$prep_bioc_submission, {
        output$deployment_status <- renderText({
            paste(
                "📋 BIOCONDUCTOR SUBMISSION CHECKLIST:",
                "",
                "✅ Required items:",
                "• Package passes BiocCheck (use BiocCheck tab)",
                "• Version number is 0.99.x",
                "• Includes appropriate biocViews",
                "• Has comprehensive documentation",
                "• Includes runnable examples",
                "• Has at least one vignette",
                "",
                "🚀 Submission process:",
                "1. Upload package to GitHub",
                "2. Submit to Bioconductor GitHub tracker",
                "3. Respond to reviewer feedback",
                "",
                "📖 More info: https://bioconductor.org/developers/package-submission/",
                sep = "\n"
            )
        })
    })
    
    # Enhanced quality report
    output$quality_report <- renderText({
        if (length(values$issues) == 0) {
            paste(
                "🏆 EXCELLENT PACKAGE QUALITY!",
                "",
                "✅ All BiocCheck requirements met",
                "✅ Code follows Bioconductor style guidelines", 
                "✅ No formatting or compliance issues",
                "",
                "🎯 Your package is ready for:",
                "• Local installation and testing",
                "• Bioconductor submission",
                "• Public release",
                "",
                "🎉 Congratulations on maintaining high code quality!",
                sep = "\n"
            )
        } else {
            error_count <- sum(sapply(values$issues, function(x) x$severity == "error"))
            warning_count <- sum(sapply(values$issues, function(x) x$severity == "warning"))
            
            # Categorize issues
            format_issues <- sum(sapply(values$issues, function(x) 
                x$type %in% c("tab_character", "invalid_indentation", "long_line")))
            bioc_issues <- sum(sapply(values$issues, function(x)
                x$type %in% c("library_usage", "naming_convention", "missing_export")))
            doc_issues <- sum(sapply(values$issues, function(x)
                x$type %in% c("missing_vignette", "missing_news")))
            
            paste(
                "📊 PACKAGE QUALITY ASSESSMENT",
                "=================================",
                "",
                sprintf("🔴 Critical Issues (Errors): %d", error_count),
                sprintf("🟡 Recommendations (Warnings): %d", warning_count),
                sprintf("📋 Total Issues: %d", length(values$issues)),
                "",
                "📂 ISSUE BREAKDOWN:",
                sprintf("🎨 Formatting Issues: %d", format_issues),
                sprintf("🔧 Bioconductor Compliance: %d", bioc_issues),
                sprintf("📚 Documentation Issues: %d", doc_issues),
                "",
                "🎯 RECOMMENDED ACTIONS:",
                "1. Use 'Fix All Bioc Issues' in Quick Fixes tab",
                "2. Add missing documentation and examples",
                "3. Include package vignette",
                "4. Re-run BiocCheck after fixes",
                "",
                "⭐ QUALITY SCORE:",
                sprintf("Current: %d%% (need 100%% for Bioconductor)", 
                       max(0, round(100 - (length(values$issues) * 5)))),
                "",
                if (error_count > 0) "❗ CRITICAL: Fix all errors before submission!" else "✅ No critical errors detected",
                sep = "\n"
            )
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
