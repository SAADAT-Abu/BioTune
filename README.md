# BioTune 🧬

**Complete Bioconductor Package Development & Quality Assurance Tool**

BioTune Pro is a comprehensive Shiny application designed to streamline the development, analysis, and deployment of Bioconductor-compliant R packages. It automates code quality checks, fixes common issues, and provides seamless Git integration for professional package development workflows.

![BioTune Dashboard](https://img.shields.io/badge/Shiny-Dashboard-blue) ![R Version](https://img.shields.io/badge/R-%E2%89%A5%204.3.0-brightgreen) ![License](https://img.shields.io/badge/License-MIT-yellow)

## ✨ Key Features

### 🔍 **Enhanced Code Analysis**
- **Real BiocCheck Integration**: Runs official Bioconductor package validation
- **Interactive Code Highlighting**: Problematic lines highlighted directly in editor
- **Local Package Linting**: Analyze R packages from local folder paths
- **Comprehensive Issue Detection**: Identifies formatting, style, and compliance problems
- **Interactive Code Editor**: Syntax-highlighted editor with real-time analysis
- **Side-by-side Comparison**: Preview fixes before applying changes

### 🛠️ **Intelligent Auto-Fixing**
- **Indentation Correction**: Ensures all indentation uses multiples of 4 spaces
- **Line Length Management**: Automatically splits lines exceeding 80 characters
- **Tab Character Removal**: Converts all tabs to proper spacing
- **Bioconductor Compliance**: Fixes library() calls, naming conventions, and more
- **One-Click Solutions**: Apply all fixes with a single button

### 🔧 **Git Integration**
- **Repository Cloning**: Clone any Git repository directly into BioTune
- **Auto-Load Package Code**: Automatically loads R package files for analysis
- **Commit & Push**: Save improvements back to the repository
- **Auto-Fix Workflow**: Automatically fix issues and push changes in one step

### 📦 **Package Building & Deployment**
- **Tarball Generation**: Create installable .tar.gz packages
- **Local Installation**: Install packages locally for testing
- **Bioconductor Submission Prep**: Comprehensive submission checklist
- **Professional Reports**: Generate quality assessment reports

### 📊 **Quality Assurance**
- **BiocCheck Compliance**: Validates against all Bioconductor requirements
- **Quality Scoring**: 0-100% quality assessment with detailed breakdown
- **Issue Categorization**: Errors, warnings, and recommendations
- **Progress Tracking**: Monitor improvements over time

## 🚀 Quick Start

### Prerequisites

```r
# Required packages
install.packages(c("shiny", "shinydashboard", "DT", "shinyAce", 
                   "stringr", "htmltools", "shinyjs", "shinyFiles", 
                   "shinycssloaders"))

# Optional but recommended for full functionality
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("BiocCheck")
install.packages(c("git2r", "devtools"))
```

### Installation & Launch

#### Option 1: Install as R Package (Recommended)

```r
# Install from GitHub (when published)
devtools::install_github("username/BioTune")

# Or install from local directory
devtools::install("/path/to/BioTune")

# Load and launch
library(BioTune)
runBioTune()
```

#### Option 2: Run Standalone Versions

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd BioTune
   ```

2. **Launch the Application**
   ```r
   # Option A: Run the stable working version
   shiny::runApp('app_working.R')
   
   # Option B: Run the full-featured version
   shiny::runApp('app.R')
   
   # Option C: Run the legacy single-file version
   shiny::runApp('biotune_app.r')
   ```

3. **Access the Interface**
   - Open your browser to the displayed URL (typically http://127.0.0.1:xxxx)
   - Start analyzing and improving your R packages!

## 📖 User Guide

### 🏁 Getting Started

#### **Option 1: Analyze Existing Code**
1. **Code Analysis Tab**: Paste your R code into the editor
2. **Click "Analyze Code"**: Review detected issues in the summary
3. **Preview Fixes**: See proposed improvements in the right panel
4. **Apply Fixes**: Click to automatically resolve issues

#### **Option 2: Git Workflow**
1. **Git Integration Tab**: Enter your repository URL
2. **Clone Repository**: Automatically loads package code
3. **Auto-Fix & Push**: One-click solution to fix and commit improvements
4. **Package Builder**: Generate distributable packages

### 🎯 Core Workflows

#### **Package Quality Assessment**
```
1. Load Code → 2. Run BiocCheck → 3. Review Issues → 4. Apply Fixes → 5. Validate
```

#### **Git-to-Deployment Pipeline**
```
1. Clone Repository → 2. Analyze & Fix → 3. Commit & Push → 4. Build Package → 5. Deploy
```

### 🔧 Feature Overview

| Tab | Purpose | Key Actions |
|-----|---------|-------------|
| **Code Analysis** | Interactive code editing and issue detection | Analyze, Preview, Apply Fixes |
| **Git Integration** | Repository management and version control | Clone, Commit, Push, Auto-Fix |
| **BiocCheck Results** | Detailed compliance reporting | Run checks, View results |
| **Quick Fixes** | One-click issue resolution | Fix formatting, compliance, naming |
| **Package Builder** | Create distributable packages | Configure, Build, Download |
| **Deployment** | Installation and submission prep | Install, Submit, Release |
| **Reports** | Quality assessment and documentation | Generate, Export, Download |

## 🎨 Screenshots & Examples

### **Before & After Code Comparison**
```r
# BEFORE: Issues detected
calculate_stats <- function(data, method = 'mean',
                          use_weights = TRUE,
                            normalize = FALSE) {
	if (method == 'mean') {  # Tab character + wrong indentation
  result <- mean(data$values, na.rm = TRUE)  # 2 spaces
      if (use_weights) {  # 6 spaces
        result <- weighted.mean(data$values, data$weights, na.rm = TRUE)
	}
}

# AFTER: BioTune fixes applied
calculate_stats <- function(data, method = 'mean',
                           use_weights = TRUE,
                           normalize = FALSE) {
    if (method == 'mean') {  # Proper 4-space indentation
        result <- mean(data$values, na.rm = TRUE)
        if (use_weights) {
            result <- weighted.mean(data$values, data$weights,
                                  na.rm = TRUE)  # Line split at 80 chars
        }
    }
}
```

### **BiocCheck Issue Detection**
- ✅ **Fixed**: Tab characters converted to spaces
- ✅ **Fixed**: Indentation standardized to multiples of 4
- ✅ **Fixed**: Long lines split for readability
- ✅ **Fixed**: library() calls converted to requireNamespace()
- ✅ **Fixed**: Function names converted to snake_case

## 🧪 Supported Bioconductor Standards

### **Code Style Requirements**
- ✅ 4-space indentation (no tabs)
- ✅ 80-character line limit
- ✅ snake_case function naming
- ✅ Proper spacing around operators

### **Package Structure**
- ✅ Valid DESCRIPTION file
- ✅ Appropriate biocViews categories
- ✅ Version numbering (0.99.x for new packages)
- ✅ Required documentation

### **Best Practices**
- ✅ requireNamespace() instead of library()
- ✅ @export tags for public functions
- ✅ Comprehensive examples
- ✅ Package vignettes

## 🔧 Configuration & Customization

### **BiocCheck Settings**
- Toggle real BiocCheck vs. mock implementation
- Configure check severity levels
- Customize formatting rules

### **Git Configuration**
- Set up SSH keys for seamless push operations
- Configure commit message templates
- Set default branch preferences

### **Package Building Options**
- Include/exclude vignettes
- Configure test frameworks
- Set biocViews categories

## 🚨 Troubleshooting

### **Common Issues**

#### **"BiocCheck not available"**
```r
# Install BiocCheck
BiocManager::install("BiocCheck")
```

#### **Git operations fail**
- Ensure SSH keys are configured for your Git provider
- Check repository permissions
- Verify network connectivity

#### **Package build errors**
```r
# Install devtools if missing
install.packages("devtools")
```

### **R Version Compatibility**
- **Minimum**: R 4.3.0
- **Recommended**: R 4.4.x
- **Tested**: R 4.4.3

### **Package Dependencies**
If optional packages are missing, BioTune gracefully falls back to mock implementations while maintaining core functionality.

## 🤝 Contributing

We welcome contributions to BioTune Pro! Here's how you can help:

### **Bug Reports**
- Use clear, descriptive titles
- Include R version and package versions
- Provide reproducible examples

### **Feature Requests**
- Describe the use case and benefit
- Consider implementation complexity
- Provide mockups or examples if possible

### **Code Contributions**
- Follow existing code style
- Add tests for new features
- Update documentation

## 📝 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🙏 Acknowledgments

- **Bioconductor Team**: For providing the BiocCheck package and development standards
- **Shiny Community**: For the excellent web framework and extensions
- **R Core Team**: For the robust R programming environment

## 📞 Support

- **Issues**: Report bugs and request features via GitHub Issues
- **Documentation**: Comprehensive help available in the application
- **Community**: Join the discussion on Bioconductor support forums

---

**Ready to create publication-quality Bioconductor packages?** 🚀

Start with BioTune Pro and transform your R package development workflow today!

```r
# Launch BioTune Pro
shiny::runApp('biotune_app.r')
```
