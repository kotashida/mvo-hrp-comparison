# Script to install required packages

required_packages <- c(
  "jsonlite",
  "quantmod",
  "xts",
  "zoo",
  "PerformanceAnalytics",
  "quadprog",
  "ggplot2",
  "reshape2",
  "tidyr"
)

# Identify missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(new_packages)) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, repos = "http://cran.us.r-project.org")
} else {
    message("All required packages are already installed.")
}
