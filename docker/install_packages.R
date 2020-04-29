pckgs <- c("dplyr", "tidyr", "shiny", "openxlsx", "stringr", "DT", "foreach")
for (pckg in pckgs){
  if(!requireNamespace(pckg, quietly = TRUE)) {
    install.packages(pckg)
  }
}