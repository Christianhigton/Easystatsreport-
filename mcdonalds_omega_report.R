



mcdonalds_omega_report <- function(data, 
                                   items = NULL,
                                   digits = 2) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("The 'psych' package is required. Please install it using install.packages('psych').")
  }
  
  if (!is.null(items)) {
    missing_items <- items[!items %in% colnames(data)]
    if (length(missing_items) > 0) {
      stop(paste("The following items were not found in the data frame:", 
                 paste(missing_items, collapse = ", ")))
    }
    data <- data[, items]
  }
  
  omega_result <- suppressWarnings(psych::omega(data, nfactors = 1, warnings = FALSE))
  omega_value <- round(omega_result$omega.tot, digits)
  
  interpretation <- dplyr::case_when(
    omega_value >= 0.90 ~ "excellent",
    omega_value >= 0.80 ~ "good",
    omega_value >= 0.70 ~ "acceptable",
    omega_value >= 0.60 ~ "questionable",
    omega_value >= 0.50 ~ "poor",
    TRUE ~ "unacceptable"
  )
  
  text <- paste0(
    "According to [INSERT CITATION HERE], [INSERT SCALE NAME HERE] has ", interpretation,
    " internal consistency, with a McDonald’s omega coefficient reported of [INSERT ORIGINAL OMEGA]. ",
    "In the current study, McDonald’s omega was ", omega_value, 
    ", based on the current sample."
  )
  
  return(text)
}
