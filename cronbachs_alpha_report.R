cronbachs_alpha_report <- function(data, 
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
  
  alpha_result <- psych::alpha(data)
  alpha_value <- round(alpha_result$total$raw_alpha, digits)
  
  interpretation <- dplyr::case_when(
    alpha_value >= 0.90 ~ "excellent",
    alpha_value >= 0.80 ~ "good",
    alpha_value >= 0.70 ~ "acceptable",
    alpha_value >= 0.60 ~ "questionable",
    alpha_value >= 0.50 ~ "poor",
    TRUE ~ "unacceptable"
  )
  
  text <- paste0(
    "According to [INSERT CITATION HERE], [INSERT SCALE NAME HERE] has ", interpretation,
    " internal consistency, with a Cronbach’s alpha coefficient reported of [INSERT ORIGINAL ALPHA]. ",
    "In the current study, the Cronbach’s alpha coefficient was ", alpha_value, 
    ", based on the current sample."
  )
  
  return(text)
}
