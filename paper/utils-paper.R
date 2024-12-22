trim_df <- function(data, n = 4, digits = 3){
    data <- lapply(data, function(x) if(is.factor(x)) as.character(x) else x)
    data <- data.frame(data)
    data <- data.frame(sapply(data, function(x) if(is.numeric(x)) format(x, nsmall=digits) else x))
    # dots <- data[1, ]
    # dots[1, ] <- "..."
    # nrows <- nrow(data)
    # if(nrows <= 5){
    #     trimmed <- data
    # } else{
    #     if(nrows <= n*2){
    #         n <- floor(n/2)
    #     }
    #     trimmed <- rbind(
    #         data[1:n,],
    #         dots,
    #         data[(nrows-(n - 1)):nrows, ]
    #     )
    # }
    # rownames(trimmed) <- NULL
    return(data)
}

knitrMeta <- function(knitTab, formatOut, capt = "", foot = NA, scaled=TRUE){
  if (formatOut == "docx") {
    # Use a Word-compatible table
    knitr::kable(
      knitTab,
      format = "pandoc",
      format.args = list(big.mark = ","),
      align = c("l", rep("c",(ncol(knitTab)-1))),
      caption = capt
    )
    #%>%
    #  add_footnote(label = foot, notation = "symbol")
  } else {
    # Use a PDF-compatible table (LaTeX)
    knitr::kable(
      knitTab,
      format = "latex",
      booktabs = TRUE,
      format.args = list(big.mark = ","),
      align = c("l", rep("c",(ncol(knitTab)-1))),
      caption = capt
    ) %>%
      {if (scaled == TRUE) kableExtra::kable_styling(., latex_options = "scale_down") else .} %>%
      kableExtra::add_footnote(label = foot, notation = "symbol")
  } 
}