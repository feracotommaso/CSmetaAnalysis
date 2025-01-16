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