caterpillars2 <- function(object, mod = "1",  group, xlab, overall = TRUE, transfm = c("none", "tanh"), 
                         colerrorbar = "#88CCEE", colpoint = "#332288", colpoly = "#661100",pred = FALSE,  
                         k = TRUE, g = TRUE, at = NULL, by = NULL, weights = "prop") {

  if(any(class(object) %in% c("rma.mv", "rma"))){

    if(mod != "1"){
      results <-  mod_results(object, mod, group,
                                       by = by, at = at, weights = weights)
    } else {
      results <-  mod_results(object, mod = "1", group,
                                       by = by, at = at, weights = weights)
    }
  }

  if (any(class(object) %in% c("orchard"))) {results <- object}

       ## evaluate choices
  transfm <- match.arg(transfm) # if not specified it takes the first choice

  # meta-analytic results
  mod_table <- results$mod_table

  # data set
  data <- results$data
  data$lower <- data$yi - stats::qnorm(0.975)*sqrt(data$vi)
  data$upper <- data$yi + stats::qnorm(0.975)*sqrt(data$vi)

  if(transfm == "tanh"){
    cols <- sapply(mod_table, is.numeric)
    mod_table[,cols] <- Zr_to_r(mod_table[,cols])
    data$yi <- Zr_to_r(data$yi)
    data$lower <- Zr_to_r(data$lower)
    data$upper <- Zr_to_r(data$upper)
    label <- xlab
  }else{
    label <- xlab
  }

  if("Intrcpt" %in% mod_table$name){
    mod_table$name <- replace(as.vector(mod_table$name), which(mod_table$name == "Intrcpt"), "Overall")
  }

  # adding moderator names
  data$moderator <- factor(data$moderator, labels = mod_table$name)

  # data frame for the meta-analytic results
  mod_table$K <- as.vector(by(data, data[,"moderator"], function(x) length(x[,"yi"])))

  # Add in total levels of a grouping variable (e.g., study ID) within each moderator level.
  mod_table$g <- as.vector(num_studies(data, moderator, stdy)[,2])

  # the number of groups in a moderator & data points
  group_no <- nrow(mod_table)
  data_no <- nrow(data)

  # use dplyr here - need to change....
  # Dan can you make this basic R code - maybe I got it
  # data <- data[order(data$moderator, -data$yi),]
  data <- data %>% dplyr::group_by(moderator) %>% dplyr::arrange(moderator, dplyr::desc(yi)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Y = 1:data_no +
             unlist(mapply(function(x, y) rep(x*6 , y) , x = 1:group_no, y = mod_table$K))
    ) %>%
    data.frame()

  # mod ID
  mod_table$Y <- data %>% dplyr::group_by(moderator) %>%
    dplyr::summarise(Y = dplyr::first(Y)) %>%
    dplyr::select(Y) %>% t() %>% as.vector() -2

  # preparing for diamons for summary
  # modified from internal_viz_classicforest() from the R package, metaviz
  sum_data <- data.frame("x.diamond" = c(mod_table$lowerCL,
                                         mod_table$estimate ,
                                         mod_table$upperCL,
                                         mod_table$estimate ),
                         "y.diamond" = c(mod_table$Y,
                                         mod_table$Y + 1.2,
                                         mod_table$Y,
                                         mod_table$Y - 1.2),
                         "moderator" = rep(mod_table$name, times = 4)
  )

  # make caterpillars plot
  plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = yi, y = Y)) +
    # 95 % CI
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower, xmax = upper),
                            colour = colerrorbar, height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
    # creating dots for point estimates
    ggplot2::geom_point(colour = colpoint, size = 1) +
    # creating diamonsts (95% CI)
    ggplot2::geom_polygon(data = sum_data, ggplot2::aes(x = x.diamond, y = y.diamond, group = moderator), fill = colpoly) +
    #ggplot2::facet_wrap(~moderator, scales = "free_y", nrow = GN,  strip.position = "left") + # using facet_wrap - does not really work well
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, size = 8),# margin = margin(t=15, r=15, b=15, l=15)),
                   strip.background = ggplot2::element_rect(colour = NULL,
                                                   linetype = "blank",
                                                   fill = "gray90"),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::labs(x = label, y = "", parse = TRUE) +

    ggplot2::annotate('text', x = min(data$lower)*0.975, y = mod_table$Y,
                      label= mod_table$name, hjust = "left", size = 3.5) +
    ggplot2::coord_cartesian(xlim = c(min(data$lower)*1.05, max(data$upper)*1.05),
                    ylim = c((min(data$Y)-10), (max(data$Y)+4))
                    , expand = F)

  # putting k in
  if(k == TRUE && g == FALSE){
    plot <- plot +
      ggplot2::annotate('text', x = max(data$upper)*0.975, y = mod_table$Y-1.7,
                        label= paste("italic(k)==", mod_table$K), parse = TRUE, hjust = "right", size = 3.5)
  }

  # putting groups
  if(k == TRUE && g == TRUE){
    # get group numbers for moderator
    plot <- plot +
      ggplot2::annotate('text', x = max(data$upper)*0.975, y = mod_table$Y-1.7,
                        label= paste("italic(k)==", mod_table$K[1:group_no], "~~","(", mod_table$g[1:group_no], ")"), parse = TRUE, hjust = "right", size = 3.5)
  }

  if(pred == TRUE) {
    plot <- plot +
      # creating 95% prediction intervals
      ggplot2::geom_segment(data = mod_table, ggplot2::aes(x = lowerPR, y = Y, xend = upperPR, yend = Y, group = name))
  }
  return(plot)
}
