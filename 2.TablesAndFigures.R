############################################## OPEN RESULTS #########################################################
library(ggplot2)
library(metafor)
library(readxl)
library(flextable)

folder_path <- "R"  # Adjust this if the folder is located elsewhere
r_files <- list.files(folder_path, full.names = TRUE, pattern = "\\.R$")
for (r_file in r_files) {
  source(r_file)  # This will run the R code in the specified script file
}

load("results/strengths.R")
resList <- readRDS("results/resList.RDS")
effList <- readRDS("results/effList.RDS")

dtot = read_excel("metaData/finalMetaData.xlsx")
strengths24 <- colnames(dtot[, which(colnames(dtot) == "Appreciation_of_beauty"):
                               which(colnames(dtot) == "Zest")])
dtot[dtot$Valence == "neg", strengths24] <- dtot[dtot$Valence == "neg", strengths24] * -1

############################################## MAIN-ANALYSIS  #######################################################
metaRes <- effList[["overall"]]

Table2 <- data.frame(
  Strength = metaRes$strength,
  N = paste0(metaRes$N, " k = (", metaRes$k, ")"),
  r = format(round(metaRes$r, 3),nsmall=3),
  CI = paste0("[",format(round(metaRes$rcil,3),nsmall=3),"; ",
              format(round(metaRes$rciu,3),nsmall=3),"]"),
  se = format(round(metaRes$se,3),nsmall=3), 
  p = format(round(metaRes$p,4),3),
  q = paste0(round(metaRes$q), "(",metaRes$qdf,")",ifelse(metaRes$qp < .01, "*", "")),
  tau2 = format(round(metaRes$tau2,3),nsmall=3))
flextable(Table2)

(Figure1 <- ggplot(metaRes, aes(x = r, y = reorder(strength,24:1))) +
    geom_point(shape = 18, 
               color = "black", 
               size = 3) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(.00), linetype = "dashed") +
    labs(x = "Meta-analytical association", y = "Strength") +
    theme_bw(base_size = 18) +
    scale_x_continuous(breaks = seq(-.10,.50,by=.10)) +
    annotate("text", x = ifelse(min(metaRes$rcil) > -.05, -.17,min(metaRes$rcil)),
             y = 24:1, 
             label = paste0(Table2$r," ",Table2$CI), hjust = "left") +
    coord_cartesian(xlim = c(ifelse(min(metaRes$rcil) > -.05, -.15,
                                    min(metaRes$rcil)),
                             max(metaRes$rciu)))
)

############################################## WB/MH-ANALYSIS  ######################################################
modRes = effList[["mainMod"]]
Table3 <- data.frame(
  strength = modRes$strength,
  mentaHealth = paste0(format(modRes$r0,nsmall = 3),
                       " [",format(modRes$rcil0,nsmall = 3),"; ",
                       format(modRes$rciu0,nsmall = 3),"]"),
  N0 = paste0(modRes$N0, " k = (", modRes$k0, ")"),
  wellBeing   = paste0(format(modRes$r1,nsmall = 3),
                       " [",format(modRes$rcil1,nsmall = 3),"; ",
                       format(modRes$rciu1,nsmall = 3),"]"),
  N1 = paste0(modRes$N1, " k = (", modRes$k1, ")"),
  deltaR = format(modRes$deltaR, nsmall = 3),
  deltaCI = paste0("[",format(modRes$cil,nsmall = 3),"; ",
                   format(modRes$ciu,nsmall = 3),"]"),
  se = format(modRes$se,nsmall = 3), 
  z = format(modRes$z,nsmall = 3),
  p = format(modRes$p,4),
  tau2 = format(modRes$tau2,nsmall = 3),
  q = paste0(format(modRes$q), "(",modRes$qdf,")",ifelse(modRes$qp < .01, "*", "")),
  qM = paste0(format(modRes$qM), "(",modRes$qMdf,")",ifelse(modRes$qMp < .01, "*", ""))
)
flextable(Table3)

dPlot2 <- data.frame(r = c(modRes$r0, modRes$r1),
                     rcil = c(modRes$rcil0, modRes$rcil1),
                     rciu = c(modRes$rciu0, modRes$rciu1),
                     factor = rep(c("Mental health","Well-being"), each = 24),
                     strength = modRes$strength)
(Figure2 <- ggplot(dPlot2, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = 3) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_color_manual(values=c("#999999", "black")) +
    scale_x_continuous(breaks = seq(-.10,.60,by=.10)) +
    labs(x = "Meta-analytical association", y = "Strength",
         color = "Moderator") +
    theme_bw(base_size = 18) +
    annotate("text", x = -.18, y = 24:1,
             label = paste0(Table3$deltaR," ",Table3$deltaCI), hjust = "left") +
    coord_cartesian(xlim = c(-.16,max(dPlot2$rciu)))
)

############################################## VIA-WB-ANALYSIS ######################################################
wbmodViaRes <- effList[["wbVia"]]
Table5 <- data.frame(strength=wbmodViaRes$strength,
                     long = paste0(round(wbmodViaRes$r0,3),
                                   " [",round(wbmodViaRes$rcil0,3),"; ",
                                   round(wbmodViaRes$rciu0,3),"]"),
                     N0 = paste0(wbmodViaRes$N0, " k = (", wbmodViaRes$k0, ")"),
                     short   = paste0(round(wbmodViaRes$r1,3),
                                      " [",round(wbmodViaRes$rcil1,3),"; ",
                                      round(wbmodViaRes$rciu1,3),"]"),
                     N1 = paste0(wbmodViaRes$N1, " k = (", wbmodViaRes$k1, ")"),
                     deltaR = format(wbmodViaRes$deltaR, nsmall = 3),
                     deltaCI = paste0("[",format(wbmodViaRes$cil,nsmall = 3),"; ",
                                      format(wbmodViaRes$ciu,nsmall = 3),"]"),
                     se = format(wbmodViaRes$se,nsmall = 3), 
                     z = format(wbmodViaRes$z,nsmall = 3),
                     p = format(wbmodViaRes$p,4),
                     q = paste0(format(wbmodViaRes$q), "(",wbmodViaRes$qdf,")",ifelse(wbmodViaRes$qp < .01, "*", "")),
                     qM = paste0(format(wbmodViaRes$qM), "(",wbmodViaRes$qMdf,")",ifelse(wbmodViaRes$qMp < .01, "*", ""))
)
flextable(Table5)

dPlot4 <- data.frame(r = c(wbmodViaRes$r0, wbmodViaRes$r1),
                     rcil = c(wbmodViaRes$rcil0, wbmodViaRes$rcil1),
                     rciu = c(wbmodViaRes$rciu0, wbmodViaRes$rciu1),
                     factor = rep(c("Long","Short"), each = 24),
                     strength = wbmodViaRes$strength)

(Figure4 <- ggplot(dPlot4, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = 3) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_x_continuous(breaks = seq(-.10,.50,by=.10)) +
    labs(x = "Meta-analytical association", y = "Strength",
         color = "Moderator") +
    scale_color_manual(values=c("#999999", "black")) +
    theme_bw(base_size = 18) +
    annotate("text", x = -.19, y = 24:1,
             label = paste0(Table5$deltaR," ",Table5$deltaCI), hjust = "left") +
    coord_cartesian(xlim = c(-.17,max(dPlot4$rciu)))
)

############################################## VIA-MH-ANALYSIS ######################################################
mhmodViaRes <- effList[["mhVia"]]
Table8 <- data.frame(strength = mhmodViaRes$strength,
                     long = paste0(round(mhmodViaRes$r0,3),
                                   " [",round(mhmodViaRes$rcil0,3),"; ",
                                   round(mhmodViaRes$rciu0,3),"]"),
                     N0 = paste0(mhmodViaRes$N0, " k = (", mhmodViaRes$k0, ")"),
                     short   = paste0(round(mhmodViaRes$r1,3),
                                      " [",round(mhmodViaRes$rcil1,3),"; ",
                                      round(mhmodViaRes$rciu1,3),"]"),
                     N0 = paste0(mhmodViaRes$N1, " k = (", mhmodViaRes$k1, ")"),
                     deltaR = format(mhmodViaRes$deltaR, nsmall = 3),
                     deltaCI = paste0("[",format(mhmodViaRes$cil,nsmall = 3),"; ",
                                      format(mhmodViaRes$ciu,nsmall = 3),"]"),
                     se = format(mhmodViaRes$se,nsmall = 3), 
                     z = format(mhmodViaRes$z,nsmall = 3),
                     p = format(mhmodViaRes$p,4),
                     q = paste0(format(mhmodViaRes$q), "(",mhmodViaRes$qdf,")",ifelse(mhmodViaRes$qp < .01, "*", "")),
                     qM = paste0(format(mhmodViaRes$qM), "(",mhmodViaRes$qMdf,")",ifelse(mhmodViaRes$qMp < .01, "*", ""))
)
flextable(Table8)

dPlot7 <- data.frame(r = c(mhmodViaRes$r0, mhmodViaRes$r1),
                     rcil = c(mhmodViaRes$rcil0, mhmodViaRes$rcil1),
                     rciu = c(mhmodViaRes$rciu0, mhmodViaRes$rciu1),
                     factor = rep(c("Long","Short"), each = 24),
                     strength = mhmodViaRes$strength)

(Figure7 <- ggplot(dPlot7, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = 3) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_color_manual(values=c("#999999", "black")) +
    scale_x_continuous(breaks = seq(-.10,.60,by=.10)) +
    labs(x = "Meta-analytical association", y = "Strength",
         color = "Moderator") +
    theme_bw(base_size = 18) +
    annotate("text", x = -.24, y = 24:1,
             label = paste0(Table8$deltaR," ",Table8$deltaCI), hjust = "left") +
    coord_cartesian(xlim = c(-.21,max(dPlot7$rciu)))
)

############################################## OUT-WB-ANALYSIS ######################################################
wbMeasures = c("domsat","happy","lifesat","negaff","posaff","pwb","swb")
wbmodwbRes <- effList[["wbwb"]]
Table6 <- data.frame(strength = wbmodwbRes$strength,
                     domainSat = paste0(format(wbmodwbRes$r1,nsmall=3),
                                        " [",format(wbmodwbRes$rcil1,nsmall=3),"; ",
                                        format(wbmodwbRes$rciu1,nsmall=3),"]"),
                     happyness = paste0(format(wbmodwbRes$r2,nsmall=3),
                                        " [",format(wbmodwbRes$rcil2,nsmall=3),"; ",
                                        format(wbmodwbRes$rciu2,nsmall=3),"]"),
                     lifesatis = paste0(format(wbmodwbRes$r3,nsmall=3),
                                        " [",format(wbmodwbRes$rcil3,nsmall=3),"; ",
                                        format(wbmodwbRes$rciu3,nsmall=3),"]"),
                     negAffect = paste0(format(wbmodwbRes$r4,nsmall=3),
                                        " [",format(wbmodwbRes$rcil4,nsmall=3),"; ",
                                        format(wbmodwbRes$rciu4,nsmall=3),"]"),
                     posAffect = paste0(format(wbmodwbRes$r5,nsmall=3),
                                        " [",format(wbmodwbRes$rcil5,nsmall=3),"; ",
                                        format(wbmodwbRes$rciu5,nsmall=3),"]"),
                     psycWellb = paste0(format(wbmodwbRes$r6,nsmall=3),
                                        " [",format(wbmodwbRes$rcil6,nsmall=3),"; ",
                                        format(wbmodwbRes$rciu6,nsmall=3),"]"),
                     subjWellb = paste0(format(wbmodwbRes$r7,nsmall=3),
                                        " [",format(wbmodwbRes$rcil7,nsmall=3),"; ",
                                        format(wbmodwbRes$rciu7,nsmall=3),"]"),
                     metaEffect= paste0(format(modRes$r1,nsmall=3),
                                        " [",format(modRes$rcil1,nsmall=3),"; ",
                                        format(modRes$rciu1,nsmall=3),"]"),
                     q = paste0(format(wbmodwbRes$q), "(",wbmodwbRes$qdf,")",ifelse(wbmodwbRes$qp < .01, "*", "")),
                     qM = paste0(format(wbmodwbRes$qM), "(",wbmodwbRes$qMdf,")",ifelse(wbmodwbRes$qMp < .01, "*", ""))
)
flextable(Table6)

dPlot5 <- data.frame(r = c(wbmodwbRes$r1, wbmodwbRes$r2,wbmodwbRes$r3, wbmodwbRes$r4,
                           wbmodwbRes$r5, wbmodwbRes$r6,wbmodwbRes$r7,
                           modRes$r1),
                     rcil = c(wbmodwbRes$rcil1,wbmodwbRes$rcil2,wbmodwbRes$rcil3,wbmodwbRes$rcil4,
                              wbmodwbRes$rcil5,wbmodwbRes$rcil6,wbmodwbRes$rcil7,
                              modRes$rcil1),
                     rciu = c(wbmodwbRes$rciu1,wbmodwbRes$rciu2,wbmodwbRes$rciu3,wbmodwbRes$rciu4,
                              wbmodwbRes$rciu5,wbmodwbRes$rciu6,wbmodwbRes$rciu7,
                              modRes$rciu1),
                     factor = rep(c('Domain satisfaction','Happiness',
                                    "Life satisfaction", "Negative affect", "Positive affect",
                                    "Psychological well-being", "Subjective well-being",
                                    " Overall"), each = 24),
                     strength = wbmodwbRes$strength)

(Figure5 <- ggplot(dPlot5, aes(x = r, y = factor, color = factor)) +
    geom_point(shape = 18,
               size = 3) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_x_continuous(breaks = seq(0,.60,by=.20)) +
    labs(x = "Meta-analytical association", y = "Strength",
         color = "Moderator") +
    theme_bw(base_size = 12) + theme(legend.position="bottom") +
    facet_wrap(~strength,ncol = 4)+
    # Change y names
    # scale_y_discrete(labels = c('Overall','Domain satisfaction','Happiness',
    #                             "Life satisfaction", "Negative affect", "Positive affect",
    #                             "Psychological well-being", "Subjective well-being")) +
    # Add annotations
    geom_text(aes(label = format(r, nsmall = 2)), 
              x=min(dPlot5$rcil)-.04, vjust = .20, size = 2.5, color = "black", 
              show.legend = FALSE) +
    coord_cartesian(xlim = c(min(dPlot5$rcil)-.08,max(dPlot5$rciu)))
)

############################################## OUT-MH-ANALYSIS ######################################################
mhMeasures = c("anx","dep","gen","stress")
mhmodmhRes <- effList[["mhmh"]]
Table9 <- data.frame(strength = mhmodmhRes$strength,
                     anxiety = paste0(round(mhmodmhRes$r1,3),
                                      " [",round(mhmodmhRes$rcil1,3),"; ",
                                      round(mhmodmhRes$rciu1,3),"]"),
                     depression = paste0(round(mhmodmhRes$r2,3),
                                         " [",round(mhmodmhRes$rcil2,3),"; ",
                                         round(mhmodmhRes$rciu2,3),"]"),
                     general = paste0(round(mhmodmhRes$r3,3),
                                      " [",round(mhmodmhRes$rcil3,3),"; ",
                                      round(mhmodmhRes$rciu3,3),"]"),
                     stress = paste0(round(mhmodmhRes$r4,3),
                                     " [",round(mhmodmhRes$rcil4,3),"; ",
                                     round(mhmodmhRes$rciu4,3),"]"),
                     metaEffect= paste0(round(modRes$r0,3),
                                        " [",round(modRes$rcil0,3),"; ",
                                        round(modRes$rciu0,3),"]"),
                     q = paste0(format(mhmodmhRes$q), "(",mhmodmhRes$qdf,")",ifelse(mhmodmhRes$qp < .01, "*", "")),
                     qM = paste0(format(mhmodmhRes$qM), "(",mhmodmhRes$qMdf,")",ifelse(mhmodmhRes$qMp < .01, "*", ""))
)
flextable(Table9)

dPlot8 <- data.frame(r = c(mhmodmhRes$r1, mhmodmhRes$r2,mhmodmhRes$r3, mhmodmhRes$r4,
                           modRes$r0),
                     rcil = c(mhmodmhRes$rcil1,mhmodmhRes$rcil2,mhmodmhRes$rcil3,mhmodmhRes$rcil4,
                              modRes$rcil0),
                     rciu = c(mhmodmhRes$rciu1,mhmodmhRes$rciu2,mhmodmhRes$rciu3,mhmodmhRes$rciu4,
                              modRes$rciu0),
                     factor = rep(c("Anxiety", "Depression" ,
                                    "General mental health", "Stress",
                                    " Overall"), each = 24),
                     strength = mhmodmhRes$strength)

(Figure8 <- ggplot(dPlot8, aes(x = r, y = factor, color = factor)) +
    geom_point(shape = 18,
               size = 3) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_x_continuous(breaks = seq(0,.60,by=.20)) +
    labs(x = "Meta-analytical association", y = "Strength",
         color = "Moderator") +
    theme_bw(base_size = 12) +theme(legend.position="bottom") +
    facet_wrap(~strength,ncol = 4)+
    # Add annotations
    geom_text(aes(label = format(r, nsmall = 2)), 
              x=min(dPlot8$rcil)-.08, vjust = .20, size = 2.5, color = "black", 
              show.legend = FALSE) +
    coord_cartesian(xlim = c(min(dPlot8$rcil)-.08,max(dPlot8$rciu)))
)

############################################## POP-WB-ANALYSIS ######################################################
wbmodPopRes <- effList[["wbPop"]]
Table4 <- data.frame(strength = wbmodPopRes$strength, 
                     clinical = paste0(round(wbmodPopRes$r0,3),
                                       " [",round(wbmodPopRes$rcil0,3),"; ",
                                       round(wbmodPopRes$rciu0,3),"]"),
                     N0 = paste0(wbmodPopRes$N0, " k = (", wbmodPopRes$k0, ")"),
                     notClinical   = paste0(round(wbmodPopRes$r1,3),
                                            " [",round(wbmodPopRes$rcil1,3),"; ",
                                            round(wbmodPopRes$rciu1,3),"]"),
                     N1 = paste0(wbmodPopRes$N1, " k = (", wbmodPopRes$k1, ")"),
                     deltaR = format(wbmodPopRes$deltaR, nsmall = 3),
                     deltaCI = paste0("[",format(wbmodPopRes$cil,nsmall = 3),"; ",
                                      format(wbmodPopRes$ciu,nsmall = 3),"]"),
                     se = format(wbmodPopRes$se,nsmall = 3), 
                     z = format(wbmodPopRes$z,nsmall = 3),
                     p = format(wbmodPopRes$p,4),
                     tau2 = wbmodPopRes$tau2,
                     q = paste0(format(wbmodPopRes$q), "(",wbmodPopRes$qdf,")",ifelse(wbmodPopRes$qp < .01, "*", "")),
                     qM = paste0(format(wbmodPopRes$qM), "(",wbmodPopRes$qMdf,")",ifelse(wbmodPopRes$qMp < .01, "*", ""))
                     
)
flextable(Table4)

dPlot3 <- data.frame(r = c(wbmodPopRes$r0, wbmodPopRes$r1),
                     rcil = c(wbmodPopRes$rcil0, wbmodPopRes$rcil1),
                     rciu = c(wbmodPopRes$rciu0, wbmodPopRes$rciu1),
                     factor = rep(c("Clinical","Not clinical"), each = 24),
                     strength = wbmodPopRes$strength)

(Figure3 <- ggplot(dPlot3, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = 3) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(-.10, .10), linetype = "dashed") +
    scale_color_manual(values=c("#999999", "black")) +
    scale_x_continuous(breaks = seq(0,.60,by=.10)) +
    labs(x = "Meta-analytical association", y = "Strength",
         color = "Moderator") +
    theme_bw(base_size = 18) +
    annotate("text", x = max(dPlot3$rciu)+.01, y = 24:1,
             label = paste0(Table4$deltaR," ",Table4$deltaCI), hjust = "left") +
    coord_cartesian(xlim = c(min(dPlot3$rcil),max(dPlot3$rciu)+.12))
)

############################################## POP-MH-ANALYSIS ######################################################
mhmodPopRes = effList[["mhPop"]]
Table7 <- data.frame(strength = mhmodPopRes$strength,
                     mentaHealth = paste0(round(mhmodPopRes$r0,3),
                                          " [",round(mhmodPopRes$rcil0,3),"; ",
                                          round(mhmodPopRes$rciu0,3),"]"),
                     N0 = paste0(mhmodPopRes$N0, " k = (", mhmodPopRes$k0, ")"),
                     wellBeing   = paste0(round(mhmodPopRes$r1,3),
                                          " [",round(mhmodPopRes$rcil1,3),"; ",
                                          round(mhmodPopRes$rciu1,3),"]"),
                     N1 = paste0(mhmodPopRes$N1, " k = (", mhmodPopRes$k1, ")"),
                     deltaR = format(mhmodPopRes$deltaR, nsmall = 3),
                     deltaCI = paste0("[",format(mhmodPopRes$cil,nsmall = 3),"; ",
                                      format(mhmodPopRes$ciu,nsmall = 3),"]"),
                     se = format(mhmodPopRes$se,nsmall = 3), 
                     z = format(mhmodPopRes$z,nsmall = 3),
                     p = format(mhmodPopRes$p,4),
                     tau2 = mhmodPopRes$tau2,
                     q = paste0(format(mhmodPopRes$q), "(",mhmodPopRes$qdf,")",ifelse(mhmodPopRes$qp < .01, "*", "")),
                     qM = paste0(format(mhmodPopRes$qM), "(",mhmodPopRes$qMdf,")",ifelse(mhmodPopRes$qMp < .01, "*", ""))
                     
)
flextable(Table7)

dPlot6 <- data.frame(r = c(mhmodPopRes$r0, mhmodPopRes$r1),
                     rcil = c(mhmodPopRes$rcil0, mhmodPopRes$rcil1),
                     rciu = c(mhmodPopRes$rciu0, mhmodPopRes$rciu1),
                     factor = rep(c("Clinical","Not clinical"), each = 24),
                     strength = mhmodPopRes$strength)

(Figure6 <- ggplot(dPlot6, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = 3) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), width = 0.4, linewidth = .8) +
    geom_vline(xintercept = c(-.10, .10), linetype = "dashed") +
    scale_color_manual(values=c("#999999", "black")) +
    scale_x_continuous(breaks = seq(0,.60,by=.10)) +
    labs(x = "Meta-analytical association", y = "Strength",
         color = "Moderator") +
    theme_bw(base_size = 18) +
    annotate("text", x = max(dPlot6$rciu)-.02, y = 24:1,
             label = paste0(Table7$deltaR," ",Table7$deltaCI), hjust = "left") +
    coord_cartesian(xlim = c(min(dPlot6$rcil),max(dPlot6$rciu)+.12))
)

