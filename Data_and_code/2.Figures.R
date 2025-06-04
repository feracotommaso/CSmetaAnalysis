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

load(here::here("results/strengths.R"))
resList <- readRDS(here::here("results/resList.RDS"))
effList <- readRDS(here::here("results/effList.RDS"))

dtot = read_excel(here::here("Data_and_code/metaData/finalMetaData.xlsx"))
strengths24 <- colnames(dtot[, which(colnames(dtot) == "Appreciation_of_beauty"):
                               which(colnames(dtot) == "Zest")])
dtot[dtot$Valence == "neg", strengths24] <- dtot[dtot$Valence == "neg", strengths24] * -1

############################################## PLOT-STANDARD  #######################################################
pointSize = 3.5
lineWidth = .9
errorWidth = .4
piWidth = .2
piError = 0
piCol = "#999999"
dodge = .9


############################################## 1. MAIN-ANALYSIS  ####################################################
metaRes <- effList[["overall"]]

(Figure3 <- ggplot(metaRes, aes(x = r, y = reorder(strength,24:1))) +
    geom_errorbar(aes(xmin = rpil, xmax = rpiu), 
                  width = piError, 
                  linewidth = piWidth, col = piCol) +
    geom_point(shape = 18, 
               color = "black", 
               size = pointSize) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), 
                  width = errorWidth, 
                  linewidth = lineWidth) +
    geom_vline(xintercept = c(.00), linetype = "dashed") +
    labs(y = element_blank()) +
    theme_bw(base_size = 18) +
    scale_x_continuous(breaks = seq(-.10,.50,by=.10),
                       name = expression("Back-transformed " * hat(r) * " values")) +
    annotate("text", x = ifelse(min(metaRes$rcil) > -.05, -.17,min(metaRes$rcil)),
             y = 24:1, 
             label = paste0(format(round(metaRes$r, 3),nsmall=3)," ",
                            "[",format(round(metaRes$rcil,3),nsmall=3),"; ",
                                format(round(metaRes$rciu,3),nsmall=3),"]"), hjust = "left") +
    coord_cartesian(xlim = c(ifelse(min(metaRes$rcil) > -.05, -.15,
                                    min(metaRes$rcil)),
                             max(metaRes$rciu))) +
    theme(panel.grid.major = element_blank())
)

############################################## 2. WB/MH-ANALYSIS  ###################################################
modRes = effList[["mainMod"]]

dPlot2 <- data.frame(r = c(modRes$r0, modRes$r1),
                     rcil = c(modRes$rcil0, modRes$rcil1),
                     rciu = c(modRes$rciu0, modRes$rciu1),
                     rpil = c(modRes$rpil0, modRes$rpil1),
                     rpiu = c(modRes$rpiu0, modRes$rpiu1),
                     factor = rep(c("Mental health","Well-being"), each = 24),
                     strength = modRes$strength)

(Figure4 <- ggplot(dPlot2, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_errorbar(aes(xmin = rpil, xmax = rpiu), width = piError, linewidth = piWidth,
                  position = position_dodge(dodge)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = 3.3,
               position = position_dodge(dodge)) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), width = errorWidth, linewidth = lineWidth,
                  position = position_dodge(dodge)) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_color_manual(values=c("#999999", "black")) +
    scale_x_continuous(breaks = seq(-.10,.60,by=.10),
                       name = expression("Back-transformed " * hat(r) * " values")) +
    labs(x = "Meta-analytical association", y = element_blank(),
         color = element_blank()) +
    theme_bw(base_size = 18) +
    annotate("text", x = -.18, y = 24:1,
             label = paste0(format(modRes$deltaR, nsmall = 3)," ",
                            "[",format(modRes$cil,nsmall = 3),"; ",
                            format(modRes$ciu,nsmall = 3),"]"), hjust = "left") +
    coord_cartesian(xlim = c(-.16,max(dPlot2$rciu))) +
    theme(panel.grid.major = element_blank(), legend.position = "bottom")
)

############################################## 3. VIA-WB-ANALYSIS ###################################################
wbmodViaRes <- effList[["wbVia"]]

dPlot4 <- data.frame(r = c(wbmodViaRes$r0, wbmodViaRes$r1),
                     rcil = c(wbmodViaRes$rcil0, wbmodViaRes$rcil1),
                     rciu = c(wbmodViaRes$rciu0, wbmodViaRes$rciu1),
                     rpil = c(wbmodViaRes$rpil0, wbmodViaRes$rpil1),
                     rpiu = c(wbmodViaRes$rpiu0, wbmodViaRes$rpiu1),
                     factor = rep(c("Long","Short"), each = 24),
                     strength = wbmodViaRes$strength)

(Figure5 <- ggplot(dPlot4, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_errorbar(aes(xmin = rpil, xmax = rpiu), 
                  width = piError, 
                  linewidth = piWidth,
                  position = position_dodge(dodge)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = pointSize,
               position = position_dodge(dodge)) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), 
                  width = errorWidth, 
                  linewidth = lineWidth,
                  position = position_dodge(dodge)) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_x_continuous(breaks = seq(-.10,.50,by=.10),
                       name = expression("Back-transformed " * hat(r) * " values")) +
    labs(x = "Meta-analytical association", y = element_blank(),
         color = element_blank()) +
    scale_color_manual(values=c("#999999", "black")) +
    theme_bw(base_size = 18) +
    annotate("text", x = -.19, y = 24:1,
             label = paste0(format(wbmodViaRes$deltaR, nsmall = 3)," ",
                            "[",format(wbmodViaRes$cil,nsmall = 3),"; ",
                            format(wbmodViaRes$ciu,nsmall = 3),"]"), hjust = "left") +
    coord_cartesian(xlim = c(-.17,max(dPlot4$rciu))) +
    theme(panel.grid.major = element_blank(), legend.position = "bottom")
)


############################################## 4. VIA-MH-ANALYSIS ###################################################
mhmodViaRes <- effList[["mhVia"]]

dPlot7 <- data.frame(r = c(mhmodViaRes$r0, mhmodViaRes$r1),
                     rcil = c(mhmodViaRes$rcil0, mhmodViaRes$rcil1),
                     rciu = c(mhmodViaRes$rciu0, mhmodViaRes$rciu1),
                     rpil = c(mhmodViaRes$rpil0, mhmodViaRes$rpil1),
                     rpiu = c(mhmodViaRes$rpiu0, mhmodViaRes$rpiu1),
                     factor = rep(c("Long","Short"), each = 24),
                     strength = mhmodViaRes$strength)

(Figure6 <- ggplot(dPlot7, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_errorbar(aes(xmin = rpil, xmax = rpiu), 
                  width = piError, 
                  linewidth = piWidth,
                  position = position_dodge(dodge)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = pointSize,
               position = position_dodge(dodge)) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), 
                  width = errorWidth, 
                  linewidth = lineWidth,
                  position = position_dodge(dodge)) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_color_manual(values=c("#999999", "black")) +
    scale_x_continuous(breaks = seq(-.10,.60,by=.10),
                       name = expression("Back-transformed " * hat(r) * " values")) +
    labs(x = "Meta-analytical association", y = element_blank(),
         color = element_blank()) +
    theme_bw(base_size = 18) +
    annotate("text", x = -.24, y = 24:1,
             label = paste0(format(mhmodViaRes$deltaR, nsmall = 3)," ",
                            "[",format(mhmodViaRes$cil,nsmall = 3),"; ",
                            format(mhmodViaRes$ciu,nsmall = 3),"]"), hjust = "left") +
    coord_cartesian(xlim = c(-.21,max(dPlot7$rciu))) +
    theme(panel.grid.major = element_blank(), legend.position = "bottom")
)

############################################## 5. OUT-WB-ANALYSIS ###################################################
wbMeasures = c("domsat","happy","lifesat","negaff","posaff","pwb","swb")
wbmodwbRes <- effList[["wbwb"]]

dPlot5 <- data.frame(r = c(wbmodwbRes$r1, wbmodwbRes$r2,wbmodwbRes$r3, wbmodwbRes$r4,
                           wbmodwbRes$r5, wbmodwbRes$r6,wbmodwbRes$r7,
                           modRes$r1),
                     rcil = c(wbmodwbRes$rcil1,wbmodwbRes$rcil2,wbmodwbRes$rcil3,wbmodwbRes$rcil4,
                              wbmodwbRes$rcil5,wbmodwbRes$rcil6,wbmodwbRes$rcil7,
                              modRes$rcil1),
                     rciu = c(wbmodwbRes$rciu1,wbmodwbRes$rciu2,wbmodwbRes$rciu3,wbmodwbRes$rciu4,
                              wbmodwbRes$rciu5,wbmodwbRes$rciu6,wbmodwbRes$rciu7,
                              modRes$rciu1),
                     rpil = c(wbmodwbRes$rpil1,wbmodwbRes$rpil2,wbmodwbRes$rpil3,wbmodwbRes$rpil4,
                              wbmodwbRes$rpil5,wbmodwbRes$rpil6,wbmodwbRes$rpil7,
                              modRes$rpil1),
                     rpiu = c(wbmodwbRes$rpiu1,wbmodwbRes$rpiu2,wbmodwbRes$rpiu3,wbmodwbRes$rpiu4,
                              wbmodwbRes$rpiu5,wbmodwbRes$rpiu6,wbmodwbRes$rpiu7,
                              modRes$rpiu1),
                     factor = rep(c('Domain satisfaction','Happiness',
                                    "Life satisfaction", "Negative affect", "Positive affect",
                                    "Psychological well-being", "Subjective well-being",
                                    " Overall"), each = 24),
                     strength = wbmodwbRes$strength)

(Figure7 <- ggplot(dPlot5, aes(x = r, y = factor, color = factor)) +
    geom_errorbar(aes(xmin = rpil, xmax = rpiu), 
                  width = piError, 
                  linewidth = piWidth) +
    geom_point(shape = 18,
               size = pointSize) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), 
                  width = errorWidth, linewidth = lineWidth) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_x_continuous(breaks = seq(0,.60,by=.20),
                       name = expression("Back-transformed " * hat(r) * " values")) +
    labs(x = "Meta-analytical association", y = element_blank(),
         color = element_blank()) +
    theme_bw(base_size = 12) + theme(legend.position="bottom") +
    facet_wrap(~strength,ncol = 4)+
    # Add annotations
    geom_text(aes(label = format(r, nsmall = 2)), 
              x=min(dPlot5$rcil)-.04, vjust = .20, size = 2.5, color = "black", 
              show.legend = FALSE) +
    coord_cartesian(xlim = c(min(dPlot5$rcil)-.08,max(dPlot5$rciu))) +
    theme(panel.grid.major = element_blank())
)

############################################## 6. OUT-MH-ANALYSIS ###################################################
mhMeasures = c("anx","dep","gen","stress")
mhmodmhRes <- effList[["mhmh"]]

dPlot8 <- data.frame(r = c(mhmodmhRes$r1, mhmodmhRes$r2,mhmodmhRes$r3, mhmodmhRes$r4,
                           modRes$r0),
                     rcil = c(mhmodmhRes$rcil1,mhmodmhRes$rcil2,mhmodmhRes$rcil3,mhmodmhRes$rcil4,
                              modRes$rcil0),
                     rciu = c(mhmodmhRes$rciu1,mhmodmhRes$rciu2,mhmodmhRes$rciu3,mhmodmhRes$rciu4,
                              modRes$rciu0),
                     rpil = c(mhmodmhRes$rpil1,mhmodmhRes$rpil2,mhmodmhRes$rpil3,mhmodmhRes$rpil4,
                              modRes$rpil0),
                     rpiu = c(mhmodmhRes$rpiu1,mhmodmhRes$rpiu2,mhmodmhRes$rpiu3,mhmodmhRes$rpiu4,
                              modRes$rpiu0),
                     factor = rep(c("Anxiety", "Depression" ,
                                    "General mental health", "Stress",
                                    " Overall"), each = 24),
                     strength = mhmodmhRes$strength)

(Figure8 <- ggplot(dPlot8, aes(x = r, y = factor, color = factor)) +
    geom_errorbar(aes(xmin = rpil, xmax = rpiu), 
                  width = piError, 
                  linewidth = piWidth) +
    geom_point(shape = 18,
               size = pointSize) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), 
                  width = errorWidth, linewidth = lineWidth) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_x_continuous(breaks = seq(0,.60,by=.20),
                       name = expression("Back-transformed " * hat(r) * " values")) +
    labs(x = "Meta-analytical association", y = element_blank(),
         color = element_blank()) +
    theme_bw(base_size = 12) + theme(legend.position="bottom") +
    facet_wrap(~strength,ncol = 4)+
    # Add annotations
    geom_text(aes(label = format(r, nsmall = 2)), 
              x=min(dPlot8$rcil)-.04, vjust = .20, size = 2.5, color = "black", 
              show.legend = FALSE) +
    coord_cartesian(xlim = c(min(dPlot8$rcil)-.08,max(dPlot8$rciu))) +
    theme(panel.grid.major = element_blank())
)

############################################## S1. POP-WB-ANALYSIS ######################################################
wbmodPopRes <- effList[["wbPop"]]

dPlot3 <- data.frame(r = c(wbmodPopRes$r0, wbmodPopRes$r1),
                     rcil = c(wbmodPopRes$rcil0, wbmodPopRes$rcil1),
                     rciu = c(wbmodPopRes$rciu0, wbmodPopRes$rciu1),
                     rpil = c(wbmodPopRes$rpil0, wbmodPopRes$rpil1),
                     rpiu = c(wbmodPopRes$rpiu0, wbmodPopRes$rpiu1),
                     factor = rep(c("Clinical","Not clinical"), each = 24),
                     strength = wbmodPopRes$strength)

(Figure3 <- ggplot(dPlot3, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_errorbar(aes(xmin = rpil, xmax = rpiu), 
                  width = piError, 
                  linewidth = piWidth,
                  position = position_dodge(dodge)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = pointSize,
               position = position_dodge(dodge)) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), 
                  width = errorWidth, 
                  linewidth = lineWidth,
                  position = position_dodge(dodge)) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_x_continuous(breaks = seq(-.10,.50,by=.10),
                       name = expression("Back-transformed " * hat(r) * " values")) +
    labs(x = "Meta-analytical association", y = element_blank(),
         color = element_blank()) +
    scale_color_manual(values=c("#999999", "black")) +
    theme_bw(base_size = 18) +
    annotate("text", x = max(dPlot3$rciu)+.01, y = 24:1,
             label = paste0(format(wbmodPopRes$deltaR, nsmall = 3)," ",
                            "[",format(wbmodPopRes$cil,nsmall = 3),"; ",
                            format(wbmodPopRes$ciu,nsmall = 3),"]"), hjust = "left") +
    coord_cartesian(xlim = c(min(dPlot3$rcil),max(dPlot3$rciu)+.12)) +
    theme(panel.grid.major = element_blank())
)

ggsave(plot = Figure3, filename = here::here("supplementary/Figures/WB.png"), 
       width = 14, height = 9, device='png', dpi=200)

############################################## S2. POP-MH-ANALYSIS ######################################################
mhmodPopRes = effList[["mhPop"]]

dPlot6 <- data.frame(r = c(mhmodPopRes$r0, mhmodPopRes$r1),
                     rcil = c(mhmodPopRes$rcil0, mhmodPopRes$rcil1),
                     rciu = c(mhmodPopRes$rciu0, mhmodPopRes$rciu1),
                     rpil = c(mhmodPopRes$rpil0, mhmodPopRes$rpil1),
                     rpiu = c(mhmodPopRes$rpiu0, mhmodPopRes$rpiu1),
                     factor = rep(c("Clinical","Not clinical"), each = 24),
                     strength = mhmodPopRes$strength)

(Figure6 <- ggplot(dPlot6, aes(x = r, y = reorder(strength,c(24:1,24:1)), color = factor)) +
    geom_errorbar(aes(xmin = rpil, xmax = rpiu), 
                  width = piError, 
                  linewidth = piWidth,
                  position = position_dodge(dodge)) +
    geom_point(shape = 18, 
               #color = "black", 
               size = pointSize,
               position = position_dodge(dodge)) +
    geom_errorbar(aes(xmin = rcil, xmax = rciu), 
                  width = errorWidth, 
                  linewidth = lineWidth,
                  position = position_dodge(dodge)) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_x_continuous(breaks = seq(-.10,.50,by=.10),
                       name = expression("Back-transformed " * hat(r) * " values")) +
    labs(x = "Meta-analytical association", y = element_blank(),
         color = element_blank()) +
    scale_color_manual(values=c("#999999", "black")) +
    theme_bw(base_size = 18) +
    annotate("text", x = max(dPlot6$rciu)-.02, y = 24:1,
             label = paste0(format(mhmodPopRes$deltaR, nsmall = 3)," ",
                            "[",format(mhmodPopRes$cil,nsmall = 3),"; ",
                            format(mhmodPopRes$ciu,nsmall = 3),"]"), hjust = "left") +
    coord_cartesian(xlim = c(min(dPlot6$rcil),max(dPlot6$rciu)+.12)) +
    theme(panel.grid.major = element_blank())
)

ggsave(plot = Figure6, filename = here::here("supplementary/Figures/MH.png"), 
       width = 14, height = 9, device='png', dpi=200)
