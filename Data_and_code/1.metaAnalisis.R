# -----------------------------------------------------------------------------------------------------------------#
############################################## DESCRIPTIVES ########################################################
# -----------------------------------------------------------------------------------------------------------------#

# ------ Libraries and utilities ------- #
library(readxl)
library(metafor)
library(ggplot2)
library(dplyr)
library(pacman)
library(flextable)
library(reshape2)

folder_path <- "R"  # Adjust this if the folder is located elsewhere
r_files <- list.files(folder_path, full.names = TRUE, pattern = "\\.R$")
for (r_file in r_files) {
  source(r_file)  # This will run the R code in the specified script file
}

# ------          Data          ------- #

# Clear environment
dtot = read_excel(here::here("Data_and_code/metaData/finalMetaData.xlsx"))

strengths24 <- colnames(dtot[, which(colnames(dtot) == "Appreciation_of_beauty"):
                               which(colnames(dtot) == "Zest")])
#Reverse negative scales
# dtot[dtot$Valence == "neg", strengths24] <- dtot[dtot$Valence == "neg", strengths24] * -1

# Create dataset with non-duplicated samples for descriptive stats
uniqueD <- dtot[!duplicated(dtot$sample), ]

# ------      SUMMARY STATS      ------- #
# Create a dataframe with one line per strength and one column per information
summStat <- data.frame(strength = strengths24,
                       N = rep(NA,24),
                       sampleSize = rep(NA,24),
                       k = rep(NA,24),
                       samples = rep(NA, 24),
                       studies = rep(NA, 24),
                       # Specific CS measure,
                       CS_short = rep(NA,24),
                       CS_long = rep(NA,24),
                       CS_other = rep(NA,24),
                       # Outcomes
                       Wellbeing = rep(NA,24),
                       MentalHealt = rep(NA,24),
                       # Clinical
                       Clinical = rep(NA, 24)
                       )

# Calculate each info for each skill by subsetting the data
for (i in 1:length(strengths24)) {
  dsum <- dtot[is.na(dtot[,strengths24[i]]) == F, ]
  summStat[i, 2:ncol(summStat)] <- c(
                                     sum(dsum$N[!duplicated(dsum$ID)], na.rm = TRUE), # N
                                     paste0("[", min(dsum$N,na.rm=TRUE), "; ", max(dsum$N,na.rm=TRUE),"]"), # N range
                                     nrow(dsum), # Effects (k)
                                     length(unique(dsum$sample)), # Samples
                                     length(unique(dsum$ID)), # Studies
                                     # Moderators
                                     paste0(sum(dsum$CS_measure=="short",na.rm=TRUE), 
                                            " (",length(unique(dsum$sample[dsum$CS_measure == "short"])),")"), # Short VIA
                                     paste0(sum(dsum$CS_measure=="long",na.rm=TRUE), 
                                     " (",length(unique(dsum$sample[dsum$CS_measure == "long"])),")"), # Long VIA
                                     paste0(sum(dsum$CS_measure=="other",na.rm=TRUE), 
                                            " (",length(unique(dsum$sample[dsum$CS_measure == "other"])),")"), # Other CS measures
                                     paste0(sum(dsum$Outcome=="wb",na.rm=TRUE), 
                                            " (",length(unique(dsum$sample[dsum$Outcome == "wb"])),")"), # WB outcomes
                                     paste0(sum(dsum$Outcome=="mh",na.rm=TRUE), 
                                            " (",length(unique(dsum$sample[dsum$Outcome == "mh"])),")"), # MH outcomes
                                     paste0(sum(dsum$Population=="clinical",na.rm=TRUE), 
                                            " (",length(unique(dsum$sample[dsum$Population == "clinical"])),")") # Clinical populations
                                     )
}

# Stats for the total sample
# Calculate each info for the total sample
totStat <- c("FullSample",
             sum(dtot$N[!duplicated(dtot$ID)], na.rm = TRUE),
             paste0("[", min(dtot$N,na.rm=TRUE), "; ", max(dtot$N,na.rm=TRUE),"]"),
             sum(is.na(dtot[,strengths24])==FALSE),
             length(unique(dtot$sample)),
             length(unique(dtot$ID)),
             paste0(sum(is.na(dtot[dtot$CS_measure == "short",strengths24])==FALSE), 
                    " (",length(unique(dtot$sample[dtot$CS_measure == "short"])),")"),
             paste0(sum(is.na(dtot[dtot$CS_measure == "long",strengths24])==FALSE), 
                    " (",length(unique(dtot$sample[dtot$CS_measure == "long"])),")"),
             paste0(sum(is.na(dtot[dtot$CS_measure == "other",strengths24])==FALSE), 
                    " (",length(unique(dtot$sample[dtot$CS_measure == "other"])),")"),
             paste0(sum(is.na(dtot[dtot$Outcome == "wb",strengths24])==FALSE), 
                    " (",length(unique(dtot$sample[dtot$Outcome == "wb"])),")"),
             paste0(sum(is.na(dtot[dtot$Outcome == "mh",strengths24])==FALSE), 
                    " (",length(unique(dtot$sample[dtot$Outcome == "mh"])),")"),
             paste0(sum(is.na(dtot[dtot$Population == "clinical",strengths24])==FALSE), 
                    " (",length(unique(dtot$sample[dtot$Population == "clinical"])),")")
)
rbind(summStat, totStat)

# ------- SAMPLE SIZE ------- #
range(uniqueD$N)
median(uniqueD$N)
mean(uniqueD$N)
sd(uniqueD$N)

# ------- COUNTRY ------- #
table(uniqueD$Country)

# ------- GENDER ------- #
range(as.numeric(uniqueD$`Gender%`),na.rm = TRUE)
median(as.numeric(uniqueD$`Gender%`),na.rm = TRUE)

# ------- AGE ------- #
sum(is.na(dtot$Age_classification)) # This should be 0!
range(as.numeric(uniqueD$Mean_age), na.rm = TRUE)
median(as.numeric(uniqueD$Mean_age),na.rm = TRUE)
       
# Age classification
table(uniqueD$Age_classification)
table(uniqueD$Age_classification)/nrow(uniqueD)

# ------- POPULATION ------- #
# Clinical or not
table(uniqueD$Population)
table(uniqueD$Population_type)

# ------- VIA MEASURE ------- #
sum(is.na(dtot$CS_measure_type)) # Some were not categorized
table(uniqueD$CS_measure)
table(uniqueD$CS_measure_type)

# ------- OUTCOMES ------- #
table(uniqueD$Outcome)
table(dtot$Outcome_type)
sum(is.na(dtot$Outcome_type)) # Some were not categorized

# ------- DESIGN ------- #
sum(is.na(dtot$Study_design)) # This should be 0!

# ------- EFFECTS ------- #
apply(dtot[,strengths24], 2, range, na.rm=T)

# -----------------------------------------------------------------------------------------------------------------#
############################################## META-ANALYSIS #######################################################
# -----------------------------------------------------------------------------------------------------------------#
# Make a long dataframe for metaanalysis (dm)
fixvar <- colnames(dtot[, !colnames(dtot)%in%strengths24])
dm <- melt(dtot, id.vars = fixvar, 
           variable.name = "strength",
           value.name = "ri")
dm <- dm[is.na(dm$ri)==FALSE, ]
range(dm$ri)
dm <- escalc(measure = "ZCOR", ri = ri, ni = N, data = dm)

range(dm$vi)
range(dm$yi)

# Correct for unreliability
brunaAlpha <- data.frame(strenth = strengths24,
                         alphas = c(.80,.75,.86,.80,
                                     .78,.81,.80,.76,
                                     .81,.75,.85,.81,
                                     .75,.77,.81,.75,
                                     .84,.78,.74,.71,
                                     .76,.84,.76,.78))
dm$cs_rel <- brunaAlpha$alphas[dm$strength]
dm$out_rel <- ifelse(is.na(dm$out_rel)==TRUE, 1, dm$out_rel)
dm$vi <- dm$vi / (dm$out_rel * dm$cs_rel)
dm$yi <- dm$yi / sqrt(dm$out_rel * dm$cs_rel)

# -----------------------------------------------------------------------------------------------------------------#
############################################## HEALTY FUNCTIONING ##################################################
# -----------------------------------------------------------------------------------------------------------------#
# Loop for each strength
outList <- list()
metaCol <- c("b","cil","ciu","se","z","p",
             "r","rcil","rciu","rpil","rpiu",
             "tau2","i2",
             "k","N",
             "q","qdf","qp")
metaRes <- matrix(nrow = length(strengths24), ncol = length(metaCol))
colnames(metaRes) <- metaCol
rownames(metaRes) <- strengths24

for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F,]
  outList[[i]] <- rma.mv(yi = yi, V = vi, random = ~ 1 | ID / sample, test = "t", data = di)
  mres <- summary(outList[[i]])
  pred <- predict(mres, transf=transf.ztor)
  tau2 <- sum(mres$sigma2)
  i2 <- i2fun(mres)
  metaRes[i,] <- c(mres$beta,mres$ci.lb,mres$ci.ub,mres$se,mres$zval,mres$pval,
                   pred$pred,pred$ci.lb,pred$ci.ub,pred$pi.lb,pred$pi.ub,
                   tau2, i2,
                   nrow(di),sum(di$N[di$sample %in% unique(di$sample)]),
                   mres$QE,mres$QEdf,mres$QEp
                   )
}
rm(mres) # Remove last loop iterations from environment
rm(pred)

round(metaRes,2)

metaRes <- data.frame(metaRes)
metaRes$strength = gsub("_", " ", strengths24)

# -----------------------------------------------------------------------------------------------------------------#
############################################# WB-MH MODERATION #####################################################
# -----------------------------------------------------------------------------------------------------------------#
# Loop for each strength
outListMod <- list()
forestMod <- list()
modCol <- c("deltaR","cil","ciu","se","z","p",
            "r0","rcil0","rciu0","rpil0","rpiu0", "k0","N0",
            "r1","rcil1","rciu1","rpil1","rpiu1", "k1","N1",
            "tau2","i2",
            "q","qdf","qp",
            "qM","qMdf","qMp")
modRes <- matrix(nrow = length(strengths24), ncol = length(modCol))
colnames(modRes) <- modCol
rownames(modRes) <- strengths24

for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F,]
  outListMod[[i]] <- rma.mv(yi = yi, V = vi, mods = ~ Outcome, random = ~ 1 | ID / sample, test = "t", data = di)
  modres <- summary(outListMod[[i]])
  modpred <- predict(modres, transf=transf.ztor, newmods = c(0,1))
  tau2 <- sum(modres$sigma2)
  i2 <- i2fun(modres)
  modRes[i,] <- c(transf.ztor(modres$beta[2]),transf.ztor(modres$ci.lb[2]),transf.ztor(modres$ci.ub[2]),
                  modres$se[2],modres$zval[2],modres$pval[2],
                  modpred$pred[1],modpred$ci.lb[1],modpred$ci.ub[1],modpred$pi.lb[1],modpred$pi.ub[1],
                  sum(di$Outcome=="mh",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$Outcome=="mh"]),
                  modpred$pred[2],modpred$ci.lb[2],modpred$ci.ub[2],modpred$pi.lb[2],modpred$pi.ub[2],
                  sum(di$Outcome=="wb",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$Outcome=="wb"]),
                  tau2, i2,
                  modres$QE,modres$QEdf,modres$QEp,
                  modres$QM,modres$QMdf[1],modres$QMp
                  )
}
rm(modres)
rm(modpred)

round(modRes, 2)

modRes <- data.frame(round(modRes,3))
modRes$strength = gsub("_", " ", strengths24)

# -----------------------------------------------------------------------------------------------------------------#
############################################## WELL-BEING META #####################################################
# -----------------------------------------------------------------------------------------------------------------#
# -------------- Population moderators
# Loop for each strength
wbModPop <- list()
wbModPopcol <- c("deltaR","cil","ciu","se","z","p",
            "r0","rcil0","rciu0","rpil0","rpiu0", "k0","N0",
            "r1","rcil1","rciu1","rpil1","rpiu1", "k1","N1",
            "tau2","i2",
            "q","qdf","qp",
            "qM","qMdf","qMp")
wbmodPopRes <- matrix(nrow = length(strengths24), ncol = length(wbModPopcol))
colnames(wbmodPopRes) <- wbModPopcol
rownames(wbmodPopRes) <- strengths24
for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F & dm$Outcome == "wb",]
  wbModPop[[i]] <- rma.mv(yi = yi, V = vi, mods = ~ Population, random = ~ 1 | ID / sample, test = "t", data = di)
  modres <- summary(wbModPop[[i]])
  modpred <- predict(modres, transf=transf.ztor, newmods = c(0,1))
  tau2 <- sum(modres$sigma2)
  i2 <- i2fun(modres)
  wbmodPopRes[i,] <- c(transf.ztor(modres$beta[2]),transf.ztor(modres$ci.lb[2]),transf.ztor(modres$ci.ub[2]),
                       modres$se[2],modres$zval[2],modres$pval[2],
                       modpred$pred[1],modpred$ci.lb[1],modpred$ci.ub[1],modpred$pi.lb[1],modpred$pi.ub[1],
                       sum(di$Population=="clinical",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$Population=="clinical"]),
                       modpred$pred[2],modpred$ci.lb[2],modpred$ci.ub[2],modpred$pi.lb[2],modpred$pi.ub[2],
                       sum(di$Population=="not_clinical",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$Population=="not_clinical"]),
                       tau2, i2,
                       modres$QE,modres$QEdf,modres$QEp,
                       modres$QM,modres$QMdf[1],modres$QMp
  )
}
rm(modres)
rm(modpred)

round(wbmodPopRes,2)

wbmodPopRes <- data.frame(round(wbmodPopRes,3))
wbmodPopRes$strength = gsub("_", " ", strengths24)

######## VIA measure effect
# Loop for each strength
wbModVia <- list()
wbModViacol <- c("deltaR","cil","ciu","se","z","p",
                 "r0","rcil0","rciu0","rpil0","rpiu0", "k0","N0",
                 "r1","rcil1","rciu1","rpil1","rpiu1", "k1","N1",
                 "tau2","i2",
                 "q","qdf","qp",
                 "qM","qMdf","qMp")
wbmodViaRes <- matrix(nrow = length(strengths24), ncol = length(wbModPopcol))
colnames(wbmodViaRes) <- wbModViacol
rownames(wbmodViaRes) <- strengths24

for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F & dm$Outcome == "wb" & dm$CS_measure %in% c("long", "short"),]
  wbModVia[[i]] <- rma.mv(yi = yi, V = vi, mods = ~ CS_measure, random = ~ 1 | ID / sample, test = "t", data = di)
  modres <- summary(wbModVia[[i]])
  modpred <- predict(modres, transf=transf.ztor, newmods = c(0,1))
  tau2 <- sum(modres$sigma2)
  i2 <- i2fun(modres)
  wbmodViaRes[i,] <- c(transf.ztor(modres$beta[2]),transf.ztor(modres$ci.lb[2]),transf.ztor(modres$ci.ub[2]),
                       modres$se[2],modres$zval[2],modres$pval[2],
                       modpred$pred[1],modpred$ci.lb[1],modpred$ci.ub[1],modpred$pi.lb[1],modpred$pi.ub[1],
                       sum(di$CS_measure=="long",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$CS_measure=="long"]),
                       modpred$pred[2],modpred$ci.lb[2],modpred$ci.ub[2],modpred$pi.lb[2],modpred$pi.ub[2],
                       sum(di$CS_measure=="short",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$CS_measure=="short"]),
                       tau2, i2,
                       modres$QE,modres$QEdf,modres$QEp,
                       modres$QM,modres$QMdf[1],modres$QMp
                       )
 
}
rm(modres);rm(modpred)
round(wbmodViaRes,2)

wbmodViaRes <- data.frame(round(wbmodViaRes,3))
wbmodViaRes$strength = gsub("_", " ", strengths24)

######## SPECIFIC OUTCOMES
dm$spec_out <- ifelse(dm$Outcome_type == "meaning", "pwb", 
                      ifelse(dm$Outcome_type == "distress", "gen", 
                             ifelse(dm$Outcome_type == "ptsd", NA,
                                    dm$Outcome_type)))
wbMeasures = c("domsat","happy","lifesat","negaff","posaff","pwb","swb")
# Loop for each strength
wbModwb <- list()
wbModwbCol <- c("r1","rcil1","rciu1","rpil1","rpiu1", "k1","N1",
                "r2","rcil2","rciu2","rpil2","rpiu2", "k2","N2",
                "r3","rcil3","rciu3","rpil3","rpiu3", "k3","N3",
                "r4","rcil4","rciu4","rpil4","rpiu4", "k4","N4",
                "r5","rcil5","rciu5","rpil5","rpiu5", "k5","N5",
                "r6","rcil6","rciu6","rpil6","rpiu6", "k6","N6",
                "r7","rcil7","rciu7","rpil7","rpiu7", "k7","N7",
                 "tau2","i2",
                "q","qdf","qp",
                "qM","qMdf","qMp")
wbmodwbRes <- as.data.frame(matrix(nrow = length(strengths24), ncol = length(wbModwbCol)))
colnames(wbmodwbRes) <- wbModwbCol
rownames(wbmodwbRes) <- strengths24
m<-matrix(c(0,1,0,0,0,0,0,
            0,0,1,0,0,0,0,
            0,0,0,1,0,0,0,
            0,0,0,0,1,0,0,
            0,0,0,0,0,1,0,
            0,0,0,0,0,0,1),nrow = 7) # Create a matrix to extract moderator effects

for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F & dm$Outcome == "wb" & dm$spec_out %in% wbMeasures,]
  wbModwb[[i]] <- rma.mv(yi = yi, V = vi, mods = ~ 1+spec_out, random = ~ 1 | ID / sample, test = "t", data = di)
  modres <- summary(wbModwb[[i]])
  modpred <- as.data.frame(predict(modres, transf=transf.ztor, newmods = m))
  tau2 <- sum(modres$sigma2)
  i2 <- i2fun(modres)
  wbmodwbRes[i,] <- c(unlist(modpred[1,]),sum(di$spec_out=="domsat",na.rm = T),
                      sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="domsat"],na.rm = T),
                        unlist(modpred[2,]),sum(di$spec_out=="happy",na.rm = T),
                        sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="happy"],na.rm = T),
                             unlist(modpred[3,]),sum(di$spec_out=="lifesat",na.rm = T),
                             sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="lifesat"],na.rm = T),
                                    unlist(modpred[4,]),sum(di$spec_out=="negaff",na.rm = T),
                                    sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="negaff"],na.rm = T),
                                           unlist(modpred[5,]),sum(di$spec_out=="posaff",na.rm = T),
                                           sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="posaff"],na.rm = T),
                                                  unlist(modpred[6,]),sum(di$spec_out=="pwb",na.rm = T),
                                                  sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="pwb"],na.rm = T),
                                                         unlist(modpred[7,]),sum(di$spec_out=="swb",na.rm = T),
                                                         sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="swb"],na.rm = T),
                      tau2, i2,
                      modres$QE,modres$QEdf,modres$QEp,
                      modres$QM,modres$QMdf[1],modres$QMp
  )
}
rm(modres);rm(modpred)
wbmodwbRes <- data.frame(round(wbmodwbRes,3))
wbmodwbRes$strength = gsub("_", " ", strengths24)

# -----------------------------------------------------------------------------------------------------------------#
############################################## MENTAL HEALTH META ##################################################
# -----------------------------------------------------------------------------------------------------------------#
# -------------- Population moderators
# Loop for each strength
mhModPop <- list()
mhModPopcol <- c("deltaR","cil","ciu","se","z","p",
                 "r0","rcil0","rciu0","rpil0","rpiu0", "k0","N0",
                 "r1","rcil1","rciu1","rpil1","rpiu1", "k1","N1",
                 "tau2","i2",
                 "q","qdf","qp",
                 "qM","qMdf","qMp")
mhmodPopRes <- matrix(nrow = length(strengths24), ncol = length(mhModPopcol))
colnames(mhmodPopRes) <- mhModPopcol
rownames(mhmodPopRes) <- strengths24
for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F & dm$Outcome == "mh",]
  if (i != which(strengths24=="Humor")) {
    mhModPop[[i]] <- rma.mv(yi = yi, V = vi, mods = ~ Population, random = ~ 1 | ID / sample, test = "t", data = di)
  } else {mhModPop[[i]] <- rma.mv(yi = yi, V = vi, mods = ~ Population, random = ~ 1 | ID / sample, test = "t", data = di,
                                  control=list(optimizer="Nelder-Mead"))}
  modres <- summary(mhModPop[[i]])
  modpred <- predict(modres, transf=transf.ztor, newmods = c(0,1))
  tau2 <- sum(modres$sigma2)
  i2 <- i2fun(modres)
  mhmodPopRes[i,] <- c(transf.ztor(modres$beta[2]),transf.ztor(modres$ci.lb[2]),transf.ztor(modres$ci.ub[2]),
                       modres$se[2],modres$zval[2],modres$pval[2],
                       modpred$pred[1],modpred$ci.lb[1],modpred$ci.ub[1],modpred$pi.lb[1],modpred$pi.ub[1],
                       sum(di$Population=="clinical",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$Population=="clinical"]),
                       modpred$pred[2],modpred$ci.lb[2],modpred$ci.ub[2],modpred$pi.lb[2],modpred$pi.ub[2],
                       sum(di$Population=="not_clinical",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$Population=="not_clinical"]),
                       tau2, i2,
                       modres$QE,modres$QEdf,modres$QEp,
                       modres$QM,modres$QMdf[1],modres$QMp
  )
}
rm(modres); rm(modpred)
round(mhmodPopRes, 2)
mhmodPopRes <- data.frame(round(mhmodPopRes,3))
mhmodPopRes$strength = gsub("_", " ", strengths24)

######## VIA measure effect
# Loop for each strength
mhModVia <- list()
mhModViacol <- c("deltaR","cil","ciu","se","z","p",
                 "r0","rcil0","rciu0","rpil0","rpiu0", "k0","N0",
                 "r1","rcil1","rciu1","rpil1","rpiu1", "k1","N1",
                 "tau2","i2",
                 "q","qdf","qp",
                 "qM","qMdf","qMp")
mhmodViaRes <- matrix(nrow = length(strengths24), ncol = length(mhModPopcol))
colnames(mhmodViaRes) <- mhModViacol
rownames(mhmodViaRes) <- strengths24
for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F & dm$Outcome == "mh" & dm$CS_measure %in% c("long", "short"),]
  mhModVia[[i]] <- rma.mv(yi = yi, V = vi, mods = ~ CS_measure, random = ~ 1 | ID / sample, test = "t", data = di)
  modres <- summary(mhModVia[[i]])
  modpred <- predict(modres, transf=transf.ztor, newmods = c(0,1))
  tau2 <- sum(modres$sigma2)
  i2 <- i2fun(modres)
  mhmodViaRes[i,] <- c(transf.ztor(modres$beta[2]),transf.ztor(modres$ci.lb[2]),transf.ztor(modres$ci.ub[2]),
                       modres$se[2],modres$zval[2],modres$pval[2],
                       modpred$pred[1],modpred$ci.lb[1],modpred$ci.ub[1],modpred$pi.lb[1],modpred$pi.ub[1],
                       sum(di$CS_measure=="long",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$CS_measure=="long"]),
                       modpred$pred[2],modpred$ci.lb[2],modpred$ci.ub[2],modpred$pi.lb[2],modpred$pi.ub[2],
                       sum(di$CS_measure=="short",na.rm = T),sum(di$N[di$sample %in% unique(di$sample)&di$CS_measure=="short"]),
                       tau2, i2,
                       modres$QE,modres$QEdf,modres$QEp,
                       modres$QM,modres$QMdf[1],modres$QMp
  )
  
}
round(mhmodViaRes,2)
mhmodViaRes <- data.frame(round(mhmodViaRes,3))
mhmodViaRes$strength = gsub("_", " ", strengths24)

######## SPECIFIC OUTCOMES
mhMeasures = c("anx","dep","gen","stress")
# Loop for each strength
mhModmh <- list()
mhModmhCol <- c("r1","rcil1","rciu1","rpil1","rpiu1", "k1","N1",
                "r2","rcil2","rciu2","rpil2","rpiu2", "k2","N2",
                "r3","rcil3","rciu3","rpil3","rpiu3", "k3","N3",
                "r4","rcil4","rciu4","rpil4","rpiu4", "k4","N4",
                "tau2","i2",
                "q","qdf","qp",
                "qM","qMdf","qMp")
mhmodmhRes <- as.data.frame(matrix(nrow = length(strengths24), ncol = length(mhModmhCol)))
colnames(mhmodmhRes) <- mhModmhCol
rownames(mhmodmhRes) <- strengths24
m<-matrix(c(0,1,0,0,
            0,0,1,0,
            0,0,0,1),nrow = 4)

for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F & dm$Outcome == "mh" & dm$spec_out %in% mhMeasures,]
  mhModmh[[i]] <- rma.mv(yi = yi, V = vi, mods = ~ spec_out, random = ~ 1 | ID / sample, test = "t", data = di)
  modres <- summary(mhModmh[[i]])
  modpred <- as.data.frame(predict(modres, transf=transf.ztor, newmods = m))
  tau2 <- sum(modres$sigma2)
  i2 <- i2fun(modres)
  mhmodmhRes[i,] <- c(unlist(modpred[1,]),sum(di$spec_out=="anx",na.rm = T),
                      sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="anx"],na.rm = T),
                      unlist(modpred[2,]),sum(di$spec_out=="dep",na.rm = T),
                      sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="dep"],na.rm = T),
                      unlist(modpred[3,]),sum(di$spec_out=="gen",na.rm = T),
                      sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="gen"],na.rm = T),
                      unlist(modpred[4,]),sum(di$spec_out=="stress",na.rm = T),
                      sum(di$N[di$sample %in% unique(di$sample)&di$spec_out=="stress"],na.rm = T),
                      tau2, i2,
                      modres$QE,modres$QEdf,modres$QEp,
                      modres$QM,modres$QMdf[1],modres$QMp
  )
}

round(mhmodmhRes,2)
mhmodmhRes <- data.frame(round(mhmodmhRes,3))
mhmodmhRes$strength = gsub("_", " ", strengths24)

# -----------------------------------------------------------------------------------------------------------------#
################################################# LEAVE ONE OUT ####################################################
# -----------------------------------------------------------------------------------------------------------------#

# Well-being
l1o_res <- list()

for (strng in 1:24) {
  print(strng)
  
  di <- dm[dm$strength == strengths24[strng] & is.na(dm$ri) == F & dm$Outcome == "wb",]
  model <- rma.mv(yi = yi, V = vi, random = ~ 1 | ID / sample, test = "t", data = di)
  
  l1o_res[[strengths24[strng]]] <- data.frame(
    sample = di$sample,
    beta = rep(NA, nrow(di)),
    cil = transf.ztor(summary(model)$ci.lb),
    ciu = transf.ztor(summary(model)$ci.ub),
    target_est = transf.ztor(summary(model)$beta)
  )

  for (i in 1:nrow(di)) {
  
    d_out <- di[-i,]
    out_fit <- rma.mv(yi = yi, V = vi, random = ~ 1 | ID / sample, test = "t", data = d_out)
    
    l1o_res[[ strengths24[strng] ]]$beta[i] <- transf.ztor(summary(out_fit)$beta)
    
  }
  
}

                          
# Mental health
l1o_res_mh <- list()

for (strng in 1:24) {
  print(strng)
  
  di <- dm[dm$strength == strengths24[strng] & is.na(dm$ri) == F & dm$Outcome == "wb",]
  model <- rma.mv(yi = yi, V = vi, random = ~ 1 | ID / sample, test = "t", data = di)
  
  l1o_res_mh[[strengths24[strng]]] <- data.frame(
    sample = di$sample,
    beta = rep(NA, nrow(di)),
    cil = transf.ztor(summary(model)$ci.lb),
    ciu = transf.ztor(summary(model)$ci.ub),
    target_est = transf.ztor(summary(model)$beta)
  )
  
  for (i in 1:nrow(di)) {
    
    d_out <- di[-i,]
    out_fit <- rma.mv(yi = yi, V = vi, random = ~ 1 | ID / sample, test = "t", data = d_out)
    
    l1o_res_mh[[ strengths24[strng] ]]$beta[i] <- transf.ztor(summary(out_fit)$beta)
    
  }
  
}

# -----------------------------------------------------------------------------------------------------------------#
############################################### PUBLICATION BIAS ###################################################
# -----------------------------------------------------------------------------------------------------------------#
# Add standard error and its square to the data
dm$sei <- sqrt(dm$vi)
dm$sei2 <- dm$sei^2

# Use PET regressions with sei to test for publication bias in wellbeing and mental health associations
# ------- Wellbeing
wbBias <- list()
wbBiasTest <- c(rep(NA, length(strengths24)))

for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F & dm$Outcome == "wb",]
  wbBias[[i]] <- rma.mv(yi = yi,
                        V = vi,
                        mods = ~ sei,
                        random = ~ 1 | ID/sample,
                        test = "t",
                        data = di)
  wbBiasTest[i] <- wbBias[[i]]$pval[2]
}

# ------- Mental health
mhBias <- list()
mhBiasTest <- c(rep(NA, length(strengths24)))

for (i in 1:length(strengths24)) {
  di <- dm[dm$strength == strengths24[i] & is.na(dm$ri) == F & dm$Outcome == "mh",]
  mhBias[[i]] <- rma.mv(yi = yi,
                        V = vi,
                        mods = ~ sei,
                        random = ~ 1 | ID/sample,
                        test = "t",
                        data = di)
  mhBiasTest[i] <- mhBias[[i]]$pval[2]
}

# -----------------------------------------------------------------------------------------------------------------#
############################################## STORE RESULTS META ##################################################
# -----------------------------------------------------------------------------------------------------------------#
resList = list(
  overall = outList,
  mainMod = outListMod,
  mhPop = mhModPop,
  mhVia = mhModVia,
  wbPop = wbModPop,
  wbVia = wbModVia,
  wbwb = wbModwb,
  mhmh = mhModmh
)
saveRDS(resList, here::here("results/resList.RDS"))

effList = list(
  overall = metaRes,
  mainMod = modRes,
  mhPop = mhmodPopRes,
  mhVia = mhmodViaRes,
  wbPop = wbmodPopRes,
  wbVia = wbmodViaRes,
  wbwb = wbmodwbRes,
  mhmh = mhmodmhRes
)
saveRDS(effList, "results/effList.RDS")

resBias = list(
  wbl1o = l1o_res,
  mhl1o = l1o_res_mh,
  biaswb = wbBias,
  biasmh = mhBias,
  biaswb = wbBiasTest,
  biasmh = mhBiasTest
)
saveRDS(resBias, "results/resBias.RDS")
