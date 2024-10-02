###########################################################################################################################################################
# Code to run 2 by 3 anova (independent variable: MDD/HC, Rew2-4) made by Emily Wei
# updated by Emily Wei, 20 Dec 2023 to incorporate ANOVA in different age groups. 

# This analysis includes 35 MDD participants and 44 HC participants.
# This analysis includes 39 OA participants and 40 MA participants.
###########################################################################################################################################################

#####2 by 3 ANOVA: MDD/AGE & Reward
rm(list=ls());  
library(abind)
library(knitr)
atlas.path <- "/data/MARCER/ATLASES/";  
out.dirname <- paste0("/scratch2/weie/COGED_CODE/Stats/");   # top-level output directory for this particular analysis
glm.path <- "/data/MARCER/participantData/AFNI_ANALYSIS/";   # top-level input directory
# group <- c("MDD", "HC")
group <- c("OA", "MA")
subcort.tbl <- read.csv(paste0(atlas.path, "subcorticalKey.csv"), stringsAsFactors=FALSE);  # subcortical_MARCER3p0.nii.gz parcel labels
# Set the knots for evaluation phase and decision phase
mean.eval.row <- 3:5
mean.dec.row <- 10:13

get.ANOVA.stat <- function(do.type) { # do.type <- "Schaefer2018_400x7_vol"; # do.type <- "subcortical_vol"
  if (do.type == "Schaefer2018_400x7_vol") { num.cols <- 400; num.rows <- 25; do.columns <- 1:400} 
  if (do.type == "subcortical_vol") { num.cols <- 19; num.rows <- 25; do.columns <- subcort.tbl$HCP.label} 
  for (g in 1:length(group)) {   #  g <-1
    sub.tbl <- read.table(paste0("/scratch2/weie/COGED_CODE/", group[g], "_subjectSet.txt"), stringsAsFactors=FALSE);
    sub.ids <- sort(sub.tbl$sub.id[which(sub.tbl[,paste0("COGED_RewardLoad")] == TRUE)]);
    if (g == 1) {
      # mdd.tbl <- array(0, c(3*length(sub.ids), 5, num.cols)); #trying to make this new data.frame to hold my data
      # colnames(mdd.tbl) <- c("sub.id","group","reward","evaluation","decision")
      # mdd.tbl[,2,] <- rep("MDD")
      # mdd.tbl[,1,] <- rep(sub.ids, each =3)
      OA.tbl <- array(0, c(3*length(sub.ids), 5, num.cols)); #trying to make this new data.frame to hold my data
      colnames(OA.tbl) <- c("sub.id","group","reward","evaluation","decision")
      OA.tbl[,2,] <- rep("OA")
      OA.tbl[,1,] <- rep(sub.ids, each =3)
      for (sid in 1:length(sub.ids)) {   # sid <- 1;
        rewload.lbls <- c(paste0("EV_Low_", 2:4), paste0("EV_Medium_", 2:4), paste0("EV_High_", 2:4));
        rewload.lst <- vector("list", length(rewload.lbls));
        for (rid in 1:length(rewload.lbls)) {      # rid <- 1;
          fname <- paste0(glm.path, sub.ids[sid], "/RESULTS/COGED_RewardLoad/", sub.ids[sid], "_", rewload.lbls[rid], "_", do.type, ".txt");
          if (file.exists(fname)) { rewload.lst[[rid]]  <- read.delim(fname)[-c(1,2)]; } # take off first two (label) columns
        }
        for  (pid in 1:num.cols)  { # pid <- 1
          # means for 2, 3, 4
          for (vid in 2:4) {   # vid <- 3;
            inds <- which(rewload.lbls %in% c(paste0("EV_Low_", vid), paste0("EV_Medium_", vid), paste0("EV_High_", vid)));
            vals.tbl <- rbind(rewload.lst[[inds[1]]][,paste0("Mean_",do.columns[pid])],
                              rewload.lst[[inds[2]]][,paste0("Mean_",do.columns[pid])],
                              rewload.lst[[inds[3]]][,paste0("Mean_",do.columns[pid])]);
            vals.reward <- apply (vals.tbl, 2, mean)
            # means for target knots (3-5, 10-13)
            val.reward.eval <- mean(vals.reward[mean.eval.row])
            val.reward.dec <- mean(vals.reward[mean.dec.row])
            #this needs to be changed.
            # mdd.tbl[sid*3+vid-4,4,pid] <- val.reward.eval
            # mdd.tbl[sid*3+vid-4,5,pid] <- val.reward.dec
            # mdd.tbl[sid*3+vid-4,3,pid] <- paste0("rew",vid)
            OA.tbl[sid*3+vid-4,4,pid] <- val.reward.eval
            OA.tbl[sid*3+vid-4,5,pid] <- val.reward.dec
            OA.tbl[sid*3+vid-4,3,pid] <- paste0("rew",vid)
          }
        }
      }
    }
    if (g == 2) { # g <- 2
      # hc.tbl <- array(0, c(3*length(sub.ids), 5, num.cols)); #trying to make this new data.frame to hold my data
      # colnames(hc.tbl) <- c("sub.id","group","reward","evaluation","decision")
      # hc.tbl[,2,] <- rep("HC")
      # hc.tbl[,1,] <- rep(sub.ids, each =3)
      MA.tbl <- array(0, c(3*length(sub.ids), 5, num.cols)); #trying to make this new data.frame to hold my data
      colnames(MA.tbl) <- c("sub.id","group","reward","evaluation","decision")
      MA.tbl[,2,] <- rep("MA")
      MA.tbl[,1,] <- rep(sub.ids, each =3)
      for (sid in 1:length(sub.ids)) {   # sid <- 1;
        rewload.lbls <- c(paste0("EV_Low_", 2:4), paste0("EV_Medium_", 2:4), paste0("EV_High_", 2:4));
        rewload.lst <- vector("list", length(rewload.lbls));
        for (rid in 1:length(rewload.lbls)) {      # rid <- 1;
          fname <- paste0(glm.path, sub.ids[sid], "/RESULTS/COGED_RewardLoad/", sub.ids[sid], "_", rewload.lbls[rid], "_", do.type, ".txt");
          if (file.exists(fname)) { rewload.lst[[rid]]  <- read.delim(fname)[-c(1,2)]; } # take off first two (label) columns
        }
        for  (pid in 1:num.cols)  { # pid <- 1
          # means for 2, 3, 4
          for (vid in 2:4) {   # vid <- 3;
            inds <- which(rewload.lbls %in% c(paste0("EV_Low_", vid), paste0("EV_Medium_", vid), paste0("EV_High_", vid)));
            vals.tbl <- rbind(rewload.lst[[inds[1]]][,paste0("Mean_",do.columns[pid])],
                              rewload.lst[[inds[2]]][,paste0("Mean_",do.columns[pid])],
                              rewload.lst[[inds[3]]][,paste0("Mean_",do.columns[pid])]);
            vals.reward <- apply (vals.tbl, 2, mean)
            # means for target knots (3-5, 10-13)
            val.reward.eval <- mean(vals.reward[mean.eval.row])
            val.reward.dec <- mean(vals.reward[mean.dec.row])
            #this needs to be changed.
            # hc.tbl[sid*3+vid-4,4,pid] <- val.reward.eval
            # hc.tbl[sid*3+vid-4,5,pid] <- val.reward.dec
            # hc.tbl[sid*3+vid-4,3,pid] <- paste0("rew",vid)
            MA.tbl[sid*3+vid-4,4,pid] <- val.reward.eval
            MA.tbl[sid*3+vid-4,5,pid] <- val.reward.dec
            MA.tbl[sid*3+vid-4,3,pid] <- paste0("rew",vid)
          }
        }
      }
    }
  }
  #Combine table, getting ready for anova
  tbl <- abind(OA.tbl, MA.tbl,along=1) 
  anova.result.eval <- array(0, c(5,5,num.cols))
  anova.result.dec <- array(0, c(5,5,num.cols))
  #Loop through each parcel to calculate anova
  for (pid in 1:num.cols)  { # pid <- 1 
    tbl.parcel <- as.data.frame(tbl[,,pid])
    tbl.parcel$evaluation <- as.numeric(as.character(tbl.parcel$evaluation))
    tbl.parcel$decision <- as.numeric(as.character(tbl.parcel$decision))
    tbl.parcel$group <- as.factor (ifelse(tbl.parcel$group == "MA", 0, 1))
    tbl.parcel$reward <- relevel(tbl.parcel$reward, ref = "rew2")
    for (tid in c("evaluation","decision")) {
      if (tid == "evaluation") {anova <- summary(aov(tbl.parcel$evaluation ~ tbl.parcel$group*tbl.parcel$reward + Error(tbl.parcel$sub.id/tbl.parcel$reward))) 
      } else {
        anova <- summary(aov(tbl.parcel$decision ~ tbl.parcel$group*tbl.parcel$reward + Error(tbl.parcel$sub.id/tbl.parcel$reward)))
      }
      a <- data.frame(anova[[1]][[1]])
      b <- data.frame(anova[[2]][[1]])
      anova.final<-rbind(a,b)
      if (tid == "evaluation") {
        rownames(anova.result.eval) <- row.names(anova.final)
        colnames(anova.result.eval) <- colnames(anova.final)
        anova.result.eval[,,pid] <- as.matrix(anova.final)
      } else {
        rownames(anova.result.dec) <- row.names(anova.final)
        colnames(anova.result.dec) <- colnames(anova.final)
        anova.result.dec[,,pid] <- as.matrix(anova.final)
      }
    }
  }
  # saveRDS(anova.result.eval, file=paste0(out.dirname,"anova_eval_", do.type,".rds"))
  # saveRDS(anova.result.dec, file=paste0(out.dirname,"anova_dec_", do.type,".rds"))
  saveRDS(anova.result.eval, file=paste0(out.dirname,"anova_eval_age_", do.type,".rds"))
  saveRDS(anova.result.dec, file=paste0(out.dirname,"anova_dec_age_", do.type,".rds"))
}

get.ANOVA.stat("subcortical_vol")
get.ANOVA.stat("Schaefer2018_400x7_vol")
###########################################################################################################################################################
##### 2 by 2 ANOVA: age & MDD
rm(list=ls());  
library(abind)
library(knitr)
atlas.path <- "/data/MARCER/ATLASES/";  
out.dirname <- paste0("/scratch2/weie/COGED_CODE/Stats/");   # top-level output directory for this particular analysis
glm.path <- "/data/MARCER/participantData/AFNI_ANALYSIS/";   # top-level input directory
group <- c("OA", "MA")
subcort.tbl <- read.csv(paste0(atlas.path, "subcorticalKey.csv"), stringsAsFactors=FALSE);  # subcortical_MARCER3p0.nii.gz parcel labels
MDD.status <- read.table(paste0("/scratch2/weie/COGED_CODE/MDD_subjectSet.txt"), stringsAsFactors=FALSE)
# Set the knots for evaluation phase and decision phase
mean.eval.row <- 3:5
mean.dec.row <- 10:13

get.ANOVA.stat <- function(do.type) { # do.type <- "Schaefer2018_400x7_vol"; # do.type <- "subcortical_vol"
  if (do.type == "Schaefer2018_400x7_vol") { num.cols <- 400; num.rows <- 25; do.columns <- 1:400} 
  if (do.type == "subcortical_vol") { num.cols <- 19; num.rows <- 25; do.columns <- subcort.tbl$HCP.label} 
  for (g in 1:length(group)) {   #  g <-1
    sub.tbl <- read.table(paste0("/scratch2/weie/COGED_CODE/", group[g], "_subjectSet.txt"), stringsAsFactors=FALSE);
    sub.ids <- sort(sub.tbl$sub.id[which(sub.tbl[,paste0("COGED_valuationSV")] == TRUE)]);
    if (g == 1) {
      OA.tbl <- array(0, c(length(sub.ids), 5, num.cols)); #trying to make this new data.frame to hold my data
      colnames(OA.tbl) <- c("sub.id","age.group","mdd.group","evaluation","decision")
      OA.tbl[,2,] <- rep("OA");
      OA.tbl[,1,] <- rep(sub.ids);
      OA.tbl[,3,] <- ifelse(OA.tbl[,1,] %in% MDD.status$sub.id, "MDD", "HC")
      for (sid in 1:length(sub.ids)) {   # sid <- 1;
        valSV.lbls <- "meanResp";
        fname <- paste0(glm.path, sub.ids[sid], "/RESULTS/COGED_valuationSV/", sub.ids[sid], "_", valSV.lbls, "_", do.type, ".txt");
        if (file.exists(fname)) { valSV.tbl <- read.delim(fname)[-c(1,2)]; } # take off first two (label) columns
        for  (pid in 1:num.cols)  { # pid <- 1
          # means for target knots (3-5, 10-13)
          val.reward.eval <- mean(valSV.tbl[mean.eval.row,pid])
          val.reward.dec <- mean(valSV.tbl[mean.dec.row,pid])
          OA.tbl[sid,4,pid] <- val.reward.eval
          OA.tbl[sid,5,pid] <- val.reward.dec
        }
      }
    }
    if (g == 2) { # g <- 2
      MA.tbl <- array(0, c(length(sub.ids), 5, num.cols)); #trying to make this new data.frame to hold my data
      colnames(MA.tbl) <- c("sub.id","age.group","mdd.group","evaluation","decision")
      MA.tbl[,2,] <- rep("MA")
      MA.tbl[,1,] <- rep(sub.ids);
      MA.tbl[,3,] <- ifelse(MA.tbl[,1,] %in% MDD.status$sub.id, "MDD", "HC")
      for (sid in 1:length(sub.ids)) {   # sid <- 1;
        valSV.lbls <- "meanResp";
        fname <- paste0(glm.path, sub.ids[sid], "/RESULTS/COGED_valuationSV/", sub.ids[sid], "_", valSV.lbls, "_", do.type, ".txt");
        if (file.exists(fname)) { valSV.tbl <- read.delim(fname)[-c(1,2)]; } # take off first two (label) columns
        for  (pid in 1:num.cols)  { # pid <- 1
          # means for target knots (3-5, 10-13)
          val.reward.eval <- mean(valSV.tbl[mean.eval.row,pid])
          val.reward.dec <- mean(valSV.tbl[mean.dec.row,pid])
          MA.tbl[sid,4,pid] <- val.reward.eval
          MA.tbl[sid,5,pid] <- val.reward.dec
        }
      }
    }
  }
  #Combine table, getting ready for anova
  tbl <- abind(OA.tbl, MA.tbl,along=1) 
  anova.result.eval <- array(0, c(4,5,num.cols))
  anova.result.dec <- array(0, c(4,5,num.cols))
  #Loop through each parcel to calculate anova
  for (pid in 1:num.cols)  { # pid <- 1 
    tbl.parcel <- as.data.frame(tbl[,,pid])
    tbl.parcel$evaluation <- as.numeric(as.character(tbl.parcel$evaluation))
    tbl.parcel$decision <- as.numeric(as.character(tbl.parcel$decision))
    tbl.parcel$age.group <- as.factor (ifelse(tbl.parcel$age.group == "MA", 0, 1))
    tbl.parcel$mdd.group <- as.factor (ifelse(tbl.parcel$mdd.group == "HC", 0, 1))
    for (tid in c("evaluation","decision")) { #tid <- "decision"
      if (tid == "evaluation") {anova <- summary(aov(tbl.parcel$evaluation ~ tbl.parcel$age.group*tbl.parcel$mdd.group)) 
      } else {
        anova <- summary(aov(tbl.parcel$decision ~ tbl.parcel$age.group*tbl.parcel$mdd.group))
      }
      anova.final <- data.frame(anova[[1]])
      if (tid == "evaluation") {
        rownames(anova.result.eval) <- row.names(anova.final)
        colnames(anova.result.eval) <- colnames(anova.final)
        anova.result.eval[,,pid] <- as.matrix(anova.final)
      } else {
        rownames(anova.result.dec) <- row.names(anova.final)
        colnames(anova.result.dec) <- colnames(anova.final)
        anova.result.dec[,,pid] <- as.matrix(anova.final)
      }
    }
  }
  # saveRDS(anova.result.eval, file=paste0(out.dirname,"anova_eval_", do.type,".rds"))
  # saveRDS(anova.result.dec, file=paste0(out.dirname,"anova_dec_", do.type,".rds"))
  saveRDS(anova.result.eval, file=paste0(out.dirname,"anova_eval_age&mdd_", do.type,".rds"))
  saveRDS(anova.result.dec, file=paste0(out.dirname,"anova_dec_age&mdd_", do.type,".rds"))
}

get.ANOVA.stat("subcortical_vol")
get.ANOVA.stat("Schaefer2018_400x7_vol")

###########################################################################################################################################################

##### Evaluating the finding
# subcortical.eval <- readRDS(paste0(out.dirname,"anova_eval_subcortical_vol.rds"))
# Schaefer.eval <- readRDS(paste0(out.dirname,"anova_eval_Schaefer2018_400x7_vol.rds"))
# subcortical.dec <- readRDS(paste0(out.dirname,"anova_dec_subcortical_vol.rds"))
# Schaefer.dec <- readRDS(paste0(out.dirname,"anova_dec_Schaefer2018_400x7_vol.rds"))
subcortical.eval <- readRDS(paste0(out.dirname,"anova_eval_age&mdd_subcortical_vol.rds"))
Schaefer.eval <- readRDS(paste0(out.dirname,"anova_eval_age&mdd_Schaefer2018_400x7_vol.rds"))
subcortical.dec <- readRDS(paste0(out.dirname,"anova_dec_age&mdd_subcortical_vol.rds"))
Schaefer.dec <- readRDS(paste0(out.dirname,"anova_dec_age&mdd_Schaefer2018_400x7_vol.rds"))



######Find significant parcels#####
# #Group effect
# which(subcortical.eval[1,5,] < 0.05) #1
# which(Schaefer.eval[1,5,] < 0.05) #13  37  40  93  96 103 114 115 116 150 152 154 158 159 166 169 170 172 193 285 321 322 324 357 396
# which(subcortical.dec[1,5,] < 0.05) 
# which(Schaefer.dec[1,5,] < 0.05) #62
# #reward effect
# which(subcortical.eval[3,5,] < 0.05) #1
# which(Schaefer.eval[3,5,] < 0.05) #29  78  80  82 102 130 131 132 137 139 141 144 171 184 187 188 193 280 281 282 284 303 307 308 320 334 336 337 345 349 356
# which(subcortical.dec[3,5,] < 0.05) #3  6 15
# which(Schaefer.dec[3,5,] < 0.05) # 96  99 122 123 127 128 129 130 131 133 135 136 137 139 140 141 142 143 147 148 149 150 151 154 155 158 160 162 163 164 165
# # 166 170 171 172 173 174 175 178 179 180 182 183 184 185 186 187 188 196 197 198 199 201 271 272 273 274 280 294 332 333 334
# # 336 337 339 340 342 344 345 346 347 348 349 350 351 352 353 354 357 358 360 361 362 363 364 365 366 368 371 373 375 377 378
# # 380 381 382 383 385 386 387 389 390 391 397 398 399
# #group:reward effect
# which(subcortical.eval[4,5,] < 0.05) 
# which(Schaefer.eval[4,5,] < 0.05) #65  85 114 115 150 170 203 224 233 234 284 297 301 320 399
# which(subcortical.dec[4,5,] < 0.05)
# which(Schaefer.dec[4,5,] < 0.05) #134 219

#age Group effect
which(subcortical.eval[1,5,] < 0.05) #13
which(Schaefer.eval[1,5,] < 0.05) 
# 10 11  16  20  21  22  25  26  29  30  31  48  83 103 107 111 112 115 127 129 131 147 149 153 166 177 181 186 187 193
# 194 195 196 198 219 224 225 228 229 231 246 306 313 332 339 358 360 367 368 383 384 390 398 399
which(subcortical.dec[1,5,] < 0.05) #5 17
which(Schaefer.dec[1,5,] < 0.05) 
#   4   5  10  11  13  16  20  21  22  24  25  26  27  28  29  30  31  36  45  48  65  83 103 107 115 127 129 131 132 144
# 147 162 164 166 171 174 177 178 181 186 187 192 193 194 195 196 198 214 218 219 220 224 225 226 228 246 280 292 339 354
# 358 359 360 362 364 375 381 384 387 394 397 398

#mdd group effect
which(subcortical.eval[2,5,] < 0.05) #9
which(Schaefer.eval[2,5,] < 0.05) #93  96 114 115 116 150 152 154 158 159 166 169 170 172 285 321 322 324 357 396
which(subcortical.dec[2,5,] < 0.05) #19
which(Schaefer.dec[2,5,] < 0.05) #62

#age:mdd effect
which(subcortical.eval[3,5,] < 0.05) 
which(Schaefer.eval[3,5,] < 0.05) #119 124 142 149 193 194 294 353 394 396
which(subcortical.dec[3,5,] < 0.05)
which(Schaefer.dec[3,5,] < 0.05) # 117 148 158 161 162 182 207 274 364
