###########################################################################################################################################################
# code to make unpair t test results for meanResp between HC and MDD group
# adapated by Emily Wei, 6 Nov 2023. makeAverageImages.R is the whole-brain version.
# updated by Emily Wei, 20 Dec 2023 to incorporate unpaired T test mean Response difference in different age groups. 

# note: this calculates *robust* means and t-tests, not regular.
# This analysis includes 35 MDD participants and 44 HC participants.
# This analysis includes 39 OA participants and 40 MA participants.
###########################################################################################################################################################

library(DescTools);   # for robust t-test (YuenTTest)
rm(list=ls());  
out.dirname <- paste0("/scratch2/weie/COGED_CODE/Stats/");   # top-level output directory for this particular analysis
glm.path <- "/data/MARCER/participantData/AFNI_ANALYSIS/";   # top-level input directory
# group <- c("MDD", "HC")
group <- c("OA", "MA")
# Set the knots for evaluation phase and decision phase
mean.eval.row <- 3:5
mean.dec.row <- 10:13
do.trim <- 0.1;   # how much to trim in the robust stats

get.t.stat <- function(do.type) { # do.type <- "Schaefer2018_400x7_vol"; # do.type <- "subcortical_vol"
  if (do.type == "Schaefer2018_400x7_vol") { num.cols <- 400; num.rows <- 25; } 
  if (do.type == "subcortical_vol") { num.cols <- 19; num.rows <- 25;} 
  for (g in 1:length(group)) {   #  g <-1
    sub.tbl <- read.table(paste0("/scratch2/weie/COGED_CODE/", group[g], "_subjectSet.txt"), stringsAsFactors=FALSE);
    sub.ids <- sort(sub.tbl$sub.id[which(sub.tbl[,paste0("COGED_valuationSV")] == TRUE)]);
    has.meanResp <- rep(0, length(sub.ids));
    # get the people to include in this particular group analysis (can vary with task)
    for (sid in 1:length(sub.ids)) {   # sid <- 1;
      meanResp.file <- paste0(glm.path, sub.ids[sid], "/RESULTS/COGED_valuationSV/", sub.ids[sid], "_meanResp_", do.type, ".txt");
      if (file.exists(meanResp.file)) {has.meanResp[sid]<-1 }
    }
    sub.ids.final <- sub.ids[which(has.meanResp == 1)];
    
    if (g == 1) {
      # mdd.tbl <- array(0, c(num.rows, num.cols, length(sub.ids.final))); #Creating a 3darray to hold all MDD participant's data
      OA.tbl <- array(0, c(num.rows, num.cols, length(sub.ids.final))); #Creating a 3darray to hold all OA participant's data
      for (sid in 1:length(sub.ids.final)) {   # sid <- 1;
        fname <- paste0(glm.path, sub.ids.final[sid], "/RESULTS/COGED_valuationSV/", sub.ids.final[sid], "_meanResp_", do.type, ".txt");
        if (file.exists(fname)) {
          in.tbl <- read.delim(fname)[,-c(1,2)]; # take off first two (label) columns
          # if (nrow(in.tbl) != num.rows | ncol(in.tbl) != num.cols) { stop("ERROR: unexpected in.tbl.");
          # } else { mdd.tbl[,,sid] <- as.matrix(in.tbl);  # as.matrix needed to undo the data.frame
          # }
          if (nrow(in.tbl) != num.rows | ncol(in.tbl) != num.cols) { stop("ERROR: unexpected in.tbl.");
          } else { OA.tbl[,,sid] <- as.matrix(in.tbl);  # as.matrix needed to undo the data.frame
          }
        }
      }
      # mdd.mean.tbl <- array(0,c(dim(mdd.tbl)[3], num.cols, 2)) #Creating a new 3darray to hold the two means during evaluation phase (knots 3-5) and decision phase (knots 10-13)
      OA.mean.tbl <- array(0,c(dim(OA.tbl)[3], num.cols, 2))
      for (sid in 1:length(sub.ids.final)) { #sid<-1
        for (pid in 1:num.cols) { #pid<-1
          # mean.eval <- mean(mdd.tbl[mean.eval.row,pid,sid]);
          # mean.dec <- mean(mdd.tbl[mean.dec.row,pid,sid]);
          # mdd.mean.tbl[sid,pid,1] <-mean.eval
          # mdd.mean.tbl[sid,pid,2] <-mean.dec
          mean.eval <- mean(OA.tbl[mean.eval.row,pid,sid]);
          mean.dec <- mean(OA.tbl[mean.dec.row,pid,sid]);
          OA.mean.tbl[sid,pid,1] <-mean.eval
          OA.mean.tbl[sid,pid,2] <-mean.dec
        }
      }
      # write.table(mdd.mean.tbl, paste0(out.dirname,do.type,"_MDD_mean.txt"), sep = "\t", row.names = TRUE, col.names = TRUE)
      write.table(OA.mean.tbl, paste0(out.dirname,do.type,"_OA_mean.txt"), sep = "\t", row.names = TRUE, col.names = TRUE)
    } 
    
    if (g == 2) {
      # hc.tbl <- array(0, c(num.rows, num.cols, length(sub.ids.final))); #Creating a 3darray to hold all HC participant's data
      MA.tbl <- array(0, c(num.rows, num.cols, length(sub.ids.final)));
      for (sid in 1:length(sub.ids.final)) {   # sid <- 1;
        fname <- paste0(glm.path, sub.ids.final[sid], "/RESULTS/COGED_valuationSV/", sub.ids.final[sid], "_meanResp_", do.type, ".txt");
        if (file.exists(fname)) {
          in.tbl <- read.delim(fname)[,-c(1,2)]; # take off first two (label) columns
          # if (nrow(in.tbl) != num.rows | ncol(in.tbl) != num.cols) {stop("ERROR: unexpected in.tbl.");
          # } else { hc.tbl[,,sid] <- as.matrix(in.tbl);  # as.matrix needed to undo the data.frame}
          # }
          if (nrow(in.tbl) != num.rows | ncol(in.tbl) != num.cols) {stop("ERROR: unexpected in.tbl.");
          } else { MA.tbl[,,sid] <- as.matrix(in.tbl);  # as.matrix needed to undo the data.frame}
          }
        }
      }
      # hc.mean.tbl <- array(0,c(dim(hc.tbl)[3], num.cols, 2)) #Creating a new 3darray to hold the two means during evaluation phase (knots 3-5) and decision phase (knots 10-13)
      MA.mean.tbl <- array(0,c(dim(MA.tbl)[3], num.cols, 2))
      for (sid in 1:length(sub.ids.final)) { #sid<-2
        for (pid in 1:num.cols) { #pid<-1
          # mean.eval <- mean(hc.tbl[mean.eval.row,pid,sid]);
          # mean.dec <- mean(hc.tbl[mean.dec.row,pid,sid]);
          # hc.mean.tbl[sid,pid,1] <-mean.eval
          # hc.mean.tbl[sid,pid,2] <-mean.dec
          mean.eval <- mean(MA.tbl[mean.eval.row,pid,sid]);
          mean.dec <- mean(MA.tbl[mean.dec.row,pid,sid]);
          MA.mean.tbl[sid,pid,1] <-mean.eval
          MA.mean.tbl[sid,pid,2] <-mean.dec
        }
      } 
      # write.table(hc.mean.tbl, paste0(out.dirname,do.type,"_HC_mean.txt"), sep = "\t", row.names = TRUE, col.names = TRUE)
      write.table(MA.mean.tbl, paste0(out.dirname,do.type,"_MA_mean.txt"), sep = "\t", row.names = TRUE, col.names = TRUE)
    }      
  }
  
  #Create a dataframe to hold the robust t value and p value for evaluation and decision
  robust.t.result <- data.frame(array(NA, c(4, num.cols)));
  colnames(robust.t.result) <- paste0("p", 1:num.cols);
  rownames(robust.t.result) <- c("t.eval","p.eval","t.dec","p.dec");
  
  for  (pid in 1:num.cols) {   # pid <- 75;
    # t<-YuenTTest(hc.mean.tbl[,pid,1], mdd.mean.tbl[,pid,1], paired = FALSE, trim=do.trim)
    t<-YuenTTest(MA.mean.tbl[,pid,1], OA.mean.tbl[,pid,1], paired = FALSE, trim=do.trim)
    robust.t.result[1,pid] <- t$statistic
    robust.t.result[2,pid] <- t$p.value
  }
  for (pid in 1:num.cols) {
    # t<-YuenTTest(hc.mean.tbl[,pid,2],mdd.mean.tbl[,pid,2],paired = FALSE, trim=do.trim)
    t<-YuenTTest(MA.mean.tbl[,pid,2],OA.mean.tbl[,pid,2],paired = FALSE, trim=do.trim)
    robust.t.result[3,pid] <- t$statistic
    robust.t.result[4,pid] <- t$p.value
  }
  # write.table(robust.t.result,paste0(out.dirname,do.type,"_robustttest.txt"), sep = "\t", row.names = TRUE, col.names = TRUE)
  write.table(robust.t.result,paste0(out.dirname,do.type,"_robustttest_age.txt"), sep = "\t", row.names = TRUE, col.names = TRUE)

  #####For Age version, I decided not to run t test just yet since MDD version didn't actually include t test. 
  #Create a dataframe to hold the t value and p value for evaluation and decision
  # t.result <- data.frame(array(NA, c(4, num.cols)));
  # colnames(t.result) <- paste0("p", 1:num.cols);
  # rownames(t.result) <- c("t.eval","p.eval","t.dec","p.dec");
  # 
  # for  (pid in 1:num.cols) {   # pid <- 75;
  #   t<-t.test(hc.mean.tbl[,pid,1], mdd.mean.tbl[,pid,1], paired = FALSE)
  #   t.result[1,pid] <- t$statistic
  #   t.result[2,pid] <- t$p.value
  # }
  # for (pid in 1:num.cols) {
  #   t<-t.test(hc.mean.tbl[,pid,2],mdd.mean.tbl[,pid,2],paired = FALSE)
  #   t.result[3,pid] <- t$statistic
  #   t.result[4,pid] <- t$p.value
  # }
  # write.table(t.result,paste0(out.dirname,do.type,"_ttest.txt"), sep = "\t", row.names = TRUE, col.names = TRUE)
}

get.t.stat("subcortical_vol")
get.t.stat("Schaefer2018_400x7_vol")

#####Inspecting my tables#####
Schaefer.mean <- read.delim(paste0(out.dirname,"Schaefer2018_400x7_vol_MDD_mean.txt"))
dim(Schaefer.mean) #There are 800 columns because there're two means per parcel, one for evaluation and one for decision. 

#####Find significant parcels#####
subcortical.robust <- read.delim(paste0(out.dirname,"subcortical_vol_robustttest.txt"))
Schaefer.robust <- read.delim(paste0(out.dirname,"Schaefer2018_400x7_vol_robustttest.txt"))
subcortical.t <- read.delim(paste0(out.dirname,"subcortical_vol_ttest.txt"))
Schaefer.t <- read.delim(paste0(out.dirname,"Schaefer2018_400x7_vol_ttest.txt"))

which(subcortical.robust[2,] < 0.05 | subcortical.robust[4,] < 0.05)
which(Schaefer.robust[2,] < 0.05 | Schaefer.robust[4,] < 0.05)
which(Schaefer.robust[2,] < 0.05)
which(Schaefer.robust[4,] < 0.05)
which(subcortical.t[2,] < 0.05 | subcortical.t[4,] < 0.05)
which(Schaefer.t[2,] < 0.05 | Schaefer.t[4,] < 0.05)

subcortical.robust[,9]
Schaefer.robust[,c(10,30,62,69,74,81,93,95,96, 103, 114, 115, 116, 118, 144, 150, 152, 154, 158, 159, 166, 168, 169, 170, 172, 193, 194 ,198, 200, 285, 321, 322, 357, 379, 396)]

# tiny bit of plotting
pid <- 82;
group.ids <- c(rep("HC", dim(hc.mean.tbl)[1]), rep("MDD", dim(mdd.mean.tbl)[1]));
mean.vals <- c(hc.mean.tbl[,pid,2], mdd.mean.tbl[,pid,2]);
p.tbl <- data.frame(group.ids, mean.vals);

boxplot(p.tbl$mean.vals~p.tbl$group.ids, notch=TRUE);  # ugly plot
tout <- t.test(hc.mean.tbl[,pid,2], mu=0)  # tout <- YuenTTest(hc.mean.tbl[,pid,2], mu=0, trim=do.trim)
text(x=0.8, y=0.4, labels=round(tout$statistic, 2))
tout <- t.test(mdd.mean.tbl[,pid,2], mu=0) 
text(x=1.8, y=0.4, labels=round(tout$statistic, 2))

tout <- t.test(hc.mean.tbl[,pid,2],mdd.mean.tbl[,pid,2], paired=FALSE)
text(x=1.5, y=0.4, labels=round(tout$statistic, 2))


