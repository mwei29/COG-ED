###########################################################################################################################################################
# code to make calculated SV and mean resp by Emily Wei, 30 Oct 2023. 
###########################################################################################################################################################

rm(list=ls());  

# file made with defineSubjectSet.R, and lists which sub.ids to include in the group analysis named dir.desc
sub.tbl <- read.table(paste0("/scratch2/weie/COGED_CODE/20230714test_subjectSet.txt"), stringsAsFactors=FALSE);
out.dirname <- paste0("/scratch2/weie/COGED_CODE/");   # top-level output directory for this particular analysis


##### shouldn't have to change code below here #####

glm.path <- "/data/MARCER/participantData/AFNI_ANALYSIS/";   # top-level input directory
# get the people to include in this particular group analysis (can vary with task)
sub.ids <- sort(sub.tbl$sub.id[which(sub.tbl[,paste0("COGED_valuationSV")] == TRUE)]);
do.cor <- function(do.type) {    
  # do.type <- "Schaefer2018_400x7_vol"
  if (!dir.exists(out.dirname)) { dir.create(out.dirname); }
  if (do.type == "Schaefer2018_400x7_vol") { num.cols <- 400; mid.path <- "RESULTS";} 
  if (do.type == "subcortical_vol") { num.cols <- 19; mid.path <- "RESULTS";} 
  has.both<- rep(0, length(sub.ids));
  # get the people to include in this particular group analysis (can vary with task)
  for (sid in 1:length(sub.ids)) {   # sid <- 73;
    SV.file <- paste0(glm.path,sub.ids[sid], "/", mid.path, "/", "COGED_valuationSV/", sub.ids[sid], "_SV_", do.type, ".txt");
    meanResp.file <- paste0(glm.path,sub.ids[sid], "/", mid.path, "/", "COGED_valuationSV/", sub.ids[sid], "_meanResp_", do.type, ".txt");
    if (file.exists(SV.file) && file.exists(meanResp.file)) {
      has.both[sid]<-1
    }
  }
  sub.ids.final <- sub.ids[which(has.both == 1)];
  SV.list <- list()
  meanResp.list <- list()
  for (sid in 1:length(sub.ids.final)) {   # sid <- 1;
    SV.file <- paste0(glm.path, sub.ids.final[sid], "/", mid.path, "/", "COGED_valuationSV/", sub.ids.final[sid], "_SV_", do.type, ".txt");
    meanResp.file <- paste0(glm.path,sub.ids.final[sid], "/", mid.path, "/", "COGED_valuationSV/", sub.ids.final[sid], "_meanResp_", do.type, ".txt");  
    SV.data <- read.delim(SV.file)[,-c(1,2)];
    meanResp.data <- read.delim(meanResp.file)[,-c(1,2)];
    SV.list[[sid]] <- SV.data;
    meanResp.list [[sid]] <- meanResp.data;
    }
 
  correlation.matrix <- matrix(NA, nrow = nrow(SV.data), ncol = ncol(SV.data))
  for (TENT in 1:nrow(SV.data)) { # TENT<-1
    for (parcel in 1:ncol(SV.data)) { # parcel<-1
    SV <- sapply(SV.list, function(sub_sv) as.numeric(sub_sv[TENT, parcel]))
    meanResp <- sapply(meanResp.list, function(sub_meanResp) as.numeric(sub_meanResp[TENT, parcel]))
    correlation <- cor(SV, meanResp)
    correlation.matrix [TENT,parcel] <- correlation
    }
  }
  write.table(correlation.matrix, file = paste0(out.dirname, "/correlation_matrix_", do.type,".txt"), sep = "\t", row.names = FALSE, col.names = TRUE)
}


for (space.id in c("Schaefer2018_400x7_vol", "subcortical_vol")) {
  do.cor (space.id)
}

###########################################################################################################################################################
###########################################################################################################################################################

