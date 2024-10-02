# started 14 July 2023 by Jo Etzel, adapted from \R01\Jo\GLMs\TEMPLATE_1TRpK_groupGLMs\defineSubjectSet.R
# script to prepare for a MARCER group analysis. This file writes a text file listing the people to include, and any missings they have.

# NOTE: this script only checks if the volume & surface STATS images exist for each person and session/task. The _subjectSet.txt file this code produces 
# should be checked against known missings, "sleeping people", etc., to be sure the desired sessions and tasks are included.

# for now, just feed it a name and set of sub.ids\
# Modified 17 July 2024 by Mengzhe Wei, looking for the entire subject set that needs to np2 images ran.
# Main edit of this version: Did not use get.id since get.id in helperFunctions.R is  not updated.
# Instead, I pulled the participants ID from data_per_participant.xlsx in MARCER/Study Status Review Box. 
rm(list=ls())

######## change these lines

desc.str <- "20240717";   # what to call the output directory and append to the output knitr names
list.id <- "all";   
out.path <- paste0("/scratch2/weie/COGED_CODE/");   # where to make the output directory

######## shouldn't have to change these lines

# source("/data/MARCER/CODE/helperFunctions.R");   # for get.sub.ids();
# sub.ids <- get.ids(); 
# 2nd version of getting sub.ids since helperFunctions.R is not update.
library(gdata)
use.boxr <- TRUE;
if (use.boxr == TRUE) {      # set variables needed to run this code at wustl and interact with box
  if (require(boxr) == FALSE) { stop("did not find the boxr library but use.boxr == TRUE"); }   # https://github.com/r-box/boxr
  box_auth();   # initiate the link to box.  
  pid.path <- "140803530320";   # box ID for Mechanisms of Age-Related Changes in ER (MARCER)/Data/Study Status Review/
}

if (use.boxr == TRUE) {
  fname <- "data_per_participant"
  boxr.in <- as.data.frame(box_search(paste0('"', fname, '"'), type='file', content_types="name", file_extensions='xlsx', ancestor_folder_ids=pid.path));
  ind <- which(boxr.in$name == paste0(fname, ".xlsx"));
  if (length(ind) == 1) { box_dl(boxr.in$id[ind], local_dir=file.path(out.path));     # found the right file, so download
  } else { print(paste0("ERROR: didn't find ", fname, ".txt in box!")); }
}
  
in.path <- "/data/MARCER/participantData/AFNI_ANALYSIS/"; 
# if (!dir.exists(out.path)) { dir.create(out.path); }    # make output directory if it doesn't already exist

glm.ids <- c("COGED_CatchNonCatch", "COGED_RewardLoad", "COGED_valuationSV", "IAPS_CueType", "IAPS_ONs", "IAPS_Valence", "NBACK_BLOCK");  
sub.tbl <- read.xls(paste0(out.path,fname,".xlsx"))
sub.ids <- sub.tbl$sub.id
#excluding 5128 for now since 5128 has not finished GLMs
sub.ids <- sub.ids[sub.ids !=5128]
  
if (length(sub.ids) != length(unique(sub.ids))) { stop("length(sub.ids) != length(unique(sub.ids))"); }    # confirm no duplicates

# sub.ids <- sub.ids[-which(sub.ids == "3958")];   # in progress

# table to store whether each person should be included in each GLM. Lots of missings, since not everyone does the fMRI session.
# check for the presence of one contrast for each; there should never be a case where only some contrasts are present.
# to.drop <- rep(FALSE, nrow(sub.tbl));  # use to store all-missings sids

sub.tbl <- data.frame(array(NA, c(length(sub.ids), length(glm.ids)+1)));
to.drop <- rep(FALSE, nrow(sub.tbl));  # use to store all-missings sids

colnames(sub.tbl) <- c("sub.id", glm.ids);
for (sid in 1:length(sub.ids)) {    # sid <- 1; 
  sub.tbl$sub.id[sid] <- sub.ids[sid];
  for (gid in 1:length(glm.ids)) {   # gid <- 1; 
    fname.vol <- paste0(in.path, sub.ids[sid], "/RESULTS/", glm.ids[gid], "/STATS_", sub.ids[sid], "_REML.nii.gz");
    fname.surf <- paste0(in.path, sub.ids[sid], "/RESULTS_SURFACE/", glm.ids[gid], "/STATS_", sub.ids[sid], "_REML_L.func.gii");
    if (file.exists(fname.vol) & file.exists(fname.surf)) { sub.tbl[sid,glm.ids[gid]] <- "TRUE"; }
    
    if (!file.exists(fname.vol) & file.exists(fname.surf) | file.exists(fname.vol) & !file.exists(fname.surf)) { 
      stop(paste("mismatching missings!", sub.ids[sid], glm.ids[gid])); }
  }
  
  if (length(which(is.na(sub.tbl[sid,]))) == length(glm.ids)) { to.drop[sid] <- TRUE; }
}
sub.tbl <- sub.tbl[-which(to.drop == TRUE),];   # take off rows for people with all missings.

write.table(sub.tbl, paste0(out.path, desc.str, "_subjectSet.txt"));
