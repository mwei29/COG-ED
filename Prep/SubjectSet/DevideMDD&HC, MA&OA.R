# started 20 Oct 2023 by Emily Wei; updated on 20 Dec 2023 by Emily to add in Age Group
# script to prepare for a MARCER group analysis with MDD group and HC group. This file writes a text file listing the people to include, and any missings they have.

# NOTE: this script only checks if the volume & surface STATS images exist for each person and session/task. The _subjectSet.txt file this code produces 
# should be checked against known missings, "sleeping people", etc., to be sure the desired sessions and tasks are included.

##### HC & MDD

# rm(list=ls())
# 
# 
# out.path <- paste0("/scratch2/weie/COGED_CODE/");   # where to make the output directory
# group.data <- read.csv(paste0(out.path, "demo.csv")); #Read in group assignment
# subject.set <- read.table(paste0(out.path,"20230714test_subjectSet.txt"), header = TRUE) #Read in subjectset
# group.data$Group.Assignment <- gsub("Healthy control", "HC", group.data$Group.Assignment) #Clean up Variable Name
# 
# for (group in c("MDD","HC")) {    # group <- "MDD"; 
#   subjects <- group.data$Participant.ID[group.data$Group.Assignment == group];
#   subjects <- intersect(subjects, subject.set$sub.id);
#   write.table(subject.set[subject.set$sub.id %in% subjects, ], paste0(out.path,group, "_subjectSet.txt"))
#   }

##### MA & OA

rm(list=ls())


out.path <- paste0("/scratch2/weie/COGED_CODE/");   # where to make the output directory
group.data <- read.csv(paste0(out.path, "demo.csv")); #Read in group assignment
subject.set <- read.table(paste0(out.path,"20230714test_subjectSet.txt"), header = TRUE) #Read in subjectset
group.data$Age.Group <- ifelse(group.data$Age.Bin %in% c("35-44 years", "45-54 years"), "MA",
                                                         ifelse(group.data$Age.Bin %in% c("55-64 years", "65-74 years"), "OA", NA)) #Create a new dichotomized age variable

for (group in c("OA","MA")) {    # group <- "MA"; 
  subjects <- group.data$Participant.ID[group.data$Age.Group == group];
  subjects <- intersect(subjects, subject.set$sub.id);
  write.table(subject.set[subject.set$sub.id %in% subjects, ], paste0(out.path,group, "_subjectSet.txt"))
}

##### Both
rm(list=ls())
out.path <- paste0("/scratch2/weie/COGED_CODE/")
group <- "OA";
sub.tbl.OA <- read.table(paste0("/scratch2/weie/COGED_CODE/", group, "_subjectSet.txt"), stringsAsFactors=FALSE);
group <- "MA";
sub.tbl.MA <- read.table(paste0("/scratch2/weie/COGED_CODE/", group, "_subjectSet.txt"), stringsAsFactors=FALSE);
group <- "MDD";
sub.tbl.MDD <- read.table(paste0("/scratch2/weie/COGED_CODE/", group, "_subjectSet.txt"), stringsAsFactors=FALSE);
group <- "HC";
sub.tbl.HC <- read.table(paste0("/scratch2/weie/COGED_CODE/", group, "_subjectSet.txt"), stringsAsFactors=FALSE);

subjects <- intersect(sub.tbl.OA$sub.id, sub.tbl.MDD$sub.id)
write.table(sub.tbl.OA[sub.tbl.OA$sub.id %in% subjects, ], paste0(out.path,"OAMDD_subjectSet.txt"))
subjects <- intersect(sub.tbl.OA$sub.id, sub.tbl.HC$sub.id)
write.table(sub.tbl.OA[sub.tbl.OA$sub.id %in% subjects, ], paste0(out.path,"OAHC_subjectSet.txt"))

subjects <- intersect(sub.tbl.MA$sub.id, sub.tbl.MDD$sub.id)
write.table(sub.tbl.MA[sub.tbl.MA$sub.id %in% subjects, ], paste0(out.path,"MAMDD_subjectSet.txt"))
subjects <- intersect(sub.tbl.MA$sub.id, sub.tbl.HC$sub.id)
write.table(sub.tbl.MA[sub.tbl.MA$sub.id %in% subjects, ], paste0(out.path,"MAHC_subjectSet.txt"))