# resample sphere ROIs from Jenny to match MARCER images
# 10 September 2024

# Make sphere mask file
rm(list=ls());  
options(warnPartialMatchDollar=TRUE);   # safety option

afni.path <- "/usr/local/pkg/afni_22/";   # path to the afni function executables
template.fname <- "/data/nil-external/ccp/COGED_PET/mask_COGED/aprioriSV.nodes.nii";  # sphere mask from Jenny
ex.fname <- "/data/MARCER/ATLASES/HCP_S1200T1w_MARCER3p0.nii.gz";   # example image the size we need

out.fname <- "/scratch2/weie/COGED_CODE/np2_trial/sphere_analysis/sphere.nii.gz";
if (!file.exists(out.fname)) {
  system2(paste0(afni.path, "3dresample"), args=paste0("-master ", ex.fname, " -prefix ", out.fname, " -inset ", template.fname), stdout=TRUE);
}

# making parcel-average timecourses
dir.desc <- "20250114";  
sub.tbl <- read.table(paste0("/scratch2/weie/COGED_CODE/SubjectSet/", dir.desc, "_subjectSet.txt"), stringsAsFactors=FALSE);
sub.ids <- sub.tbl$sub.id[which(sub.tbl$COGED_CatchNonCatch == TRUE)]
task.ids <- "COGED";  
afni.path <- "/usr/local/pkg/afni_22/";   # path to the afni function executables
p.fname <- "/scratch2/weie/COGED_CODE/np2_trial/sphere_analysis/sphere.nii.gz";   # sphere-parcel template
out.path <- "/scratch2/weie/COGED_CODE/np2_trial/sphere_analysis/sphere_data/"
run.ids <- c(1,2,3); 

for (sid in 1:length(sub.ids)) {
  for (rid in 1:length(run.ids)) {
    in.path <- paste0("/data/MARCER/participantData/AFNI_ANALYSIS/",sub.ids[sid],"/INPUT_DATA_NP2/");
    np2.fname <- paste0(in.path, "sub-", sub.ids[sid], "_task-", task.ids, "_run-", run.ids[rid], "_np2.nii.gz");
    out.fname <- paste0(out.path,"sub-", sub.ids[sid], "_ses-1_task-", task.ids, "_run", rid, "_np2_sphere.txt");  
    if (file.exists(np2.fname) & !file.exists(out.fname)) {
    system2(paste0(afni.path, "3dROIstats"), args=paste0("-mask ", p.fname, " '", np2.fname, "' > ", out.fname), stdout=TRUE);
    }
  }
}    

