# resample sphere ROIs from Jenny to match MARCER images
# 10 September 2024


options(warnPartialMatchDollar=TRUE);   # safety option
rm(list=ls());  

afni.path <- "/usr/local/pkg/afni_22/";   # path to the afni function executables

template.fname <- "/data/nil-external/ccp/COGED_PET/mask_COGED/aprioriSV.nodes.nii";  # sphere mask from Jenny
ex.fname <- "/data/MARCER/ATLASES/HCP_S1200T1w_MARCER3p0.nii.gz";   # example image the size we need
out.fname <- "/scratch2/JoEtzel/temp/test.nii.gz";

system2(paste0(afni.path, "3dresample"), args=paste0("-master ", ex.fname, " -prefix ", out.fname, " -inset ", template.fname), stdout=TRUE);


# making parcel-average timecourses

p.fname <- "/scratch2/JoEtzel/temp/test.nii.gz";   # sphere-parcel template
np2.fname <- "/data/MARCER/participantData/AFNI_ANALYSIS/3235/INPUT_DATA_NP2/sub-3235_task-COGED_run-2_np2.nii.gz"
out.fname <- "/scratch2/JoEtzel/temp/testtimecourse2.txt";   
system2(paste0(afni.path, "3dROIstats"), args=paste0("-mask ", p.fname, " '", np2.fname, "' > ", out.fname), stdout=TRUE);
