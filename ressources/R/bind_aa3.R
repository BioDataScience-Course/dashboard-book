library(magrittr)
source("R/read_aa3.R")

bind_aa3 <- function(aa3_samp, aa3_corresp){
  
  attributes(aa3_samp)$run -> att_samp 
  attributes(aa3_corresp)$run -> att_corresp
  
  if(att_corresp != att_samp)
    stop(paste("deux run differents avec le txt ", att_samp, " et le xlsx ", att_corresp))
  
  if(all(aa3_samp$sample_id == aa3_corresp$sample_id) != 1){
    a <- aa3_samp$sample_id == aa3_corresp$sample_id
    print(a)
    stop(paste("le run ", att_corresp, " semble contenir une erreur", "veuillez corriger aa3_corresp"))
  }
    
  
  complete <- bind_cols(aa3_samp, aa3_corresp)
  class(complete) <- c("aa3_comp", "aa3","tbl_df","tbl","data.frame")
  attr(complete, "run") <- att_samp
  attr(complete, "analyse") <- attributes(aa3_samp)$analyse
  
  complete
}

# exemple
# 
#fs::dir_ls('/media/sf_shared/projects/dataset/nutrient/raw/', glob = "*.TXT") -> t
#fs::dir_ls('/media/sf_shared/projects/dataset/nutrient/raw/', glob = "*.xlsx") -> t1

#i <- 46
#
# test9 <-  read_corresp_aa3("/media/sf_shared/projects/dataset/nutrient/raw/190212C.xlsx")
# test8 <- read_samp_aa3("/media/sf_shared/projects/dataset/nutrient/raw/190212C.TXT")
# 
# for(i in 1:length(t)){
#   test10 <- bind_aa3(
#     aa3_corresp =  read_corresp_aa3(t1[i]), 
#     aa3_samp = read_samp_aa3(t[i]))
# }
#   
# test10 <- bind_aa3(
#  aa3_corresp =  read_corresp_aa3(t1[i]), 
#   aa3_samp = read_samp_aa3(t[i]))
# 
# #attributes(test10)
# #
# ##test10 <- bind_aa3(
# aa3_corresp =  read_corresp_aa3(t1[45]), 
#  aa3_samp = read_samp_aa3(t[46]))

#attributes(test10)
