dir <- '/Users/Marcelpv96/Dropbox/MASTER/1rANY/Q2/ML/Project/ML-Project/input/'

# FIRST STEP, DETECT MISSING VALUES #

detect_missings <- function(dir){
  list_files = list.files(path=dir, pattern="*.csv")
  all_df = c()
  for (i in 1:length(list_files)){
    all_df[i] = read.csv(paste(dir, list_files[i], sep=''))  
  }
  for (i in 1:length(list_files)){
    print(paste("> File <",list_files[i] ,"> has a number of: ", sum(is.na(all_df))," missing values.", sep='' ))
  }
}


# TREATMENT OF MIXED DATA TYPES #

