SciViews::R

#path <- "/media/sf_shared/projects/dataset/nutrient/raw/190212C.TXT"
#aa3_txt <- path

# read raw data of aa3 txt file ------------------------------------------------
read_raw_aa3 <- function(aa3_txt){
  # Import metadata and extract informaation
  readr::read_lines(aa3_txt, n_max = 12,
                    locale = readr::locale(encoding = "LATIN1")) %>%
    stringr::str_extract_all(
      pattern = "(-?\u03BC?\\w+:?\\.?-?/?\\w*:?/?\\d*)") -> header
  
  header %>%
    purrr::map(first) -> names(header) 
  
  header %>%
    purrr::map(function(x) x[-1]) -> header
  
  stringr::str_replace(header$ANAL, "NPinorganique.ANL",  "inorga") -> header$ANAL
  stringr::str_replace(header$ANAL, "NPorganique.ANL",  "orga") -> header$ANAL
  
  # Change NO3 in NOx
  stringr::str_replace_all(header$METH, "NO3", "NOx") -> header$METH
  
  # Extract nutrients that are analysed
  results <- header$METH
  
  # Prepare names of variables for raw_data
  stds <- paste(results, "std", sep = "_")
  concs <- paste(results, "conc", sep = "_")
  vals <- paste(results, "values", sep = "_")
  
  # Extract raw data
  raw_data <- readr::read_delim(file = aa3_txt, delim = ";", skip = 13)
  
  # Select variables in raw_data
  raw_data <- raw_data[ , -c(3, 5:7, 18, 19)]
  
  # Rename the columns in raw_data
  names(raw_data) <- c("sample_id", "peak_number", "sample_type",
                       "date_time", stds[1], concs[1], vals[1], stds[2],
                       concs[2], vals[2], stds[3], concs[3], vals[3])
  
  # recoding type of variable
  raw_data$sample_type <- as.factor(raw_data$sample_type)
  raw_data$date_time <- lubridate::dmy_hms(raw_data$date_time)
  
  direction <- aa3_txt
  dir <- stringr::str_split(direction, pattern = "/")
  dir_last <- stringr::str_remove(last(dir[[1]]), pattern = ".TXT")
  
  raw_data <- mutate(raw_data, run = dir_last)

  class(raw_data) <- c("aa3_raw", "aa3","tbl_df","tbl","data.frame")
  attr(raw_data, "run") <- dir_last
  attr(raw_data, "analyse") <- header[["ANAL"]]
  raw_data
} 

# fs::dir_ls('/media/sf_shared/projects/dataset/nutrient/raw/', glob = "*.TXT") -> t
#(test <- read_raw_aa3(t[47]))
#(test <- read_raw_aa3(aa3_txt))

# read metadata of aa3 txt file ------------------------------------------------

read_metadata_aa3 <- function(aa3_txt){
  # Import metadata and extract informaation
  readr::read_lines(aa3_txt, n_max = 12,
                    locale = readr::locale(encoding = "LATIN1")) %>%
    stringr::str_extract_all(
      pattern = "(-?\u03BC?\\w+:?\\.?-?/?\\w*:?/?\\d*)") -> header
  
  header %>%
    purrr::map(first) -> names_header
  
   names(header) <- stringr::str_to_lower(names_header)

  header %>%
    purrr::map(function(x) x[-1]) -> header
  
  stringr::str_replace(header$anal, "NPinorganique.ANL",  "inorga") -> header$anal
  stringr::str_replace(header$anal, "NPorganique.ANL",  "orga") -> header$anal

  if(length(header[["comm"]]) == 0){
    header$comm <- "NA"
  }
    
  if(length(header[["oper"]]) == 0){
    header[["oper"]] <- "NA"
  }
    
  as_tibble(header[1:6]) -> metadata
  
  metadata$date_time <- lubridate::dmy_hms(paste(metadata$date, metadata$time, sep = " "))
  metadata <- dplyr::select(metadata, -date, -time)
  
  class(metadata) <- c("aa3_metadata", "aa3","tbl_df","tbl","data.frame")
  
  direction <- aa3_txt
  dir <- stringr::str_split(direction, pattern = "/")
  dir_last <- stringr::str_remove(last(dir[[1]]), pattern = ".TXT")
  
  attr(metadata, "run") <- dir_last
  attr(metadata, "analyse") <- header[["anal"]]
  
  metadata
}

# fs::dir_ls('/media/sf_shared/projects/dataset/nutrient/raw/', glob = "*.TXT") -> t
# (met <- read_metadata_aa3(t[1]))
#(test <- read_metadata_aa3(aa3_txt))


# read method of aa3 txt file --------------------------------------------------

read_method_aa3 <- function(aa3_txt){
  # Import metadata and extract informaation
  readr::read_lines(aa3_txt, n_max = 12,
                    locale = readr::locale(encoding = "LATIN1")) %>%
    stringr::str_extract_all(
      pattern = "(-?\u03BC?\\w+:?\\.?-?/?\\w*:?/?\\d*)") -> header
  
  header %>%
    purrr::map(first) -> names_header
  
  names(header) <- stringr::str_to_lower(names_header)
  
  header %>%
    purrr::map(function(x) x[-1]) -> header
  
  stringr::str_replace(header$anal, "NPinorganique.ANL",  "inorga") -> header$anal
  stringr::str_replace(header$anal, "NPorganique.ANL",  "orga") -> header$anal
  
  # Change NO3 in NOx
  stringr::str_replace(header$meth, "NO3", "NOx") -> header$meth
  
  as_tibble(header[7:12]) -> method
  
  class(method) <- c("aa3_method", "aa3","tbl_df","tbl","data.frame")
  
  direction <- aa3_txt
  dir <- stringr::str_split(direction, pattern = "/")
  dir_last <- stringr::str_remove(last(dir[[1]]), pattern = ".TXT")
  
  attr(method, "run") <- dir_last
  attr(method, "analyse") <- header[["anal"]]
  method
}

# fs::dir_ls('/media/sf_shared/projects/dataset/nutrient/raw/', glob = "*.TXT") -> t
# (met <- read_method_aa3(t[1]))
#(test <- read_method_aa3(aa3_txt))
#
#
# read only sample date of aa3 txt file ----------------------------------------

read_samp_aa3 <- function(aa3_txt){
  raw <- read_raw_aa3(aa3_txt)
  samp <- filter(raw, sample_type == "SAMP")
  samp <- select(samp, - ends_with("std"), - ends_with("values"))
  samp$sample_type <- as.factor(as.character(samp$sample_type))
  class(samp) <- c("aa3_samp", "aa3","tbl_df","tbl","data.frame")
  samp
}

# fs::dir_ls('/media/sf_shared/projects/dataset/nutrient/raw/', glob = "*.TXT") -> t
#(test <- read_samp_aa3(t[47]))
#(test <- read_samp_aa3(aa3_txt))
#
#
# read only sample date of aa3 txt file ----------------------------------------

read_calb_aa3 <- function(aa3_txt){
  raw <- read_raw_aa3(aa3_txt)
  calb <- filter(raw, sample_type == "CALB")
  calb$sample_type <- as.factor(as.character(calb$sample_type))
  class(calb) <- c("aa3_calb", "aa3","tbl_df","tbl","data.frame")
  calb
}

# fs::dir_ls('/media/sf_shared/projects/dataset/nutrient/raw/', glob = "*.TXT") -> t
#(test <- read_calb_aa3(t[47]))
#(test <- read_calb_aa3(aa3_txt))
#
read_corresp_aa3 <- function(aa3_xlsx){
  corresp <- readxl::read_xlsx(aa3_xlsx, sheet = "data")
  corresp$sample_date <- as.character(corresp$sample_date)
  
  direction <- aa3_xlsx
  dir <- stringr::str_split(direction, pattern = "/")
  dir_last <- stringr::str_remove(last(dir[[1]]), pattern = ".xlsx")
  
  class(corresp) <- c("aa3_corresp", "aa3","tbl_df","tbl","data.frame")
  attr(corresp, "run") <- dir_last
  corresp
}

# fs::dir_ls('/media/sf_shared/projects/dataset/nutrient/raw/', glob = "*.xlsx") -> t1
#(test <- read_corresp_aa3('/media/sf_shared/projects/dataset/nutrient/raw/190318A.xlsx'))
#test$sample_date <- lubridate::ymd_hms(test$sample_date)
