
# Warnings related to the sample information sheet
warnings_sample <- function(dframe){
  msg <- ""
  vars <- colnames(dframe)
  
  if(vars[1] != "SAMPLE_ID"){
    msg <- paste(msg,"- First column is not named \"SAMPLE_ID\"", sep = "<br/>")
  }
  # R replaces parentheses, brackets and spaces with a dot "."
  # Duplicated column names receive indexing at the end, for example Time and Time.1
  if (grepl(".",vars, fixed = TRUE) %>% sum() > 0){
    msg <- paste(msg,"- Make sure you have no special characters like (), [] or spaces in column names. This warning might also be due to duplicated column names.", sep = "<br/>")
  }
  # R places X in front of column names that start with a number or a special character
  suspicious <- dframe %>% colnames() %>% grep("^X",.) %>% sum()
  explained <- dframe %>% colnames() %>% grep("^X[[:alpha:]]",.) %>% sum() # These are just words that start with X
  if (suspicious > explained){
    msg <- paste(msg,"- Check if a column name starts with a number (or X[number])!", sep = "<br/>")
  }
  if (msg != ""){
    msg <- paste("Warnings:",msg,sep="")
  }
  return(msg)
}

# Warnings related to the metadata sheet
warnings_meta <- function(sample_dframe,meta_dframe){
  msg <- ""
  
  if( colnames(meta_dframe)[1] %>% toupper() != "VARIABLE"){
    msg <- paste(msg,"- The first column should be named \"VARIABLE\"",sep = "<br/>")
  }
  
  if( colnames(meta_dframe)[2] %>% toupper() != "DESCRIPTION"){
    msg <- paste(msg,"- The second column should be named \"DESCRIPTION\"",sep = "<br/>")
  }
  
  # Check that all the variables described are in column names of sample information file, not case sensitive
  condition <- toupper(meta_dframe$VARIABLE) %in% toupper(colnames(sample_dframe)) %>% all()
  if(!condition){
    msg <- paste(msg,"- All variables do not match the columns in sample information sheet!",sep = "<br/>")
  }
  extra_vars <- sample_dframe %>% colnames() %>% dplyr::setdiff(c("SAMPLE_ID,SUBJECT_ID,GROUP,TIME"))
  condition2 <- toupper(extra_vars) %in% toupper(meta_dframe$VARIABLE) %>% all()
  if(!condition2){
    msg <- paste(msg,"- Some extra variables lack description",sep="<br/>")
  }
  msg
}

# Get the number of variables and the number of variables which have description
info_variables <- function(sample_dframe,meta_dframe){
  msg <- paste("Found",as.character(ncol(sample_dframe)),"variables")
  described <- toupper(colnames(sample_dframe)) %in% toupper(meta_dframe$VARIABLE) %>% sum()
  msg <- paste(msg,"of which",as.character(described),"have description")
  msg
}

# Counts the number of groups and/or timepoints
count_group_time <- function(dframe){
  colnames(dframe) <- colnames(dframe) %>% toupper()
  msg <- ""
  if("GROUP" %in% colnames(dframe)){
   groups <- dframe$GROUP %>% base::unique() %>% length()
   msg <- paste("Number of groups:",as.character(groups))
  }
  if("TIME" %in% colnames(dframe)){
    times <- dframe$TIME %>% base::unique() %>% length()
    msg <- paste(msg,"Number of time points:",sep = "<br/>") %>% paste(as.numeric(times))
  }
  msg
}

# Generates datafile labels
# INPUT:
#       - folder: character
#       - project_title: character
#       - modes: character vector of modes run
#       - qc_begins: named list of numbers of QC samples in the beginning of each mode
#       - n_mod: number of samples excluding the QCs in the beginning
# OUTPUT: list of two types of datafile labels
#       - short: character vector of datafile labels without complete path and .d file ending
#       - long: character vector of datafile labels with complete path and .d file ending
generate_datafile_labels <- function(folder,project_title,modes,qc_begins,n_mod){
  labels <- c()
  longs <- c()
  
  if("HILIC_neg" %in% modes){
    hilic_neg_labels <- str_pad(as.character(1:(n_mod+qc_begins$hilicneg)),width=4, pad = "0") %>% paste(project_title,"HILIC_neg",.,sep = "_")
    labels <- c(labels,hilic_neg_labels)
    hilic_neg_long <- paste(folder,project_title,"_HILIC_neg\\", hilic_neg_labels,".d",sep="")
    longs <- c(longs,hilic_neg_long)
  }
  if("HILIC_pos" %in% modes){
    hilic_pos_labels <- str_pad(as.character(1:(n_mod+qc_begins$hilicpos)),width=4, pad = "0") %>% paste(project_title,"HILIC_pos",.,sep = "_")
    labels <- c(labels,hilic_pos_labels)
    hilic_pos_long <- paste(folder,project_title,"_HILIC_pos\\", hilic_pos_labels,".d",sep="")
    longs <- c(longs,hilic_pos_long)
  }
  if("RP_neg" %in% modes){
    rp_neg_labels <- str_pad(as.character(1:(n_mod+qc_begins$rpneg)),width=4, pad = "0") %>% paste(project_title,"RP_neg",.,sep = "_")
    labels <- c(labels,rp_neg_labels)
    rp_neg_long <- paste(folder,project_title,"_RP_neg\\", rp_neg_labels,".d",sep="")
    longs <- c(longs,rp_neg_long)
  }
  if("RP_pos" %in% modes){
    rp_pos_labels <- str_pad(as.character(1:(n_mod+qc_begins$rppos)),width=4, pad = "0") %>% paste(project_title,"RP_pos",.,sep = "_")
    labels <- c(labels,rp_pos_labels)
    rp_pos_long <- paste(folder,project_title,"_RP_pos\\", rp_pos_labels,".d",sep="")
    longs <- c(longs,rp_pos_long)
  }
  list(short = labels,long = longs)
}

# Get the position of the QC samples in XYZ-coordinates

# INPUT:
#     qc_char: character giving the position of the QC samples, e.g. "P2-H12"
#     position_type: the type of plate to use, "vial" for 54-vial plate or "well" for 96-well plate
#OUTPUT:
# list of xyz-coordinates
# x, y = position on plate
# z = plate number (1 or 2)
# These coordinates are used by generate_sample_positions
get_qc_positions <- function(qc_pos_chars,position_type){
  
  x <- rep(NA,length(qc_pos_chars))
  y <- x
  z <- x
  
  for (i in 1:length(qc_pos_chars)){
    qc_pos_char <- qc_pos_chars[[i]]
    qc_char <- strsplit(qc_pos_char,split = NULL) %>% unlist()
    len <- length(qc_char)
    
    if(grepl("^Vial ", qc_pos_char) & len == 6 & grepl("[1-6]",qc_char[6])){
      x[i] <- -1
      y[i] <- -1
      z[i] <- -1
      next
    }
    
    if(!len %in% c(5,6) | qc_char[1] != "P" | !grepl("[0-2]",qc_char[2]) | qc_char[3] != "-" |
       !grepl("[A-H]",qc_char[4]) | !grepl("[0-9]",qc_char[5])){
      stop(paste("Invalid QC sample position:",qc_pos_char))
    }
    if(len == 6 & position_type == "well"){
      if(!grepl("[0-2]",qc_char[6]) | as.numeric(paste0(qc_char[5],qc_char[6])) > 12){
        stop(paste("Invalid QC sample position:",qc_pos_char))
      }
    }
    if(position_type == "vial" & (len == 6 | !grepl("[A-F]",qc_char[4]))){
      stop(paste("Invalid QC sample position:",qc_pos_char))
    }
    if(len == 5){
      x[i] <- as.numeric(qc_char[5])
    }
    else{
      x[i] <- as.numeric(paste0(qc_char[5],qc_char[6]))
    }
    y[i] <- as.numeric(charToRaw(qc_char[4])) - 64
    z[i] <- as.numeric(qc_char[2])
  }
  
  return(data.frame(x,y,z))
}

# Generate sample positions for all the samples after initial QCs
# INPUT:
#     n: number of samples
#     position_type: the type of plate to use, "vial" for 54-vial plate or "well" for 96-well plate
#     qc_pos_char: character giving the position of the QC samples, e.g. "P2-H12"
#     all_qc_pos_chars: character vector of the QC sample positions for all the modes
#     qc_int: interval of the QC samples = number of samples between QC samples
# OUTPUT:
#     character vector of positions for worklist file
generate_sample_positions <- function(n, position_type, qc_pos_char, all_qc_pos_chars, qc_int){
  positions <- rep(NA,n)
  qc <- get_qc_positions(all_qc_pos_chars, position_type)
  
  # Set the size of the plate
  if(position_type == "well"){
    x_cut <- 13
    y_cut <- 9
  }
  else if(position_type == "vial"){
    x_cut <- 10
    y_cut <- 7
  }
  
  x <- 1
  y <- 1
  z <- 1
  
  while(any(x == qc$x & y == qc$y & z == qc$z)){ # skip the positions reserved for QC samples
    x <- x + 1
  }
  
  for(i in 1:n){
    if (i %% (qc_int+1) == 0){
      positions[i] <- qc_pos_char
    }
    else{
      positions[i] <- paste("P",as.character(z),"-",LETTERS[y],as.character(x),sep="")
      x <- x + 1
      while(any(x == qc$x & y == qc$y & z == qc$z)){ # skip the positions reserved for QC samples
        x <- x + 1
      }
      if(x >= x_cut){
        x <- 1
        y <- y + 1
        if(y == y_cut){
          y <- 1
          z <- z + 1
          if(z == 3){
            z <- 1
          }
        }
      }
    }
  }
  positions
}

# Randomize the samples, add QC and generate internal ID
# Generate worklist files with AutoMSMS and STOP
# INPUT:
#     dframe: data frame containing the sample information
#     project_title: character
#     save_code: Boolean indicating if the code should be saved
#     folder: character, path to project home folder
#     qc_int: interval of the QC samples = number of samples between QC samples
#     modes: character vector of modes run
#     qc_begins: named list of numbers of QC samples in the beginning of each mode
#     sample_order: "random_global", "random_group" or "original"
#     grouping column: grouping column name, NULL unless sample_order = "random_group"
#     position_type: the type of plate to use, "vial" for 54-vial plate or "well" for 96-well plate
#     qc_pos_chars: character vector of the QC sample positions for all the modes
#     second_column: character, name of the second column
modify_sample <- function(dframe, project_title, project_code, folder, qc_int, modes,
                          qc_begins, sample_order, grouping_column, position_type, qc_pos_chars, second_column){
  
  # set factor columns to character
  classes <- lapply(dframe, class) %>% unlist() %>% unname()
  for (i in 1:length(classes)){
    if(classes[i] == "factor"){
      dframe[,i] <- as.character(dframe[,i])
    }
  }
  
  # Randomize row order
  # seed generated from project title
  n <- nrow(dframe)
  set.seed( project_title %>% charToRaw() %>% as.numeric() %>% sum())
  if(sample_order == "random_global"){
    dframe_ord <- dframe[sample(n),]
  }
  else if(sample_order == "random_group"){
    dframe_ord <- dframe %>%
      group_by_(grouping_column) %>%
      sample_frac(1)
  }
  else if(sample_order == "original"){
    dframe_ord <- dframe
  }
  
  # Generate internal ID for non-QC samples
  dframe_ord$INTERNAL_SAMPLE_ID <- paste(project_code, 1:n, sep="_")
  
  # 
  if (!is.na(qc_int)){
    n_mod <- n + floor(n/(qc_int))
    dframe_mod <- matrix(NA,n_mod,ncol(dframe_ord)) %>% data.frame()
    colnames(dframe_mod) <- colnames(dframe_ord)
    j <- 0
    QC <- rep(NA,n_mod)
    
    # Add QC rows in the middle and create QC column
    for (i in 1:n_mod){
      if (i %% (qc_int+1) == 0){
        dframe_mod[i,] <- rep(NA,times = ncol(dframe_ord))
        QC[i] <- TRUE
        j <- j + 1
      }
      else{
        dframe_mod[i,] <- dframe_ord[i-j,]
        QC[i] <- FALSE
      }
    }
  }
  else{
    dframe_mod <- dframe_ord
    n_mod <- n
    QC <- rep(FALSE,n)
  }
  
  # Generate sample positions
  # Add QC rows to beginning and combine
  for(i in 1:length(modes)){
    dframe_tmp <- dframe_mod
    dframe_tmp$SAMPLE_POSITION <- generate_sample_positions(nrow(dframe_tmp),position_type,qc_pos_chars[[i]],qc_pos_chars,qc_int)
    QC_tmp <- c(rep(TRUE,qc_begins[[i]]),QC)
    dummyframe <- matrix(NA,qc_begins[[i]],ncol(dframe_tmp)) %>% data.frame()
    colnames(dummyframe) <- colnames(dframe_tmp)
    dframe_tmp <- rbind(dummyframe, dframe_tmp)
    dframe_tmp$RUN_ORDER <- 1:nrow(dframe_tmp)
    dframe_tmp$QC <- QC_tmp
    
    # Generate internal ID and sample position for QC samples
    j <- 0
    for(k in 1:(nrow(dframe_tmp))){
      if (dframe_tmp$QC[k]){
        j <- j + 1
        dframe_tmp$INTERNAL_SAMPLE_ID[k] <- paste("QC",as.character(j),sep="_")
        dframe_tmp$SAMPLE_POSITION[k] <- qc_pos_chars[[i]]
      }
    }
    
    if(i == 1){
      dframe_big <- dframe_tmp
    }
    else{
      dframe_big <- rbind(dframe_big,dframe_tmp)
    }
  }
  
  # Generate datafile labels
  datafile_labels <- generate_datafile_labels(folder,project_title,modes,qc_begins,n_mod)
  dframe_big$DATAFILE <- datafile_labels$short
  dframe_big$DATAFILE_LONG <-datafile_labels$long
  
  dframe_big <- dframe_big %>% dplyr::select(DATAFILE,RUN_ORDER,INTERNAL_SAMPLE_ID,SAMPLE_ID,QC,everything())
  index <- which(colnames(dframe_big) == second_column)
  dframe_big <- cbind(dframe_big[,c(1,index)],dframe_big[,-c(1,index)])
  
  # Separate dframe_big into MPP and worklist tables
  dframe_MPP <- dframe_big %>% dplyr::select(-DATAFILE_LONG,-SAMPLE_POSITION)
  dframe_worklist <- dframe_big %>% dplyr::select(INTERNAL_SAMPLE_ID,SAMPLE_POSITION,DATAFILE_LONG)
  
  return(list(MPP = dframe_MPP,worklist = dframe_worklist))
}

# Add AutoMSMS runs and METHOD column to worklist table
# INPUT:
#       dframe: data frame, worklist table
#       mode: the mode used
#       msms_qc: boolean, should AutoMSM be run on a QC sample
#       msms_sample_ids: internal sample IDs of the samples going to AutoMSMS
# OUTPUT:
#       data frame with AutoMSM added
add_auto_msms <- function(dframe, mode, msms_qc, msms_sample_ids){
  
  dframe$METHOD <- paste0(mode,".m")
  
  if(is.null(msms_qc) & is.null(msms_sample_ids)){
    return(dframe)
  }
  
  # Get the datafile path and number of the last datafile
  last_split <- dframe[nrow(dframe),]$DATAFILE_LONG %>% as.character() %>% strsplit("") %>% unlist()
  l <- length(last_split)
  df_number <- last_split[(l-5):(l-2)] %>% paste0(collapse="") %>% as.numeric()
  df_path <- last_split[-((l-5):l)] %>% paste0(collapse="")
  
  if(msms_qc){
    msms_sample_ids <- c(msms_sample_ids,"QC_1")
  }
  
  n_msms_samples <- length(msms_sample_ids)
  dframe_bottom <- matrix(NA,3*n_msms_samples+1,ncol(dframe)) %>% data.frame()
  colnames(dframe_bottom) <- colnames(dframe)
  
  msms_voltages <- c("10V","20V","40V")
  for (i in 1:n_msms_samples){
    position <- dframe[dframe$INTERNAL_SAMPLE_ID %in% msms_sample_ids[i],]$SAMPLE_POSITION %>% as.character()
    
    for (j in 1:3){
      row <- (i-1)*3 +j
      df_number <- df_number + 1
      dframe_bottom[row,]$INTERNAL_SAMPLE_ID <- paste(msms_sample_ids[i],"auto_msms",msms_voltages[j],sep="_")
      dframe_bottom[row,]$SAMPLE_POSITION <- position
      dframe_bottom[row,]$DATAFILE_LONG <- paste(df_path,str_pad(df_number, width = 4, pad = "0"),".d",sep="")
      dframe_bottom[row,]$METHOD <- paste0(mode,"_AutoMSMS_",msms_voltages[j],".m")
    }
  }
  
  dframe_bottom[nrow(dframe_bottom),]$INTERNAL_SAMPLE_ID <- "STOP"
  dframe_bottom[nrow(dframe_bottom),]$SAMPLE_POSITION <- "Vial 1"
  dframe_bottom[nrow(dframe_bottom),]$DATAFILE_LONG <- paste(df_path,str_pad(df_number + 1, width = 4, pad = "0"),".d",sep="")
  dframe_bottom[nrow(dframe_bottom),]$METHOD <- "STOP.m"
  
  dframe <- rbind(dframe, dframe_bottom) %>% select(INTERNAL_SAMPLE_ID, SAMPLE_POSITION, METHOD, DATAFILE_LONG)
  dframe
}

# Separates worklist table, adds AutoMSMS
# INPUT:
#       dframe: data frame with workllists of all the modes
#       modes: modes: character vector of modes run
#       msms_qc: boolean, should AutoMSM be run on a QC sample
#       msms_sample_ids: internal sample IDs of the samples going to AutoMSMS
# OUTPUT:
#       named list of worklist tables, names = modes
separate_worklists <- function(dframe, modes, msms_qc, msms_sample_ids){
  separated <- as.list(rep(NA,4))
  modeopts <- c("HILIC_neg","HILIC_pos","RP_neg","RP_pos")
  for(i in 1:4){
    if(modeopts[i] %in% modes){
      dframe_tmp <- dframe %>% dplyr::filter(grepl(modeopts[i],DATAFILE_LONG))
      dframe_tmp <- add_auto_msms(dframe_tmp, modeopts[i], msms_qc, msms_sample_ids)
      colnames(dframe_tmp) <- c("Sample Name","Sample Position","Method", "Data File")
      separated[[i]] <- dframe_tmp
      names(separated)[i] <- modeopts[i]
    }
  }
  separated
}

# ----- MPP tab --------

# Create warnings based on processes sample info file
warnings_sample_processed <- function(dframe){
  msg <- ""
  incProgress(amount = 0.5, message = "Checking file")
  if(!"INTERNAL_SAMPLE_ID" %in% colnames(dframe)){
    msg <- "INTERNAL_SAMPLE_ID column missing"
  }
  if(!"DATAFILE" %in% colnames(dframe)){
    msg <- paste(msg,"DATAFILE column missing",sep = "<br/>")
  }

  return(msg)
}

# Count non-QC and QC samples in processes sample info file
get_sample_counts <- function(dframe){
  incProgress(amount = 0.2, message = "Counting samples")
  orig_n <- dframe %>% dplyr::filter(!QC) %>% nrow()
  qc_n <- dframe %>% dplyr::filter(QC) %>% nrow()
  msg <- paste("Sample information file OK",paste("Found",orig_n,"original samples and",qc_n,"QC samples"),sep="<br/>")
}

# Load data from MPP file
load_metabo_data <- function(file, scan.lines=20) {
  
  # Skip comment lines starting with #
  tmp.data <- scan(file=file, what="character", nlines=scan.lines, sep="\n", quiet = T)
  index <- grep("^#", tmp.data)
  rm(tmp.data)
  if (length(index) > 0) {
    skip.lines <- max(index)
  } else {
    skip.lines <- 0
  }
  
  if (skip.lines >= scan.lines) {
    stop("Found only header lines.")
  }
  
  data <- read.table(file, header=TRUE, skip=skip.lines, sep = "\t")
  data
  
}

# Extract metabolite compound raw data from the full raw data set
extract_compound_data <- function(data, analysis_mode,sample_dframe) {
  index <- which(colnames(data) == "Compound")
  index <- c(index, grep(".raw.$", colnames(data)))
  
  compound.data <- data[,index]
  
  # Clean sample names
  colnames(compound.data) <- gsub(".raw.$", "", colnames(compound.data))
  
  # Clean metabolite names
  compound.data$Compound <- gsub("@", "a", compound.data$Compound)
  
  # Replace any non-alphanumeric characters with _
  compound.data$Compound <- gsub("[^[:alnum:]]+", "_", compound.data$Compound)
  #compound.data$Compound <- gsub(" ", "_", compound.data$Compound)
  #compound.data$Compound <- gsub("[:punct:]", "_", compound.data$Compound)
  compound.data$Compound <- paste("COMPOUND", compound.data$Compound, analysis_mode, sep="_")
  
  # Transpose data
  compound.data.t <- t(compound.data[,-1])
  colnames(compound.data.t) <- compound.data$Compound
  
  # Add DATAFILE column
  # Match INTERNAL_SAMPLE_ID
  result.df <- cbind(data.frame(DATAFILE=rownames(compound.data.t)), compound.data.t)
  tmp <- sample_dframe %>% select(DATAFILE, INTERNAL_SAMPLE_ID)
  if(!all(result.df$DATAFILE %in% tmp$DATAFILE)){
    stop(paste("Datafiles in MPP file",analysis_mode,"not found in sample information file"))
  }
  result.df$DATAFILE <- as.character(result.df$DATAFILE)
  tmp$DATAFILE <- as.character(tmp$DATAFILE)
  result.df <- left_join(result.df,tmp, by = "DATAFILE") %>% select(INTERNAL_SAMPLE_ID,everything(),-DATAFILE)
  
  result.df
}

# Extract annotations from the full raw data set
extract_annotations <- function(data){
  index <- which(colnames(data) == "Compound")
  index <- c(index, grep(".raw.$", colnames(data)))
  as.data.frame(data[,-index])
}

# Combine abundance values into one data matrix
# Split the information output of MPP into two files:
# INPUT:
#     mpp_files: named list of the uploaded MPP tables, names = modes
#     modes: character vector of the modes run
#     sample_dframe: data frame read from processes sample info file
# OUTPUT: list
#     - abundance data matrix
#     - annotation data frame with the other columns
combine_mpp <- function(mpp_files,modes,sample_dframe){
  
  if(length(mpp_files) > 1){
    for(i in 2:length(mpp_files)){
      if(ncol(mpp_files[[i-1]]) != ncol(mpp_files[[i]])){
        stop("MPP files have different amount of datafiles")
      }
    }
  }
  
  
  # Clean compound names, add analysis mode in the end
  data_matrices <- as.list(rep(NA,length(mpp_files)))
  annotations <- data.frame()
  for (i in 1:length(mpp_files)){
    if(i ==1){
      incProgress(0.125,message = paste("Processing",modes[i],"file"))
    }
    else{
      incProgress(0.1,message = paste("Processing",modes[i],"file"))
    }
    data_matrices[[i]] <- extract_compound_data(mpp_files[[i]],modes[i],sample_dframe)
    annotations_tmp <- mpp_files[[i]] %>% select(Compound,Mass,Retention.Time,CompositeSpectrum,Frequency,CompoundAlgo,Ionization.mode)
    annotations_tmp$Compound <- gsub("@", "a", annotations_tmp$Compound)
    annotations_tmp$Compound <- gsub("[^[:alnum:]]+", "_", annotations_tmp$Compound)
    annotations_tmp$Compound <- paste("COMPOUND", annotations_tmp$Compound, modes[i], sep="_")
    annotations <- rbind(annotations,annotations_tmp)
  }
  incProgress(0.1,message = "Combining files")
  
  data_matrix_joined <- data_matrices[[1]]
  if(length(mpp_files) > 1){
    for(i in 2:length(data_matrices)){
      data_matrix_joined <- left_join(data_matrix_joined,data_matrices[[i]], by = "INTERNAL_SAMPLE_ID")
    }
  }
  sample_dframe <- sample_dframe %>% select(-DATAFILE) %>% distinct()
  data_matrix_joined <- right_join(sample_dframe,data_matrix_joined,by="INTERNAL_SAMPLE_ID")
  
  # Create chromatography column
  chromatography <- c()
  if("HILIC_neg" %in% modes){
    chromatography <- c(chromatography,rep("hilic", nrow(mpp_files$hilic_neg)))
  }
  if("HILIC_pos" %in% modes){
    chromatography <- c(chromatography,rep("hilic", nrow(mpp_files$hilic_pos)))
  }
  if("RP_neg" %in% modes){
    chromatography <- c(chromatography,rep("rp", nrow(mpp_files$rp_neg)))
  }
  if("RP_pos" %in% modes){
    chromatography <- c(chromatography,rep("rp", nrow(mpp_files$rp_pos)))
  }
  annotations$Chromatography <- chromatography
  
  
  return(list(annotations = annotations,data_matrix = data_matrix_joined))
}
