check_dependencies <- function() {

  pckgs <- c("dplyr", "tidyr", "shiny", "openxlsx", "stringr")
  miss <- c()
  for (pckg in pckgs){
    if(!requireNamespace(pckg, quietly = TRUE)) {
      miss <- c(miss, pckg)
    }
  }
  if (length(miss)) {
    stop(paste("The following packages are needed for this app to work:", paste(miss, collapse = ", "), ". Please install them."), .call = FALSE)
  }
}


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

get_default_project_code <- function(project_title){
  saved_codes <- read.csv("project_codes.csv")
  
  if(project_title %in% saved_codes$Project){ # Use existing code
    index <- which(saved_codes$Project == project_title)
    project_code <- saved_codes$Code[index]
  }
  else{ # Generate new code
    # Generate the possible choices for the code, first capital letters, then combinations of two capital letters
    two_letter_df <- expand.grid(LETTERS, LETTERS, stringsAsFactors = FALSE) %>% unite("Code", Var2, Var1, sep = "")
    choices <- c(LETTERS, two_letter_df$Code)
    # Pick the first possible code not already included in the saved project codes
    project_code <- choices[min(which(!choices %in% saved_codes$Code))]
  }
  project_code
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
get_qc_positions <- function(qc_pos_chars, n_plates, position_type){
  
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
    
    if(!len %in% c(5,6) | qc_char[1] != "P" | !grepl(paste0("[0-", n_plates, "]"),qc_char[2]) | qc_char[3] != "-" |
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
generate_sample_positions <- function(n, n_plates, position_type, qc_pos_char, all_qc_pos_chars, qc_int){
  positions <- rep(NA,n)
  qc <- get_qc_positions(all_qc_pos_chars, n_plates, position_type)
  
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
          if(z == n_plates+1){
            z <- 1
          }
        }
      }
    }
  }
  positions
}

save_project_code <- function(project_title, project_code, save_code){
  saved_codes <- read.csv("project_codes.csv")
  
  newrow <- data.frame(Project = project_title, Code = project_code)
  if(save_code & !project_title %in% saved_codes$Project){ # Use existing code
    write.table(newrow,"project_codes.csv", append = T, row.names = F, col.names = F, sep = ",", quote = F , eol = "\r\n")
  }
  write.table(newrow,"project_log.csv", append = T, row.names = F, col.names = F, sep = ",", quote = F , eol = "\r\n")
}

# Randomize the samples, add QC and generate internal ID
# Generate worklist files with AutoMSMS and STOP
# INPUT:
#     dframe: data frame containing the sample information
#     project_title: character
#     project_code: Character concatenated in the beginning of every internal sample ID
#     n_plates: the number of plates available
#     folder: character, path to project home folder
#     qc_int: interval of the QC samples = number of samples between QC samples
#     modes: character vector of modes run
#     qc_begins: named list of numbers of QC samples in the beginning of each mode
#     sample_order: "random_global", "random_group" or "original"
#     grouping column: grouping column name, NULL unless sample_order = "random_group"
#     position_type: the type of plate to use, "vial" for 54-vial plate or "well" for 96-well plate
#     qc_pos_chars: character vector of the QC sample positions for all the modes
#     second_column: character, name of the second column
modify_sample <- function(dframe, project_title, project_code, save_code, folder, n_plates, qc_int, modes,
                          qc_begins, sample_order, grouping_column, include_subject_id, subject_id_column,
                          position_type, qc_pos_chars){
  # Save project title and code
  save_project_code(project_title, project_code, save_code)
  
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
    # If subject id is included, randomize the subject IDs and run all the sampels of one subject in a sequence
    if(include_subject_id){
      subject_order <- sample(unique(dframe[,subject_id_column]))
      dframe[subject_id_column] <- factor(dframe[,subject_id_column], levels = subject_order)
      dframe_ord <- dframe %>%
        group_by_(subject_id_column) %>%
        sample_frac(1) %>%
        ungroup() %>%
        as.data.frame()
      dframe_ord[subject_id_column] <- as.character(dframe_ord[,subject_id_column])
    } else { # Completely randomize the data frame
      dframe_ord <- dframe[sample(n),]
    }
  } else if(sample_order == "random_group") {
    # Randomize subjects inside every group, run all the sample of one sunject in a sequence
    if(include_subject_id){
      subject_order <- sample(unique(dframe[,subject_id_column]))
      dframe[subject_id_column] <- factor(dframe[,subject_id_column], levels = subject_order)
      dframe_ord <- dframe %>%
        group_by_(grouping_column, subject_id_column) %>%
        sample_frac(1) %>%
        ungroup() %>%
        as.data.frame()
      dframe_ord[subject_id_column] <- as.character(dframe_ord[,subject_id_column])
    } else { # Randomize completely inside one group
      dframe_ord <- dframe %>%
        group_by_(grouping_column) %>%
        sample_frac(1) %>%
        as.data.frame()
    }
  } else if(sample_order == "original"){
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
  } else {
    dframe_mod <- dframe_ord
    n_mod <- n
    QC <- rep(FALSE,n)
  }
  
  # Generate sample positions
  # Add QC rows to beginning and combine
  for(i in 1:length(modes)){
    dframe_tmp <- dframe_mod
    dframe_tmp$SAMPLE_POSITION <- generate_sample_positions(nrow(dframe_tmp), n_plates, position_type, qc_pos_chars[[i]], qc_pos_chars, qc_int)
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
  
  # Separate dframe_big into modified sample information and worklist tables
  dframe_samples <- dframe_big %>% dplyr::select(-DATAFILE_LONG,-SAMPLE_POSITION)
  dframe_worklist <- dframe_big %>% dplyr::select(INTERNAL_SAMPLE_ID,SAMPLE_POSITION,DATAFILE_LONG)

  return(list(samples = dframe_samples, worklist = dframe_worklist))
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

#' Summarize data
#'
#' @description Summarize and check \code{data.frame} for potential data errors.
#'
#' @param data a data.frame to process
#' @param parallel logical flag indicating if the tasks should be run in parallel. The default value is \code{FALSE}.
#' @param row.checks row specific checks to run.
#' @param col.checks column specific checks to run.
#'
#' @return An object of class "summarized.data" with the following components: (TBA)
#'
#' @export summarize_data
#'
#' @examples
#' summarize_data(iris)
#'
#' # Add values that will cause warnings in checks
#' tmp <- iris
#' tmp[3,4] <- " "
#' tmp[4,2] <- ""
#' tmp[1,2] <- NA
#' tmp[8,2] <- " test"
#' tmp[1,2] <- Inf
#' tmp[1,4] <- NaN
#' tmp[18,1] <- "#NAME?"
#'
#' summarize_data(tmp)

summarize_data <- function(data, row.checks=c("duplicated_row"), col.checks=c("na", "infinite", "nan", "empty_string", "whitespace", "leading_or_trailing_whitespace", "byte_sequence_character", "unicode_replacement_character", "linebreak", "excel_formula_error", "comma_as_decimal_separator", "duplicated_column"), parallel=FALSE) {
  
  # Toggle for logging/availability of futile.logger package
  f_logging <- requireNamespace("futile.logger", quietly = TRUE)
  
  if (f_logging) futile.logger::flog.debug("Initializing summary analysis.")
  
  # Select serial or parallel operator for foreach-function
  if (parallel) {
    `%do_operator%` <- foreach::"%dopar%"
  } else {
    `%do_operator%` <- foreach::"%do%"
  }
  
  classes <- sapply(X=data, FUN=class)
  classes_df <- as.data.frame(table(classes))
  class_freq <- classes_df$Freq
  names(class_freq) <- classes_df$classes
  
  col.checks.result <- foreach::foreach (i=1:length(col.checks), .combine=c, .inorder=FALSE) %do_operator% {
    check <- col.checks[i]
    if (f_logging) futile.logger::flog.debug(paste0("Column check: ", check))
    
    if (check == "na") {
      result <- list(na=which(colSums(sapply(X=data, FUN=function(x) { is.na(x) })) > 0))
    } else if (check == "infinite") {
      result <-  list(infinite=which(colSums(sapply(X=data, FUN=function(x) { !is.na(x) & x == Inf })) > 0))
    } else if (check == "nan") {
      result <-  list(nan=which(colSums(sapply(X=data, FUN=function(x) { !is.na(x) & is.nan(x) })) > 0))
    } else if (check == "empty_string") {
      result <- list(empty_string=which(colSums(sapply(X=data, FUN=function(x) { !is.na(x) & x == "" })) > 0))
    } else if (check == "whitespace") {
      result <- list(whitespace=which(sapply(X=data, FUN=function(x) { length(grep("^[[:space:]]+$", x))}) > 0))
    } else if (check == "comma_as_decimal_separator") {
      result <- list(comma_as_decimal_separators=which(sapply(X=data, FUN=function(x) { length(grep("(^[0-9]+,[0-9]*$)|(^[0-9]*,[0-9]+$)", x))}) > 0))
    } else if (check == "leading_or_trailing_whitespace") {
      result <- list(leading_or_trailing_whitespace=which(sapply(X=data, FUN=function(x) { length(grep("(^\\s+\\S+)|(\\S+\\s+$)", x, perl=TRUE))}) > 0))
    } else if (check == "byte_sequence_character") {
      result <- list(byte_sequence_character=which(sapply(X=data, FUN=function(x) { length(grep("[\\]x", x))}) > 0))
    } else if (check == "unicode_replacement_character") {
      result <- list(unicode_replacement_character=which(sapply(X=data, FUN=function(x) { length(grep("\xEF\xBF\xBD", x))}) > 0))
    } else if (check == "linebreak") {
      result <- list(linebreak=which(sapply(X=data, FUN=function(x) { length(grep("(\r)|(\n)", x))}) > 0))
    } else if (check == "excel_formula_error") {
      result <- list(excel_formula_error=which(sapply(X=data, FUN=function(x) { length(grep("^((#DIV/0!)|(#N/A)|(#NAME\\?)|(#NULL!)|(#NUM!)|(#REF!)|(#VALUE!)|(#GETTING_DATA))$", x))}) > 0))
    } else if (check == "duplicated_column") {
      dupli <- which(duplicated(as.list(data)))
      names(dupli) <- colnames(data)[dupli]
      result <- list(duplicated_columns=dupli)
    }
    if (f_logging) futile.logger::flog.trace(paste0("- Check result: ", result))
    
    result
  }
  
  if (f_logging) futile.logger::flog.debug("Starting row checks.")
  row.checks.result <- foreach::foreach (i=1:length(row.checks), .combine=c, .inorder=FALSE) %do_operator% {
    check <- row.checks[i]
    if (f_logging) futile.logger::flog.debug(paste0("Row check: ", check))
    
    if (check == "duplicated_row") {
      dupli <- which(duplicated(data))
      names(dupli) <- rownames(data)[dupli]
      result <- list(duplicated_row=dupli)
      
    }
    if (f_logging) futile.logger::flog.trace(paste0("- Check result: ", result))
    
    result
  }
  
  structure(list(dimensions=dim(data), classes=classes, class_freq=class_freq, col.checks=col.checks.result, row.checks=row.checks.result), class="summarized.data")
}

#' Printing summarize_data
#'
#' @description Print a \code{summarized.data} object.
#'
#' @usage
#' ## S3 method for class 'summarized.data'
#'
#' @param x object of class \code{summarize_data}.
#' @param ... further arguments passed to or from other methods.
#'
#' @export print.summarized.data
#'
#' @seealso \code{\link{summarize_data}}
#'
#' @examples
#' print(summarize_data(iris))
#'
print.summarized.data <- function(x, ...) {
  cat(paste0("Dimensions: ",  x$dimensions[1], " (rows) x ", x$dimensions[2], " (cols). Total observations: ", x$dimensions[1] * x$dimensions[2], "\n"))
  cat(paste0("Column classes:\n - ", paste0(names(x$class_freq), ": ", x$class_freq, collapse=", "), "\n"))
  
  # Column warnings
  warning_text_displayed <- FALSE
  for (name in names(x$col.checks)) {
    warning_hits <- x$col.checks[[name]]
    if (length(warning_hits) > 0) {
      if (!warning_text_displayed) {
        cat("Warnings (amount of columns with possible errors):\n")
        warning_text_displayed <- TRUE
      }
      cat(paste0(" - ", name, ": ", length(warning_hits), collapse=" "), "\n")
    }
  }
  
  # Row warnings
  warning_text_displayed <- FALSE
  for (name in names(x$row.checks)) {
    warning_hits <- x$row.checks[[name]]
    if (length(warning_hits) > 0) {
      if (!warning_text_displayed) {
        cat("Warnings (amount of rows with possible errors):\n")
        warning_text_displayed <- TRUE
      }
      cat(paste0(" - ", name, ": ", length(warning_hits), collapse=" "), "\n")
    }
  }
}