library(openxlsx)
library(bbd)
library(dplyr)
library(stringr)

source("functions.R")

# Set the maximum size of uploaded files to 30MB
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input,output){
  
  # Check that the input sample list file can be read correctly
  output$format_check <- renderUI({
    if (is.null(input$sample_input)){
      return(NULL)
    }
    datafile <- try(read.xlsx(input$sample_input$datapath, sheet = 1))
    if(class(datafile)=="try-error"){
      return(HTML('<p style = "color: red;"> The file should be in .xlsx format </p>'))
    }
    else{
      return("")
    }
  })
  
  # Read the input sample list file
  sample_dframe <- reactive({
    if (is.null(input$sample_input)){
      return(NULL)
    }
    datafile <- try(read.xlsx(input$sample_input$datapath, sheet = 1))
    if(class(datafile)=="try-error"){
      return(NULL)
    }
    datafile
  })
  
  # Validate that the sample information sheet matches the required format
  sample_warnings <- reactive({
    if (is.null(sample_dframe())){
      return(NULL)
    }
    warnings <- warnings_sample(sample_dframe())
    return(warnings)
  })
  
  # Ouput possible warnings about the sample information sheet
  output$sample_warnings <- renderUI({
    if (is.null(sample_warnings())){
      return(NULL)
    }
    if(sample_warnings() == ""){
      return(p("Sample information sheet OK"))
    }
    paste("<p style=\"color: red;\">",sample_warnings(),"</p>",sep="") %>% HTML()
  })
  
  # Count the number of groups and/or timepoints in the sample info sheet
  output$group_time <- renderUI({
    if(is.null(sample_dframe())){
      return(NULL)
    }
    msg <- count_group_time(sample_dframe())
    if(is.null(msg)){
      return(NULL)
    }
    tagList(
      HTML(msg)
    )
  })
  
  # Print the output of bbd::summarize_data
  output$summarize_data <- renderUI({
    if (is.null(sample_dframe())){
      return(NULL)
    }
    smry <- capture.output(summarize_data(sample_dframe()))
    out_text <- ""
    for (i in 1:length(smry)){
      out_text <- paste(out_text,smry[i],sep = "<br/>")
    }
    out_text %>% HTML()
  })
  
  # Read the metadata sheet from sample info file
  sample_meta_dframe <- reactive({
    if(is.null(input$sample_input)){
      return(NULL)
    }
    datafile <- try(read.xlsx(input$sample_input$datapath, sheet = 1))
    if(class(datafile)=="try-error"){
      return(NULL)
    }
    if (names(loadWorkbook(input$sample_input$datapath)) %>% length() < 2){
      return(NULL)
    }
    read.xlsx(input$sample_input$datapath, sheet = 2)
  })
  
  # Project home folder input
  # defaults to "D:\\MassHunter\\Data\\[project_title]"
  output$destination <- renderUI({
    if(input$project_title != ""){
      textInput("folder","Project home folder",
                value= paste("D:\\MassHunter\\Data\\",input$project_title,"\\",sep=""))
    }
    else{
      textInput("folder","Project home folder",
                value= "D:\\MassHunter\\Data\\")
    }
  })
  
  output$subject_id_column_choice <- renderUI({
    tagList(
      checkboxInput("include_subject_id", "Run samples from same subject in a sequence"),
      conditionalPanel(condition = "input.include_subject_id",
                       selectInput("subject_id_column","Subject ID column",
                                   choices = colnames(sample_dframe())))
    )
  })
  
  output$grouping_column_choice <- renderUI({
    selectInput("grouping_column","Column containing groups for randomization",
                choices = colnames(sample_dframe()))
  })
  
  # Choices for QC positions
  # All default to last position on the second plate
  output$qc_poschars <- renderUI({
    if(input$sample_position_type == "well"){
      qc_def <- paste0("P", input$n_plates, "-H12")
    }
    else{
      qc_def <- paste0("P", input$n_plates, "-F9")
    }
    tagList(
      fluidRow(
        conditionalPanel(condition = "input.modes.includes('HILIC_neg')",
                         column(3,
                                textInput("qc_poschar_hilicneg","HILIC neg",
                                          value = qc_def))),
        conditionalPanel(condition = "input.modes.includes('HILIC_pos')",
                         column(3,
                                textInput("qc_poschar_hilicpos","HILIC pos",
                                          value = qc_def))),
        conditionalPanel(condition = "input.modes.includes('RP_neg')",
                         column(3,
                                textInput("qc_poschar_rpneg","RP neg",
                                          value = qc_def))),
        conditionalPanel(condition = "input.modes.includes('RP_pos')",
                         column(3,
                                textInput("qc_poschar_rppos","RP pos",
                                          value = qc_def)))
      )
    )
  })
  
  # Choice of samples for AutoMSMS
  # values from original SAMPLE_ID column
  output$msms_samples <- renderUI({
    selectizeInput("msms_samples_choice","Choose samples for AutoMSMS",
                   choices = sample_dframe()$SAMPLE_ID, multiple = TRUE)
  })
  
  # Coice of second column for MPP
  output$second_column <- renderUI({
    selectInput("second_column_choice","Second column for MPP",
                choices = c(colnames(sample_dframe()),"QC"),
                selected = default_column())
  })
  
  # Default column for MPP
  # GROUP if found, QC otherwise
  default_column <- reactive({
    if(is.null(sample_dframe())){
      return(NULL)
    }
    cnames <- toupper(colnames(sample_dframe()))
    if("GROUP" %in% cnames){
      return(colnames(sample_dframe())[which(cnames == "GROUP")])
    }
    return("QC")
  })
  
  # Read the number of leading QC samples for each mode
  # output: a named list, names = modes
  qc_begins <- reactive({
    begins <- list()
    if("HILIC_neg" %in% input$modes){
      begins$hilicneg <- input$qc_begin_hilicneg
    }
    if("HILIC_pos" %in% input$modes){
      begins$hilicpos <- input$qc_begin_hilicpos
    }
    if("RP_neg" %in% input$modes){
      begins$rpneg <- input$qc_begin_rpneg
    }
    if("RP_pos" %in% input$modes){
      begins$rppos <- input$qc_begin_rppos
    }
    begins
  })
  
  # Read the position of QC samples for each mode
  # output: a named list, names = modes
  qc_pos_chars <- eventReactive(input$modify_sample,{
    poschars <- list()
    if("HILIC_neg" %in% input$modes){
      poschars$HILIC_neg <- input$qc_poschar_hilicneg
    }
    if("HILIC_pos" %in% input$modes){
      poschars$HILIC_pos <- input$qc_poschar_hilicpos
    }
    if("RP_neg" %in% input$modes){
      poschars$RP_neg <- input$qc_poschar_rpneg
    }
    if("RP_pos" %in% input$modes){
      poschars$RP_pos <- input$qc_poschar_rppos
    }
    poschars
  })
  
  # Randomize the samples, add QC and generate internal ID
  # Generate unseparated worklist file
  # OUTPUT: list of 2 data frames
  #       - MPP: table for MPP and data analysis
  #       - worklist: a data frame containing the worklists for all the modes
  sample_modified <- eventReactive(input$modify_sample,{
    if(is.null(sample_dframe()) | input$project_title == "" | input$project_code == "" | !grepl('^[A-Za-z0-9_.-]+$', input$project_title)){
      return(NULL)
    }
    if (!is.null(sample_warnings()) & sample_warnings() != ""){
      return(NULL)
    }
    modify_sample(sample_dframe(), input$project_title, input$project_code, input$folder, as.numeric(input$n_plates), as.numeric(input$qc_int), input$modes, qc_begins(),
                  input$sample_order, input$grouping_column, input$include_subject_id, input$subject_id_column,
                  input$sample_position_type, qc_pos_chars(), input$second_column_choice)
  })
  
  # Separate the worklist files
  # Add AutoMSMS and STOP to the end of file
  separated_worklist <- reactive({
    if(is.null(sample_modified())){
      return(NULL)
    }
    samples <- sample_modified()$MPP
    indx <- which(as.character(samples$SAMPLE_ID) %in% input$msms_samples_choice)
    msms_sample_ids <- samples$INTERNAL_SAMPLE_ID[indx] %>% unique()
    separate_worklists(sample_modified()$worklist,input$modes, input$msms_qc, msms_sample_ids)
  })
  
  output$sample_mod_info <- renderUI({
    sample_modified_info()
  })
  
  # Check that everything is OK
  # Process the sample information file
  # Create display for MPP file and worklist file
  # Create download buttons
  sample_modified_info <-eventReactive(input$modify_sample,{
    if(is.null(sample_dframe())){
      return(p("Please input a sample information file",style = "color:red;"))
    }
    if ((!is.null(sample_warnings()) & sample_warnings() != "")){
      return(p("The sample information sheet is not valid!",style = "color:red;"))
    }
    if (input$project_title == ""){
      return(p("Please input a project title",style = "color:red;"))
    }
    if (input$project_code == ""){
      return(p("Please input a project code",style = "color:red;"))
    }
    if(!grepl('^[A-Za-z0-9_.-]+$', input$project_title)){
      return(p("Only alphanumeric characters allowed in project title (no umlauts)",style = "color:red;"))
    }
    return(tagList(
      uiOutput("randomize_columns_warnings"),
      h4("Preview:"),
      radioButtons("table_choice",NULL,
                   choices = c("Processed sample information file for MPP" = "MPP","Worklist file" = "worklist")),
      uiOutput("sample_mod_table"),
      fluidRow(
        column(4,
               strong("Download processed sample info sheet:"),
               downloadButton("sample_mod_down_button")),
        column(4,
               strong("Download files for Worklist:"),
               uiOutput("worklist_downloads"))
      )
    ))
  })
  
  sample_warnings <- reactive({
    if (is.null(sample_dframe())){
      return(NULL)
    }
    warnings <- warnings_sample(sample_dframe())
    return(warnings)
  })
  
  # Ouput possible warnings about the sample information sheet
  output$sample_warnings <- renderUI({
    if (is.null(sample_warnings())){
      return(NULL)
    }
    if(sample_warnings() == ""){
      return(p("Sample information sheet OK"))
    }
    paste("<p style=\"color: red;\">",sample_warnings(),"</p>",sep="") %>% HTML()
  })
  
  randomize_column_warning_msg <- eventReactive(input$modify_sample, {
    if (is.null(sample_dframe()) | (input$sample_order %in% c("original", "random_global") & !input$include_subject_id)){
      return(NULL)
    }
    msg <- ""
    if (input$sample_order == "random_group"){
      if (sum(is.na(sample_dframe()[input$grouping_column])) > 0) {
        msg <- paste(msg, "Warning: Group column contains NAs")
      }
    }
    if(input$include_subject_id){
      if (sum(is.na(sample_dframe()[input$subject_id_column])) > 0) {
        if(msg != ""){
          msg <- paste0(msg, "<br/>")
        }
        msg <- paste(msg, "Warning: Subject ID column contains NAs")
      }
    }
    return(msg)
  })
  
  # Check whether the grouping and/or subject ID columns contain NAs
  output$randomize_columns_warnings <-renderUI({
    msg <- randomize_column_warning_msg()
    paste("<p style=\"color: red;\">",msg,"</p>",sep="") %>% HTML()
  })
  
  # dataTableOutput of either MPP file or the worklist file
  output$sample_mod_table <- renderUI({
    if(input$table_choice == "MPP"){
      dataTableOutput("sample_mod")
    }
    else{
      dataTableOutput("sample_mod_worklist")
    }
  })
  
  # First worklist file
  output$sample_mod_worklist <- renderDataTable({
    separated_worklist()[[1]]
  })
  
  # MPP file
  output$sample_mod <- renderDataTable({
    sample_modified()$MPP
  })
  
  # Download button for MPP file
  output$sample_mod_down_button <- downloadHandler(
    filename = paste(input$project_title,"sample_info_processed.csv",sep = "_"),
    
    content = function(file) {
      write.csv(sample_modified()$MPP,file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  # Download buttons for worklist files
  output$worklist_downloads <- renderUI({
    tagList(
      fluidRow(
        conditionalPanel(condition = "input.modes.includes('HILIC_neg')",
                         column(4,
                                downloadButton("worklist_hilic_neg_download","HILIC neg file"))),
        conditionalPanel(condition = "input.modes.includes('HILIC_pos')",
                         column(4,offset = 1,
                                downloadButton("worklist_hilic_pos_download","HILIC pos file")))),
      br(),
      fluidRow(
        conditionalPanel(condition = "input.modes.includes('RP_neg')",
                         column(4,
                                downloadButton("worklist_rp_neg_download","RP neg file"))),
        conditionalPanel(condition = "input.modes.includes('RP_pos')",
                         column(4,offset = 1,
                                downloadButton("worklist_rp_pos_download","RP pos file")))
      )
    )
  })
  
  output$worklist_hilic_neg_download <- downloadHandler(
    filename = paste(input$project_title,"worklist_hilic_neg.csv",sep = "_"),
    
    content = function(file) {
      write.csv(separated_worklist()$HILIC_neg,file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  output$worklist_hilic_pos_download <- downloadHandler(
    filename = paste(input$project_title,"worklist_hilic_pos.csv",sep = "_"),
    
    content = function(file) {
      write.csv(separated_worklist()$HILIC_pos,file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  output$worklist_rp_neg_download <- downloadHandler(
    filename = paste(input$project_title,"worklist_rp_neg.csv",sep = "_"),
    
    content = function(file) {
      write.csv(separated_worklist()$RP_neg,file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  output$worklist_rp_pos_download <- downloadHandler(
    filename = paste(input$project_title,"worklist_rp_pos.csv",sep = "_"),
    
    content = function(file) {
      write.csv(separated_worklist()$RP_pos,file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  # |--------------------------------- MPP tab ------------------------------------------------------------------|
  
  # Project title input
  # Defaults to project title on the first page
  output$project_title_mirror <- renderUI({
    textInput("project_title_mpp","Project Title", value = input$project_title)
  })
  
  # Read the processed sample info file
  sample_dframe_processed <- reactive({
    if (is.null(input$sample_input_mpp)){
      return(NULL)
    }
    incProgress(message = "Reading file")
    read.csv(input$sample_input_mpp$datapath, h = T)
  })
  
  # Create warnings for the processed sample info file
  sample_processed_warnings <- reactive({
    if(is.null(sample_dframe_processed())){
      return(NULL)
    }
    warnings_sample_processed(sample_dframe_processed())
  })
  
  # Display warnings
  # If no warnings are found, display the number of samples found
  output$sample_processed_warnings <- renderUI({
    
    withProgress(message = "Checking sample information file", value = 0.2,{
      if(is.null(sample_processed_warnings())){
        return(NULL)
      }
      if(sample_processed_warnings() == ""){
        return(HTML(get_sample_counts(sample_dframe_processed())))
      }
      paste("<p style=\"color:red;\">",sample_processed_warnings(),"</p>") %>% HTML()
    })
  })
  
  # Check that everything is OK
  # If yes, display the number of compounds in each mode
  mpp_combine_message <- eventReactive(input$combine_mpp,{
    if(is.null(sample_dframe_processed())){
      return(p("Please input sample information file",style="color:red;"))
    }
    if(sample_processed_warnings() != ""){
      return(p("The sample information file is not valid!",style="color:red;"))
    }
    if(is.null(input$hilic_neg) & is.null(input$hilic_pos) & is.null(input$rp_neg) & is.null(input$rp_pos)){
      return(p("Please input at least one MPP output file",style="color:red;"))
    }
    if(input$project_title_mpp == ""){
      return(p("Please input a project title",style="color:red;"))
    }
    msg <- "Compounds found:"
    for(i in 1:length(mpp_loaded()$modes)){
      tmp <- paste(mpp_loaded()$modes[i],":",nrow(mpp_loaded()$files[[i]]))
      msg <- paste(msg,tmp,sep="<br/>")
    }
    p(HTML(msg))
  })
  
  output$mpp_combine_msg <- renderUI({
    mpp_combine_message()
  })
  
  # Read the uploaded MPP files
  # OUTPUT: list
  #       - files: named list of data matrices from MPP, names = modes
  #       - modes: the modes for which an MPP file was uplpoaded
  mpp_loaded <- reactive({
    if(is.null(input$hilic_neg) & is.null(input$hilic_pos) & is.null(input$rp_neg) & is.null(input$rp_pos)){
      return(NULL)
    }
    withProgress({
      files <- list()
      modes <- c()
      incProgress(0.1,message = "Loading hilic neg file")
      if(!is.null(input$hilic_neg)){
        files$hilic_neg <- load_metabo_data(input$hilic_neg$datapath)
        modes <- c(modes,"HILIC_neg")
      }
      incProgress(0.25,message = "Loading hilic pos file")
      if(!is.null(input$hilic_pos)){
        files$hilic_pos <- load_metabo_data(input$hilic_pos$datapath)
        modes <- c(modes,"HILIC_pos")
      }
      incProgress(0.25,message = "Loading rp neg file")
      if(!is.null(input$rp_neg)){
        files$rp_neg <- load_metabo_data(input$rp_neg$datapath)
        modes <- c(modes,"RP_neg")
      }
      incProgress(0.25,message = "Loading rp pos file")
      if(!is.null(input$rp_pos)){
        files$rp_pos <- load_metabo_data(input$rp_pos$datapath)
        modes <- c(modes,"RP_pos")
      }
    })
    
    list(files=files,modes=modes)
  })
  
  # Combine MPP files
  # OUTPUT: list
  #       - annotations: data frame of compound annotations
  #       - data_matrix: the combined data matrix
  mpp_combine_list <- eventReactive(input$combine_mpp,{
    
    if (is.null(sample_dframe_processed())){
      return (NULL)
    }
    if(is.null(input$hilic_neg) & is.null(input$hilic_pos) & is.null(input$rp_neg) & is.null(input$rp_pos)){
      return(NULL)
    }
    if(input$project_title_mpp == "" | sample_processed_warnings() != ""){
      return(NULL)
    }
    withProgress(message = "Preliminary check", value = 0.05,{
      mpp_files<- mpp_loaded()$files
      modes <- mpp_loaded()$modes
      combine_mpp(mpp_files,modes,sample_dframe_processed())
    })
  })
  
  # Preview the data matrix and annotation tables
  output$mpp_preview <- renderUI({
    if (is.null(mpp_combine_list())){
      return(NULL)
    }
    tagList(
      br(),
      strong("Preview:"),
      radioButtons("preview_choice",NULL,choices = c("Data matrix" = "data_matrix","Annotation file" = "annotations")),
      dataTableOutput("mpp_preview_tbl")
    )
  })
  
  output$mpp_preview_tbl <- renderDataTable({
    mpp_preview_dframe()
  })
  
  mpp_preview_dframe <- reactive({
    if(input$preview_choice == "data_matrix"){
      mpp_combine_list()$data_matrix[,1:10]
    }
    else{
      mpp_combine_list()$annotations
    }
  })
  
  # Create download buttons
  output$mpp_download <- renderUI({
    if (is.null(mpp_combine_list())){
      return(NULL)
    }
    tagList(
      fluidRow(
        column(5,
               strong("Download data matrix:"),
               downloadButton("mpp_data_matrix")),
        column(5,
               strong("Download annotation file:"),
               downloadButton("mpp_annotations"))
      ))
  })
  
  output$mpp_annotations <- downloadHandler(
    filename = paste(input$project_title_mpp,"mpp_annotations.csv",sep="_"),
    
    content = function(file) {
      write.csv(mpp_combine_list()$annotations,file, na = "", row.names = FALSE, eol = "\r\n")
    }
  )
  
  output$mpp_data_matrix <- downloadHandler(
    filename = paste(input$project_title_mpp,"mpp_data_matrix.csv",sep="_"),
    
    content = function(file) {
      write.csv(mpp_combine_list()$data_matrix,file, na = "", row.names = FALSE, eol = "\r\n")
    }
  )
  
  
  # Download buttons for instruction PDFs
  output$instructions <- downloadHandler(
    filename = "Wranglr_instructions.pdf",
    
    content = function(file){
      file.copy("Wranglr_instructions.pdf",file)
    }
  )
  
  output$sample_specs <- downloadHandler(
    filename = "sample_form_specification.pdf",
    
    content = function(file){
      file.copy("sample_form_specification.pdf",file)
    }
  )
})