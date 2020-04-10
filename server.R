source("www/functions.R")

library(openxlsx)
library(dplyr)
library(stringr)
library(tidyr)
library(shiny)
# Set the maximum size of uploaded files to 30MB
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output){
  
  # Check that the input sample list file can be read correctly
  output$format_check <- renderUI({
    if (is.null(input$sample_input)){
      return(NULL)
    }
    datafile <- try(read.xlsx(input$sample_input$datapath, sheet = 1))
    if(class(datafile) == "try-error"){
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
    paste0("<p style=\"color: red;\">", sample_warnings(), "</p>") %>% HTML()
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
  
  # Print the output of summarize_data
  output$summarize_data <- renderUI({
    if (is.null(sample_dframe())){
      return(NULL)
    }
    smry <- capture.output(summarize_data(sample_dframe()))
    out_text <- ""
    for (i in 1:length(smry)){
      out_text <- paste(out_text,smry[i], sep = "<br/>")
    }
    out_text %>% HTML()
  })
  
  default_project_code <- reactive({
    if(input$project_title != ""){
      default <- get_default_project_code(input$project_title)
    } else{
      default <- ""
    }
    default
  })
  
  output$project_code <- renderUI({
    textInput("project_code", "Project code", value = default_project_code())
  })
  
  # Project home folder input
  # defaults to "D:\\MassHunter\\Data\\[project_title]"
  output$destination <- renderUI({
    if(input$project_title != ""){
      textInput("folder","Project home folder",
                value= paste0("D:\\MassHunter\\Data\\", input$project_title, "\\"))
    }
    else{
      textInput("folder", "Project home folder",
                value= "D:\\MassHunter\\Data\\")
    }
  })
  
  output$subject_id_column_choice <- renderUI({
    tagList(
      checkboxInput("include_subject_id", "Run samples from same subject sequently"),
      conditionalPanel(condition = "input.include_subject_id",
                       selectInput("subject_id_column", "Subject ID column",
                                   choices = colnames(sample_dframe())))
    )
  })
  
  output$grouping_column_choice <- renderUI({
    selectInput("grouping_column", "Column containing groups for randomization",
                choices = colnames(sample_dframe()))
  })
  
  # Choices for QC positions
  # All default to last position on the second plate
  output$qc_poschars <- renderUI({
    if(input$sample_position_type == "well"){
      qc_defs <- paste0(paste0("P", input$n_plates, "-H"), (13-length(input$modes)):12)
    } else {
      qc_defs <- paste0(paste0("P", input$n_plates, "-F"), (10-length(input$modes)):9)
    }
    names(qc_defs) <- input$modes
    tagList(
      fluidRow(
        conditionalPanel(condition = "input.modes.includes('HILIC_neg')",
                         column(3,
                                textInput("qc_poschar_hilicneg", "HILIC neg",
                                          value = qc_defs["HILIC_neg"]))),
        conditionalPanel(condition = "input.modes.includes('HILIC_pos')",
                         column(3,
                                textInput("qc_poschar_hilicpos", "HILIC pos",
                                          value = qc_defs["HILIC_pos"]))),
        conditionalPanel(condition = "input.modes.includes('RP_neg')",
                         column(3,
                                textInput("qc_poschar_rpneg", "RP neg",
                                          value = qc_defs["RP_neg"]))),
        conditionalPanel(condition = "input.modes.includes('RP_pos')",
                         column(3,
                                textInput("qc_poschar_rppos", "RP pos",
                                          value = qc_defs["RP_pos"])))
      )
    )
  })
  
  # Choice of samples for AutoMSMS
  # values from original SAMPLE_ID column
  output$msms_samples <- renderUI({
    selectizeInput("msms_samples_choice", "Choose samples for AutoMSMS",
                   choices = sample_dframe()$SAMPLE_ID, multiple = TRUE)
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
  #       - samples: a modified sample information file
  #       - worklist: a data frame containing the worklists for all the modes
  sample_modified <- eventReactive(input$modify_sample,{
    if(is.null(sample_dframe()) || input$project_title == "" || input$project_code == "" ||
       !grepl('^[A-Za-z0-9_.-]+$', input$project_title)){
      return(NULL)
    }
    if (!is.null(sample_warnings()) && sample_warnings() != ""){
      return(NULL)
    }
    modify_sample(sample_dframe(), input$project_title, input$project_code, input$save_code, input$folder,
                  as.numeric(input$n_plates), as.numeric(input$qc_int), input$modes, qc_begins(),
                  input$sample_order, input$grouping_column, input$include_subject_id, input$subject_id_column,
                  input$sample_position_type, qc_pos_chars())
  })
  
  # Separate the worklist files
  # Add AutoMSMS and STOP to the end of file
  separated_worklist <- reactive({
    if(is.null(sample_modified())){
      return(NULL)
    }
    samples <- sample_modified()$samples
    indx <- which(as.character(samples$SAMPLE_ID) %in% input$msms_samples_choice)
    msms_sample_ids <- samples$INTERNAL_SAMPLE_ID[indx] %>% unique()
    separate_worklists(sample_modified()$worklist,input$modes, input$msms_qc, msms_sample_ids)
  })
  
  output$sample_mod_info <- renderUI({
    sample_modified_info()
  })
  
  # Check that everything is OK
  # Process the sample information file
  # Create display for modified sample information file and worklist file
  # Create download buttons
  sample_modified_info <-eventReactive(input$modify_sample,{
    if(is.null(sample_dframe())){
      return(p("Please input a sample information file", style = "color:red;"))
    }
    if ((!is.null(sample_warnings()) && sample_warnings() != "")){
      return(p("The sample information sheet is not valid!",style = "color:red;"))
    }
    if (input$project_title == ""){
      return(p("Please input a project title", style = "color:red;"))
    }
    if (input$project_code == ""){
      return(p("Please input a project code", style = "color:red;"))
    }
    if(!grepl('^[A-Za-z0-9_.-]+$', input$project_title)){
      return(p("Only alphanumeric characters allowed in project title (no umlauts)", style = "color:red;"))
    }
    return(tagList(
      uiOutput("randomize_columns_warnings"),
      h4("Preview:"),
      radioButtons("table_choice", NULL,
                   choices = c("Processed sample information file" = "samples", "Worklist file" = "worklist")),
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
    paste0("<p style=\"color: red;\">", sample_warnings(),"</p>") %>% HTML()
  })
  
  randomize_column_warning_msg <- eventReactive(input$modify_sample, {
    if (is.null(sample_dframe()) || (input$sample_order %in% c("original", "random_global") &&
                                     !input$include_subject_id)){
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
    paste0("<p style=\"color: red;\">", msg, "</p>") %>% HTML()
  })
  
  # dataTableOutput of either modified sample information file or the worklist file
  output$sample_mod_table <- renderUI({
    if(input$table_choice == "samples"){
      DT::dataTableOutput("sample_mod")
    }
    else{
      DT::dataTableOutput("sample_mod_worklist")
    }
  })
  
  # First worklist file
  output$sample_mod_worklist <- DT::renderDataTable({
    separated_worklist()[[1]]
  })
  
  # Modified sample informaiton ifle
  output$sample_mod <- DT::renderDataTable({
    sample_modified()$samples
  })
  
  # Download button for modified sample information file file
  output$sample_mod_down_button <- downloadHandler(
    filename = paste(input$project_title, "sample_info_processed.csv", sep = "_"),
    
    content = function(file) {
      write.csv(sample_modified()$samples, file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  # Download buttons for worklist files
  output$worklist_downloads <- renderUI({
    tagList(
      fluidRow(
        conditionalPanel(condition = "input.modes.includes('HILIC_neg')",
                         column(4,
                                downloadButton("worklist_hilic_neg_download", "HILIC neg file"))),
        conditionalPanel(condition = "input.modes.includes('HILIC_pos')",
                         column(4,offset = 1,
                                downloadButton("worklist_hilic_pos_download", "HILIC pos file")))),
      br(),
      fluidRow(
        conditionalPanel(condition = "input.modes.includes('RP_neg')",
                         column(4,
                                downloadButton("worklist_rp_neg_download", "RP neg file"))),
        conditionalPanel(condition = "input.modes.includes('RP_pos')",
                         column(4,offset = 1,
                                downloadButton("worklist_rp_pos_download", "RP pos file")))
      )
    )
  })
  
  output$worklist_hilic_neg_download <- downloadHandler(
    filename = paste(input$project_title, "worklist_hilic_neg.csv", sep = "_"),
    
    content = function(file) {
      write.csv(separated_worklist()$HILIC_neg, file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  output$worklist_hilic_pos_download <- downloadHandler(
    filename = paste(input$project_title,"worklist_hilic_pos.csv", sep = "_"),
    
    content = function(file) {
      write.csv(separated_worklist()$HILIC_pos, file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  output$worklist_rp_neg_download <- downloadHandler(
    filename = paste(input$project_title,"worklist_rp_neg.csv", sep = "_"),
    
    content = function(file) {
      write.csv(separated_worklist()$RP_neg, file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  output$worklist_rp_pos_download <- downloadHandler(
    filename = paste(input$project_title,"worklist_rp_pos.csv", sep = "_"),
    
    content = function(file) {
      write.csv(separated_worklist()$RP_pos, file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  # ------------------- Instructions tab ------------------------

  # Download buttons for instruction PDFs
  output$instructions <- downloadHandler(
    filename = "Wranglr_instructions.pdf",
    
    content = function(file){
      file.copy("www/Wranglr_instructions.pdf", file)
    }
  )
  
  output$sample_specs <- downloadHandler(
    filename = "sample_form_specification.pdf",
    
    content = function(file){
      file.copy("www/sample_form_specification.pdf", file)
    }
  )
  
  output$sample_info <- downloadHandler(
    filename = "sample_info.xlsx",
    
    content = function(file){
      file.copy("www/sample_info.xlsx", file)
    }
  )
  
  output$repeated_measurements <- downloadHandler(
    filename = "repeated_measurements.xlsx",
    
    content = function(file){
      file.copy("www/repeated_measurements.xlsx", file)
    }
  )
  
  output$packages <- downloadHandler(
    filename = "installed_packages.csv",
    
    content = function(file){
      write.csv(installed.packages(), file,  na = "", row.names = FALSE, eol = "\r\n")
    }
  )
})