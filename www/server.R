library(xlsx)
library(bbd)
library(dplyr)
library(stringr)

source("functions.R")

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input,output){
  
  output$format_check <- renderUI({
    if (is.null(input$sample_input)){
      return(NULL)
    }
    datafile <- try(read.xlsx(input$sample_input$datapath, sheetIndex = 1))
    if(class(datafile)=="try-error"){
      return(HTML('<p style = "color: red;"> The file should be in .xlsx format </p>'))
    }
    else{
      return("")
    }
  })
  
  sample_dframe <- reactive({
    if (is.null(input$sample_input)){
      return(NULL)
    }
    datafile <- try(read.xlsx(input$sample_input$datapath, sheetIndex = 1))
    if(class(datafile)=="try-error"){
      return(NULL)
    }
    datafile
  })
  
  # Validates that the sample information file matches the required format
  sample_warnings <- reactive({
    if (is.null(sample_dframe())){
      return(NULL)
    }
    warnings <- warnings_sample(sample_dframe())
    return(warnings)
  })
  
  output$sample_warnings <- renderUI({
    if (is.null(sample_warnings())){
      return(NULL)
    }
    if(sample_warnings() == ""){
      return(p("Sample information sheet OK"))
    }
    paste("<p style=\"color: red;\">",sample_warnings(),"</p>",sep="") %>% HTML()
  })
  
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
  
  # Prints the output of bbd::summarize_data
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
  
  sample_meta_dframe <- reactive({
    if(is.null(input$sample_input)){
      return(NULL)
    }
    datafile <- try(read.xlsx(input$sample_input$datapath, sheetIndex = 1))
    if(class(datafile)=="try-error"){
      return(NULL)
    }
    if (getSheets(loadWorkbook(input$sample_input$datapath)) %>% length() < 2){
      return(NULL)
    }
    read.xlsx(input$sample_input$datapath, sheetIndex = 2)
  })
  
  # Checks that the file matches the required format and that the variables described
  # are found in the sample information file
  output$meta_warnings <- renderUI({
    if(is.null(sample_dframe())){
      return(NULL)
    }
    meta_title <- p("Variable metadata information:")
    if(getSheets(loadWorkbook(input$sample_input$datapath)) %>% length() < 2){
      return(tagList(meta_title,
                     p("No variable metadata sheet found",style="color:red;")))
    }
    warnings <- warnings_meta(sample_dframe(),sample_meta_dframe())
    if(warnings == ""){
      return(tagList(meta_title,
                     p("Variable metadata sheet OK")))
    }
    tagList(meta_title,
                   p(HTML(warnings),style="color:red;"))
  })
  
  output$variables <- renderText({
    if(is.null(sample_dframe()) | is.null(sample_meta_dframe())){
      return(NULL)
    }
    info_variables(sample_dframe(),sample_meta_dframe())
  })
  
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
  
  output$qc_poschars <- renderUI({
    if(input$sample_position_type == "well"){
      qc_def <- "P2-H12"
    }
    else{
      qc_def <- "P2-F9"
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
  
  output$second_column <- renderUI({
    selectInput("second_column_choice","Second column for MPP",
                choices = c(colnames(sample_dframe()),"QC"),
                selected = default_column())
  })
  
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
  
  # Randomizes the samples, adds QC and generates internal IDinput$project_title_mpp == ""input$project_title_mpp == ""input$project_title_mpp == ""input$project_title_mpp == ""input$project_title_mpp == ""
  sample_modified <- eventReactive(input$modify_sample,{
    if(is.null(sample_dframe()) | input$project_title == "" | !grepl('^[A-Za-z0-9_.-]+$', input$project_title)){
      return(NULL)
    }
    if (!is.null(sample_warnings()) & sample_warnings() != ""){
      return(NULL)
    }
    modify_sample(sample_dframe(),input$project_title, input$save, input$folder, as.numeric(input$qc_int),input$modes, qc_begins(),
                  input$random, input$sample_position_type,qc_pos_chars(), input$second_column_choice)
  })
  
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
    if(!grepl('^[A-Za-z0-9_.-]+$', input$project_title)){
      return(p("Only alphanumeric characters allowed in project title (no umlauts)",style = "color:red;"))
    }
    return(tagList(
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
  
  output$sample_mod_table <- renderUI({
    if(input$table_choice == "MPP"){
      dataTableOutput("sample_mod")
    }
    else{
      dataTableOutput("sample_mod_worklist")
    }
  })
  
  output$sample_mod_worklist <- renderDataTable({
    separated_worklist()[[1]]
  })
  
  output$sample_mod <- renderDataTable({
    sample_modified()$MPP
  })
  
  output$sample_mod_info <- renderUI({
    sample_modified_info()
  })
  
  output$sample_mod_down_button <- downloadHandler(
    filename = paste(input$project_title,"sample_info_processed.csv",sep = "_"),
    
    content = function(file) {
      write.csv(sample_modified()$MPP,file, na = "", row.names = FALSE, quote = F, eol = "\r\n")
    }
  )
  
  output$worklist_downloads <- renderUI({
    tagList(
      fluidRow(
        conditionalPanel(condition = "input.modes.includes('HILIC_neg')",
                         column(3,
                               downloadButton("worklist_hilic_neg_download","HILIC neg file"))),
        conditionalPanel(condition = "input.modes.includes('HILIC_pos')",
                         column(3,
                                downloadButton("worklist_hilic_pos_download","HILIC pos file")))),
      br(),
      fluidRow(
        conditionalPanel(condition = "input.modes.includes('RP_neg')",
                         column(3,
                                downloadButton("worklist_rp_neg_download","RP neg file"))),
        conditionalPanel(condition = "input.modes.includes('RP_pos')",
                         column(3,
                                downloadButton("worklist_rp_pos_download","RP pos file")))
      )
    )
  })
  
  separated_worklist <- reactive({
    if(is.null(sample_modified())){
      return(NULL)
    }
    separate_worklists(sample_modified()$worklist,input$modes)
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
  
  output$project_title_mirror <- renderUI({
    textInput("project_title_mpp","Project Title", value = input$project_title)
  })
  
  sample_dframe_processed <- reactive({
    if (is.null(input$sample_input_mpp)){
      return(NULL)
    }
    incProgress(message = "Reading file")
    read.csv(input$sample_input_mpp$datapath, h = T)
  })
  
  sample_processed_warnings <- reactive({
    if(is.null(sample_dframe_processed())){
      return(NULL)
    }
    warnings_sample_processed(sample_dframe_processed())
  })
  
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
  
  # Returns combined data matrix and annotation file in a list
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
})