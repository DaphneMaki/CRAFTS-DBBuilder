library(shiny)


ui <- fluidPage(

    titlePanel("CRAFTS Database Builder"),

    # Which file they want to use 
    fileInput("Master_file",
              label = "Please Choose a Master File", 
              accept = c(".xlsx", ".csv")
              ),
    
    
    # the file inputs
    fluidRow(
      column(4,fileInput("folder_one",
              label = "Please upload all files from your first subfolder", 
              accept = c(".txt"), 
              multiple = TRUE)),
    
      column(4,fileInput("folder_two",
              label = "Please upload all files from your second subfolder", 
              accept = c(".txt"), 
              multiple = TRUE)),
    
      column(4,fileInput("folder_three",
              label = "Please upload all files from your third subfolder", 
              accept = c(".txt"), 
              multiple = TRUE))
    ),
    
    
    # if I can find a way to get them to upload a folder, I'd use this to get the subfolders
   # selectInput("sub_folder_structure", label = "Subfolder Structure", choices = c("LOW MID HIGH", "1 2 3", " +30 V +60 V +90 V")),
    
    fluidRow(
      column(6, selectInput("type_of_database", label = "Type of Database", choices = c("Basic", "Full"))),
      column (6, numericInput("tolerance", "Enter m/z tolerance in Daltons", value = 0.005, min = 0))
      ),
    
    fluidRow(
      column(6, uiOutput("gas")),
      column(6,uiOutput("ion"))
    ),
   
   textInput("file_name", "Enter name for generated database files."),
   
   actionButton("run", "Run", class = "btn-block btn-lg btn-primary"),
   
   #tableOutput("results") # this was for testing reactive elements and should be deleted when I'm done
   )


server <- function(input, output, session) {
  
  # first the stuff that only appears if you select a full database
  
  output$gas <- renderUI({
    if(input$type_of_database == "Full"){
      selectInput("gas_phase", "Under which gas phase were the spectra collected?", choices = c("He", "N2"))
      
    } # ends if statement
      }) #ends renderUI for gas phase
  
  output$ion <- renderUI({
    if(input$type_of_database == "Full"){
      selectInput("ion_mode", "Please choose the ion mode for creating database", choices = c("Positive", "Negative"))
    } # ends if statement
  }) #ends renderUI for ion mode
  
  
  results <- eventReactive(input$run, {
    
    req(input$Master_file)
    
    # Make sure the master file is an xlsx or csv file
    
    if(grepl("\\.xlsx$", input$Master_file$datapath, ignore.case = TRUE)){
      meta_dat <- readxl::read_excel(input$Master_file$datapath) 
    } else if(grepl("\\.csv$", input$Master_file$datapath, ignore.case = TRUE)){
      meta_dat <- read.csv(input$Master_file$datapath)
    } else {
      showNotification( "Please upload a .xlsx or .csv file.",type = "error"
      )} # gives an error if the file isn't a csv or xlsx
      
    
    
    
    # output$results <- renderTable({
    #   results()
    # }) # This was for testing reactive elements and should be deleted when I'm done
    
    # do the stuff HERE!!!
    
    # Step 1. Initiating database metadata by reading master_file.
    LibMaster <- data.table::as.data.table(metadat)
    nCompounds <- dim(LibMaster)[1]
    
    
      })
    

  
}
shinyApp(ui = ui, server = server)
