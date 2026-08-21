library(shiny)


ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("CRAFTS Database Builder",

    titlePanel("CRAFTS Database Builder"),

    # Which file they want to use 
    fluidRow(
      column(6, fileInput("Master_file",
                          label = "Please Choose a Master File", 
                          accept = c(".xlsx", ".csv"))), # defaults to only accept csv and xlsx files, but it's easily bypassed so there are checks further in 
      column(6, uiOutput("codes")) # this here is to display the codes that were found in the metadata
    ),
    
    # the file inputs
    fluidRow(
      column(4,fileInput("folder_one",
                         label = "Please upload all files from your first subfolder", 
                         accept = c(".txt"),  # defaults to only accept txt files, but it's easily bypassed so I need to make more checks
                         multiple = TRUE), # allows the user to upload multiple files
             #uiOutput("folder_one_files"), # displays the names of the uploaded files; gets long if user uploads too many files
             uiOutput("folder_one_dropdown") # creates a drop down for all the uploaded files
             ),
    
      column(4,fileInput("folder_two",
                         label = "Please upload all files from your second subfolder", 
                         accept = c(".txt"), # defaults to only accept txt files, but it's easily bypassed so I need to make more checks
                         multiple = TRUE), # allows the user to upload multiple files
             #uiOutput("folder_two_files")  # displays the names of the uploaded files; gets long if user uploads too many files
             uiOutput("folder_two_dropdown") # creates a drop down for all the uploaded files
             
             ), 
    
      column(4,fileInput("folder_three",
                         label = "Please upload all files from your third subfolder",
                         accept = c(".txt"), # defaults to only accept txt files, but it's easily bypassed so I need to make more checks
                         multiple = TRUE), # allows the user to upload multiple files
             #uiOutput("folder_three_files")   # displays the names of the uploaded files; gets long if user uploads too many files
             uiOutput("folder_three_dropdown") # creates a drop down for all the uploaded files
             ) 
      ),
      
    # if I can find a way to get them to upload a folder, I'd use this to get the subfolders
   # selectInput("sub_folder_structure", label = "Subfolder Structure", choices = c("LOW MID HIGH", "1 2 3", " +30 V +60 V +90 V")),
    
    fluidRow(
      column(6, selectInput("type_of_database", label = "Type of Database", choices = c("Basic", "Full"))),
      column (6, numericInput("tolerance", "Enter m/z tolerance in Daltons", value = 0.005, min = 0))
      ),
    
   # the "gas" and "ion" rows will only show up if "Full" database type is selected
    fluidRow(
      column(6, uiOutput("gas")),
      column(6,uiOutput("ion"))
    ),
   
   textInput("file_name", "Enter name for generated database files."),
   
   actionButton("run", "Run", class = "btn-block btn-lg btn-primary"),
   
   tableOutput("results") # this was for testing reactive elements and should be deleted when I'm done
   ), # end of page one
   
   # start of page two
   tabPanel("About",  textOutput("text") # add anything that needs to be added (i.e tables, graphs, ect) to page two here
            )
   
  )
  ) 
server <- function(input, output, session) {
  
  # showing the user a list of the files that they uploaded
  
  # output$folder_one_files <- renderUI({
  #   req(input$folder_one)
  #   
  #   tags$div(
  #     tags$strong("Uploaded files from first folder:"),
  #     tags$ul(
  #       lapply(input$folder_one$name, function(x){
  #         tags$li(style = "overflow-wrap: anywhere; word-break: break-word;",
  #                 x)} # this function is to make sure that the file names don't overlap by wrapping the text
  #         )))
  # }) # this is for the text-only output
  
  output$folder_one_dropdown <- renderUI({
    req(input$folder_one)
    
    selectizeInput("folder_one_uplodaded_files", 
                   "Uploaded files from first folder:",
                   choices = setNames(
                     input$folder_one$name,
                     input$folder_one$name
      ),
      options = list(
        placeholder = "Search for a file...",
        maxOptions = 20
      )
    )
  })

  
  # output$folder_two_files <- renderUI({
  #   req(input$folder_two)
  #   
  #   tags$div(
  #     tags$strong("Uploaded files from second folder:"),
  #     tags$ul(
  #       lapply(input$folder_one$name, function(x){
  #         tags$li(style = "overflow-wrap: anywhere; word-break: break-word;",
  #                 x)} # this function is to make sure that the file names don't overlap by wrapping the text
  #       )))
  # })
  
  output$folder_two_dropdown <- renderUI({
    req(input$folder_two)
    
    selectizeInput("folder_two_uplodaded_files", 
                   "Uploaded files from second folder:",
                   choices = setNames(
                     input$folder_two$name,
                     input$folder_two$name
                   ),
                   options = list(
                     placeholder = "Search for a file...",
                     maxOptions = 20
                   )
    )
  })
  
  
  # output$folder_three_files <- renderUI({
  #   req(input$folder_three)
  #   
  #   tags$div(
  #     tags$strong("Uploaded files from third folder:"),
  #     tags$ul(
  #       lapply(input$folder_one$name, function(x){
  #         tags$li(style = "overflow-wrap: anywhere; word-break: break-word;",
  #                 x)} # this function is to make sure that the file names don't overlap by wrapping the text
  #       )))
  # })
  
  output$folder_three_dropdown <- renderUI({
    req(input$folder_three)
    
    selectizeInput("folder_three_uplodaded_files", 
                   "Uploaded files from third folder:",
                   choices = setNames(
                     input$folder_three$name,
                     input$folder_three$name
                   ),
                   options = list(
                     placeholder = "Search for a file...",
                     maxOptions = 20
                   )
    )
  })
  
  
  #display the codes from the metadata
  
  output$codes <- renderUI({
    
    req(input$Master_file)
    
    # this part is just making sure that they uploaded a xlsx or csv file
    if(grepl("\\.xlsx$", input$Master_file$datapath, ignore.case = TRUE)){
      meta_dat <- readxl::read_excel(input$Master_file$datapath) 
    } else if(grepl("\\.csv$", input$Master_file$datapath, ignore.case = TRUE)){
      meta_dat <- read.csv(input$Master_file$datapath)
    } else {
      showNotification( "Please upload a .xlsx or .csv file.",type = "error")
      return(NULL)
    }
    
    # this part is the one that actually grabs the codes and displays them as a list
    codes <- meta_dat$Code
    tags$div(
      tags$strong("Codes found in the metadata file:"),
      tags$ul(
      lapply(codes, function(x) {
        tags$li(x)
      })
    ))
  })
  
  
  
  # display the stuff that only appears if you select a full database
  
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
      showNotification( "Please upload a .xlsx or .csv file.",type = "error")
       return(NULL)
      
      }
    
    # do the stuff HERE
    
    # Step 1. Initiating database metadata by reading master_file.
    LibMaster <- data.table::as.data.table(meta_dat)
    #nCompounds <- dim(LibMaster)[1]
    
    
    # gives an error if the file isn't a csv or xlsx, and reads the file in if it is 
  }) # ends the "results" section
  
  
  # in this case, right now the code is outputing a table of the metadata, or an error if it's not a csv or xlsx file
     output$results <- renderTable({
       results()
     }) # This was for testing reactive elements and should be deleted when I'm done
    

     
     # for page two
     
     output$text <- renderText({ 
       "Hello world" # put all of the "about" text here 
     }) 
  
}
shinyApp(ui = ui, server = server)
