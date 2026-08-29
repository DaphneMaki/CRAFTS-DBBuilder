library(shiny)
library(shinyjs)

#needed functions, idk if I can use "source" so just putting them here

path <- getwd()
parent_path <- paste0(path,"/source")
child_path <- paste0(parent_path,"/asm_Preliminaries.R")
source(child_path)

user_OS <- .Platform$OS.type   # for OS specific system commands
DEThreshold = 0.30;           # Dimer Error Threshold
NoiseThreshold = 0.45;        # Unexplained Peak Intensity Threshold
IsotopeRatioThreshold = 0.90 # similarity between  measured
# and calculated molecular ion envelope
# the above 2 lines taken from the full DB builder



#############################################################################################################################

ui <- fluidPage(
  useShinyjs(),
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
                                  label = "Please upload all LOW fragmentation Mass Spec files", 
                                  accept = c(".txt"),  # defaults to only accept txt files, but it's easily bypassed so I need to make more checks
                                  multiple = TRUE), # allows the user to upload multiple files
                      #uiOutput("folder_one_dropdown") # creates a drop down for all the uploaded files
                      uiOutput("folder_one_file_names") # creates a scrollable text box of the names of the uploaded files
               ),
               
               column(4,fileInput("folder_two",
                                  label = "Please upload all MID fragmentation Mass Spec files", 
                                  accept = c(".txt"), # defaults to only accept txt files, but it's easily bypassed so I need to make more checks
                                  multiple = TRUE), # allows the user to upload multiple files
                      uiOutput("folder_two_dropdown") # creates a drop down for all the uploaded files
               ), 
               
               column(4,fileInput("folder_three",
                                  label = "Please upload all HIGH fragmentation Mass Spec files",
                                  accept = c(".txt"), # defaults to only accept txt files, but it's easily bypassed so I need to make more checks
                                  multiple = TRUE), # allows the user to upload multiple files
                      uiOutput("folder_three_dropdown") # creates a drop down for all the uploaded files
               ) 
             ),
             
             # if I can find a way to get them to upload a folder, I'd use this to get the subfolders
             # selectInput("sub_folder_structure", label = "Subfolder Structure", choices = c("LOW MID HIGH", "1 2 3", " +30 V +60 V +90 V")),
             
             fluidRow(
               column(6, selectInput("type_of_database", label = "Type of Database", choices = c("Basic", "Full"))),
               column (6, numericInput("tolerance", "Enter m/z tolerance in Daltons", value = 0.005, min = 0, step = 0.001))
             ),
             
             # the "gas" and "ion" rows will only show up if "Full" database type is selected
             fluidRow(
               column(6, uiOutput("gas")),
               column(6,uiOutput("ion"))
             ),
             
             fluidRow(
               column(6, uiOutput("SDF_file")),
               column(6,uiOutput("formula_list"))
             ),
             
             fluidRow(
               column(6, uiOutput("build_type"))
             ),
             
             textInput("file_name", "Enter name for generated database files."),
             
             actionButton("run", "Run", class = "btn-block btn-lg btn-primary"),
             
             div(
               style = "position: absolute;
               left: -9999px;
               width: 1px;
               height: 1px;
               overflow: hidden;",
               downloadButton("download_library", "Download Library")
             ),
             
    ), # end of page one
    
    # start of page two
    tabPanel("About",  textOutput("text") # add anything that needs to be added (i.e tables, graphs, ect) to page two here
    )
    
  )
) 

###############################################################################################################################

server <- function(input, output, session) {
  
  metadata_good <- reactiveVal(FALSE, label = "checks for metadata")
  
  uploaded_metadata <- reactive({
    req(input$Master_file)
    # this part is just making sure that they uploaded a xlsx or csv file
    if(grepl("\\.xlsx$", input$Master_file$datapath, ignore.case = TRUE)){
      meta_dat <- readxl::read_excel(input$Master_file$datapath) 
    } else if(grepl("\\.csv$", input$Master_file$datapath, ignore.case = TRUE)){
      meta_dat <- read.csv(input$Master_file$datapath)
    } else {
      showNotification( "Please upload a .xlsx or .csv file.",type = "error", duration = NULL)
      return(NULL)
    }
    metadata_good(TRUE)
    meta_dat
  })
  
  #display the codes from the metadata
  
  output$codes <- renderUI({
    
    meta_dat <- uploaded_metadata()
    
    req(meta_dat)
    
    # this part is the one that actually grabs the codes and displays them as a list
    codes <- meta_dat$Code
    
    if(length(codes) == 0){
      showNotification("No codes found in metadata", type = "error", duration = NULL, closeButton = FALSE, 
                       id = "no_codes_in_metadata")
      return(NULL)
    } else{
      removeNotification("no_codes_in_metadata")
    }
    
    selectizeInput("codes_in_metadata", 
                   "Codes found in the metadata file:",
                   choices = setNames(
                     codes,
                     codes),
                   options = list(
                     placeholder = "Search for a code...",
                     maxOptions = 20
                   )
    )
  })
  
  
  # showing the user a list of the files that they uploaded as a drop-down menu
  
  
  output$folder_one_file_names <- renderUI({
    req(input$folder_one)
    
    tags$div(
      tags$strong("Uploaded files from first folder:"),
      tags$div(
        style ="height: 120px; 
                          overflow-y: auto; 
                          border: 1px solid #ccc; 
                          border-radius: 4px;
                          padding: 8px;
                          margin-top: 5px;",
        lapply(input$folder_one$name, function(filename) {
          tags$div(
            style = "padding: 2px 0;",
            filename
          )
        })
      )
    )
  })
  
  
  output$folder_one_dropdown <- renderUI({
    req(input$folder_one)
    
    selectizeInput("folder_one_uplodaded_files", 
                   "Uploaded files from first folder:",
                   choices = setNames(
                     input$folder_one$name,
                     input$folder_one$name),
                   options = list(
                     placeholder = "Search for a file...",
                     maxOptions = 20
                   )
    )
  })
  
  output$folder_two_dropdown <- renderUI({
    req(input$folder_two)
    
    selectizeInput("folder_two_uplodaded_files", 
                   "Uploaded files from second folder:",
                   choices = setNames(
                     input$folder_two$name,
                     input$folder_two$name),
                   options = list(
                     placeholder = "Search for a file...",
                     maxOptions = 20
                   )
    )
  })
  
  output$folder_three_dropdown <- renderUI({
    req(input$folder_three)
    
    selectizeInput("folder_three_uplodaded_files", 
                   "Uploaded files from third folder:",
                   choices = setNames(
                     input$folder_three$name,
                     input$folder_three$name),
                   options = list(
                     placeholder = "Search for a file...",
                     maxOptions = 20
                   )
    )
  })
  
  # check if the uploaded spectra files are .txt files
  
  folder_one_good <- reactiveVal(FALSE, label = "checks for folder one")
  folder_two_good <- reactiveVal(TRUE, label = "checks for folder two")
  folder_three_good <- reactiveVal(TRUE, label = "checks for folder three")
  
  # checks for folder 1
  observe({
    req(input$folder_one)
    
    extensions_one <- tolower(tools::file_ext(input$folder_one$name))
    
    bad_files_one <- input$folder_one$name[extensions_one != "txt"]
    
    if (length(bad_files_one) > 0) {
      showNotification(
        paste(
          "Please upload all spectra files as .txt files. The following files are not .txt files:",
          paste(bad_files_one, collapse = ", ")),
        type = "error",
        duration = NULL, 
        closeButton = FALSE,
        id = "folder_one_not_a_txt"
      )
      folder_one_good(FALSE)
      return(NULL)
    } else {
      removeNotification("folder_one_not_a_txt")
      folder_two_good(TRUE)
    }
    
  })
  
  # checks for folder 2
  observe({
    req(input$folder_two)
    
    extensions_two <- tolower(tools::file_ext(input$folder_two$name))
    
    bad_files_two <- input$folder_two$name[extensions_two != "txt"]
    
    if (length(bad_files_two) > 0) {
      showNotification(
        paste(
          "Please upload all spectra files as .txt files. The following files are not .txt files:",
          paste(bad_files_two, collapse = ", ")),
        type = "error",
        duration = NULL,
        closeButton = FALSE, 
        id = "folder_two_not_a_txt"
      )
      folder_two_good(FALSE)
      
      return(NULL)
    } else {
      removeNotification("folder_two_not_a_txt")
      folder_two_good(TRUE)
    }
    
  })
  
  # checks for folder 3
  observe({
    req(input$folder_three)
    
    extensions_three <- tolower(tools::file_ext(input$folder_three$name))
    
    bad_files_three <- input$folder_three$name[extensions_three != "txt"]
    
    if (length(bad_files_three) > 0) {
      showNotification(
        paste(
          "Please upload all spectra files as .txt files. The following files are not .txt files:",
          paste(bad_files_three, collapse = ", ")),
        type = "error",
        duration = NULL,
        closeButton = FALSE,
        id = "folder_three_not_a_txt"
      )
      folder_three_good(FALSE)
      return(NULL)
    } else {
      removeNotification("folder_one_not_a_txt")
      folder_three_good(TRUE)
    }
    
  }) 
  
  
  # check if file names match the metadata code
  
  #folder one
  observe({
    meta_dat <- uploaded_metadata()
    req(meta_dat, input$folder_one)
    
    codes <- as.character(meta_dat$Code)
    file_codes <- tools::file_path_sans_ext(input$folder_one$name)
    
    missing_codes <- setdiff(file_codes, codes)
    missing_files <- setdiff(codes, file_codes)
    
    if (length(missing_codes) > 0) {
      showNotification(
        paste("The following spectra in folder 1 do not have corresponding codes in the metadata file: ",
              paste(missing_codes, collapse = ", ")),
        type = "error",
        duration = NULL, closeButton = FALSE,
        id = "folder_one_spectra_not_in_metadata"
      )
      folder_one_good(FALSE)
    } else{
      removeNotification("folder_one_spectra_not_in_metadata")
      folder_one_good(TRUE)
    }
    
    if (length(missing_files) > 0) {
      showNotification(
        paste("The following codes with metadata in the master file do not have corresponding spectra in folder one: ",
              paste(missing_files, collapse = ", ")),
        type = "error",
        duration = NULL, closeButton = FALSE, 
        id = "folder_one_code_in_metadata_but_not_files"
      )
      # folder_one_good(FALSE)
    } else{
      removeNotification("folder_one_code_in_metadata_but_not_files")
      #folder_one_good(TRUE)
    }
    
    folder_one_good(length(missing_files) == 0 && length(missing_codes) == 0)
  })
  
  #folder two
  observe({
    meta_dat <- uploaded_metadata()
    req(meta_dat, input$folder_two)
    
    codes <- as.character(meta_dat$Code)
    file_codes <- tools::file_path_sans_ext(input$folder_two$name)
    
    missing_codes <- setdiff(file_codes, codes)
    missing_files <- setdiff(codes, file_codes)
    
    if (length(missing_codes) > 0) {
      showNotification(
        paste("The following spectra in folder 2 do not have corresponding codes in the metadata file: ",
              paste(missing_codes, collapse = ", ")),
        type = "error",
        duration = NULL, closeButton = FALSE, 
        id = "folder_two_spectra_not_in_metadata"
      )
      folder_two_good(FALSE)
    } else{
      removeNotification("folder_two_spectra_not_in_metadata")
      folder_two_good(TRUE)
    }
    
    if (length(missing_files) > 0) {
      showNotification(
        paste("The following codes with metadata in the master file do not have corresponding spectra in folder two: ",
              paste(missing_files, collapse = ", ")),
        type = "error",
        duration = NULL, closeButton = FALSE,
        id = "folder_two_code_in_metadata_but_not_files"
      )
      #folder_two_good(FALSE)
    } else{
      removeNotification("folder_two_code_in_metadata_but_not_files")
      #folder_two_good(TRUE)
    }
    folder_two_good(length(missing_files) == 0 && length(missing_codes) == 0)
    
  })
  
  # folder three
  observe({
    meta_dat <- uploaded_metadata()
    req(meta_dat, input$folder_three)
    
    codes <- as.character(meta_dat$Code)
    file_codes <- tools::file_path_sans_ext(input$folder_three$name)
    
    missing_codes <- setdiff(file_codes, codes)
    missing_files <- setdiff(codes, file_codes)
    
    if (length(missing_codes) > 0) {
      showNotification(
        paste("The following spectra in folder 3 do not have corresponding codes in the metadata file: ",
              paste(missing_codes, collapse = ", ")),
        type = "error",
        duration = NULL, closeButton = FALSE,
        id = "folder_three_spectra_not_in_metadata"
      )
      folder_three_good(FALSE)
    } else{
      removeNotification("folder_three_spectra_not_in_metadata")
      folder_three_good(TRUE)
    }
    
    if (length(missing_files) > 0) {
      showNotification(
        paste("The following codes with metadata in the master file do not have corresponding spectra in folder three: ",
              paste(missing_files, collapse = ", ")),
        type = "error",
        duration = NULL, closeButton = FALSE,
        id = "folder_three_code_in_metadata_but_not_files"
      )
      #  folder_three_good(FALSE)
    } else{
      removeNotification("folder_three_code_in_metadata_but_not_files")
      # folder_three_good(TRUE)
    }
    
    folder_three_good(length(missing_files) == 0 && length(missing_codes) == 0)
  })
  
  
  
  # get the tolerance
  
  mz_tolerance <- reactive({
    req(input$tolerance)
    mz_res <- input$tolerance
    mz_res
  })
  
  
  # get the database name
  
  database_name <- reactive({
    
    if(is.null(input$file_name)| trimws(input$file_name) == ""){
      RDSfilename <- "Output_DARTMS_Database" # default database name if there was no input
    } else{
      RDSfilename <- input$file_name
    }
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
  
  output$SDF_file <- renderUI({
    if(input$type_of_database == "Full"){
      radioButtons("create_SDF", "Do you want to export the library as an SDF file (for MS Search)", choices = c("Yes", "No"))
    }
  }) # ends renderUI for SDF export
  
  output$formula_list <- renderUI({
    if(input$type_of_database == "Full"){
      radioButtons("create_formula_list", " Do you want to export the library as a formula list (for Mass Mountaineer)", choices = c("Yes", "No"))
    }
  }) # ends renderUI for formula list export
  
  output$build_type <- renderUI({
    if(input$type_of_database == "Full"){
      selectInput("selected_build_type", "Please select a build type", choices = c("Traditional", "Collapsed"))
    }
    
  })
  
  
  RDT_database <- reactiveVal(NULL) # this is where we store the data.table that we'll be making, because it's easier to call in the downloadHandler; the value is reassigned at the end of the "results" section
  
  
  # disable the run button until the files are confirmed good
  
  observe({
    if(all(c(folder_one_good(), folder_two_good(), folder_three_good(), metadata_good()))){
      enable("run")
    } else{
      disable("run")
    }
    
  })
  
  
  # the main part of the program, aka what happens when the "run" button is pressed
  
  observeEvent(input$run, {
    
    req(input$Master_file)
    
    # Make sure the master file is an xlsx or csv file; aka doing a second error check just to make sure that the user doesn't shoot themself in the foot
    
    if(grepl("\\.xlsx$", input$Master_file$datapath, ignore.case = TRUE)){
      meta_dat <- readxl::read_excel(input$Master_file$datapath) 
    } else if(grepl("\\.csv$", input$Master_file$datapath, ignore.case = TRUE)){
      meta_dat <- read.csv(input$Master_file$datapath)
    } else {
      showNotification( "Please upload a .xlsx or .csv file.",type = "error", duration = NULL)
      return(NULL)
    }
    
    
    #check if all folders have been uploaded 
    
    contains_folder_one <- TRUE
    contains_folder_two <- TRUE
    contains_folder_three <- TRUE
    
    if(is.null(input$folder_one)){
      showNotification( "Please upload files for LOW fragmentation spectra", 
                        type = "error", 
                        duration = NULL, 
                        closeButton = FALSE, 
                        id = "Low_fragmentation_missing")
      return(NULL)
    } else{
      removeNotification("Low_fragmentation_missing")
    }
    if(is.null(input$folder_two)){
      contains_folder_two <- FALSE
      showNotification( "No files uploaded for MID fragmentation", type = "warning")
    }
    if(is.null(input$folder_three)){
      contains_folder_three <- FALSE
      showNotification( "No files uploaded for HIGH fragmentation", type = "warning")
    }    
    
    
    # double check to make sure all spectra are .txt files
    
    # folder one
    req(input$folder_one)
    extensions_one <- tolower(tools::file_ext(input$folder_one$name))
    bad_files_one <- input$folder_one$name[extensions_one != "txt"]
    if (length(bad_files_one) > 0) {
      showNotification(
        paste(
          "Please upload all spectra files as .txt files. The following files are not .txt files:",
          paste(bad_files_one, collapse = ", ")),
        type = "error",
        duration = NULL)
      return(NULL)
    }
    
    # folder two
    if(contains_folder_two){
      
      req(input$folder_two)
      extensions_two <- tolower(tools::file_ext(input$folder_two$name))
      bad_files_two <- input$folder_two$name[extensions_two != "txt"]
      
      if (length(bad_files_two) > 0) {
        showNotification(
          paste(
            "Please upload all spectra files as .txt files. The following files are not .txt files:",
            paste(bad_files_two, collapse = ", ")),
          type = "error",
          duration = NULL)
        return(NULL)
      }
    }
    # folder three
    if(contains_folder_three){
      
      req(input$folder_three)
      extensions_three <- tolower(tools::file_ext(input$folder_three$name))
      bad_files_three <- input$folder_three$name[extensions_three != "txt"]
      
      if (length(bad_files_three) > 0) {
        showNotification(
          paste(
            "Please upload all spectra files as .txt files. The following files are not .txt files:",
            paste(bad_files_three, collapse = ", ")),
          type = "error",
          duration = NULL)
        return(NULL)
      }
    }
    
    
    #double check that the spectra exist in the metadata and vice versa
    codes <- as.character(meta_dat$Code)
    folder_error <- FALSE
    
    #folder one 
    file_codes_one <- tools::file_path_sans_ext(input$folder_one$name)
    
    missing_codes_one <- setdiff(file_codes_one, codes)
    missing_files_one <- setdiff(codes, file_codes_one)
    
    if (length(missing_codes_one) > 0) {
      showNotification(
        paste("The following spectra in folder 1 do not have corresponding codes in the metadata file: ",
              paste(missing_codes_one, collapse = ", ")),
        type = "error",
        duration = NULL)
      folder_error <- TRUE
      #return(NULL)
    }
    
    if (length(missing_files_one) > 0) {
      showNotification(
        paste("The following codes with metadata in the master file do not have corresponding spectra in folder one: ",
              paste(missing_files_one, collapse = ", ")),
        type = "error",
        duration = NULL)
      folder_error <- TRUE
      #return(NULL)
    }
    # folder two
    if(contains_folder_two){
      
      file_codes_two <- tools::file_path_sans_ext(input$folder_two$name)
      
      missing_codes_two <- setdiff(file_codes_two, codes)
      missing_files_two <- setdiff(codes, file_codes_two)
      
      if (length(missing_codes_two) > 0) {
        showNotification(
          paste("The following spectra in folder 2 do not have corresponding codes in the metadata file: ",
                paste(missing_codes_two, collapse = ", ")),
          type = "error",
          duration = NULL)
        folder_error <- TRUE
        #return(NULL)
      }
      
      if (length(missing_files_two) > 0) {
        showNotification(
          paste("The following codes with metadata in the master file do not have corresponding spectra in folder two: ",
                paste(missing_files_two, collapse = ", ")),
          type = "error",
          duration = NULL
        )
        folder_error <- TRUE
        #return(NULL)
      }
    }
    #folder three
    if(contains_folder_three){
      file_codes_three <- tools::file_path_sans_ext(input$folder_three$name)
      
      missing_codes_three <- setdiff(file_codes_three, codes)
      missing_files_three <- setdiff(codes, file_codes_three)
      
      if (length(missing_codes_three) > 0) {
        showNotification(
          paste("The following spectra in folder 3 do not have corresponding codes in the metadata file: ",
                paste(missing_codes_three, collapse = ", ")),
          type = "error",
          duration = NULL)
        folder_error <- TRUE
        #return(NULL)
      }
      
      if (length(missing_files_three) > 0) {
        showNotification(
          paste("The following codes with metadata in the master file do not have corresponding spectra in folder three: ",
                paste(missing_files_three, collapse = ", ")
          ),
          type = "error",
          duration = NULL
        )
        folder_error <- TRUE
        #return(NULL)
      }
    }
    
    
    # ends the folder check while showing all the notifications
    if(folder_error){
      return(NULL)
    }
    
    # do more checks HERE
    
    # this is "here"
    
    
    
    # Step 1. Initiating database metadata by reading master_file.
    LibMaster <- data.table::as.data.table(meta_dat)
    nCompounds <- dim(LibMaster)[1]
    
    # Step two was validation, done above
    
    if(input$type_of_database == "Basic"){
      
      # Step 3: collect the spectra
      
      nEnergyFolders <- sum(contains_folder_one, contains_folder_two, contains_folder_three)
      mz_res <- mz_tolerance()
      eFixed <- character(nEnergyFolders)
      
      # a bunch of definitions copy pasted from the original script
      
      peaks = character(nEnergyFolders)       # individual peak list
      numPeaks = numeric(nEnergyFolders)      # number of peaks per list
      Energies <- vector("list", nCompounds)        # likely fixed at nEnergyFolders
      PeakLists <- vector("list", nCompounds)       # combined peak lists for a given entry
      NumSpectra = numeric(1);                # likely fixed at nEnergyFolders
      NumPeaksList <- vector("list", nCompounds)    # likely fixed at nEnergyFolders
      DimerProb = numeric(nCompounds)         # check if dimer
      DimerErrorProb = numeric(nCompounds)    # abundance ratio of dimer : base peak
      MassCaliError = numeric(nCompounds)     # mass error
      PotentialBPs = numeric(nCompounds)      # number of "potential" base peaks
      BP = numeric(nCompounds)                # m/z value of base peak in +30 V spectrum
      PotentialErrors = numeric(nCompounds)   # potential error spectra (mass calibration)
      formulasubscript = character(nCompounds)# formula with subscript # added by Stephen
      polarity = character(nCompounds)        # the library's polarity (positive or negative)
      sourceGas = character(nCompounds)       # the library's source gas (He, N, etc.)
      IsotopeRatioSim = numeric(nCompounds)   # the similarity between measured and computed protonated molecule envelope
      CollapsedSpectra = character(nCompounds) # a placeholder for collapsed mass spectra
      
      FragmentationMetrics = numeric(nCompounds) # a set of internal metrics to measure fragmentation consistency
      PotentialErrorsFM1 = numeric(nCompounds) # potential errors based on fragmentation inconsistency
      
      
      #Read in the files
      
      LOW_spectra <- "folder_one"
      MID_spectra <- NULL
      HIGH_spectra <- NULL
      
      if(contains_folder_two){
        MID_spectra <- "folder_two"
      }
      if(contains_folder_three){
        HIGH_spectra <- "folder_three"
      }
      
      inputed_spectra <- c(LOW_spectra, MID_spectra, HIGH_spectra)
      
      for (i in seq_along(inputed_spectra)) {
        uploaded_spectra_file <- input[[inputed_spectra[i]]]
        
        if (is.null(uploaded_spectra_file)) {
          next
        }
        
        for (j in seq_len(nrow(uploaded_spectra_file))) {
          
          filename <- uploaded_spectra_file$name[j]
          h <- strsplit(filename, "\\.")[[1]][1]
          k <- which(LibMaster[, Code] == h)
          preMZ <- LibMaster[k, 3][[1]]
          data <- readLines(uploaded_spectra_file$datapath[j])
          b <- asm_ListCreator_v1.02(data, preMZ)
          
          #############################################################################################################################
          # identical() says that they are identical when this code is part of it, and FALSE without it, but *I* don't see any differences, and setdiff gives a list of 0       
          # 
          if (is.null(PeakLists[[k]])) {
            PeakLists[[k]] <- vector("list", nEnergyFolders)
            NumPeaksList[[k]] <- numeric(nEnergyFolders)
          }
          #########################################################################################################################  
          
          if (i == 1) {
            MassCaliError[k] <- as.numeric(b[[2]])
            DimerProb[k] <- as.numeric(b[[3]])
            DimerErrorProb[k] <- as.numeric(b[[5]])
            PotentialBPs[k] <- as.numeric(b[[4]])
            BP[k] <- as.numeric(b[[6]])
            
            if (abs(as.numeric(b[[2]])) > mz_res) {
              PotentialErrors[k] <- 1
            }
          }
          
          NumSpectra[k] <- nEnergyFolders
          PeakLists[[k]][[i]] <- b[[1]]
          NumPeaksList[[k]][i] <- length(data)
          Energies[[k]] <- list(eFixed)
        }
      }
      

      # Step 4: Creating library for the DIT
      
      Structure_gen = character(nCompounds)
      for(i in 1:nCompounds){
        Structure_gen[i] = list(c("",                                                                     
                                  "Arun's Random Structure Block",                                               
                                  "",                                                                     
                                  " 4 1  0  0  0  0  0  0  0  0999 V2000",                              
                                  "    0  1    0.0000 No   0  0  0  0  0  0  0  0  0  0  0  0",
                                  "    0  -1    0.0000 Structure   0  0  0  0  0  0  0  0  0  0  0  0",
                                  "    0   2    0.0000 .   0  0  0  0  0  0  0  0  0  0  0  0",
                                  "    0  -2    0.0000 .   0  0  0  0  0  0  0  0  0  0  0  0",
                                  "  1  2  1  0  0  0  0",                                                
                                  "M  END"))
      }
      
      Energies_306090 = rep(list(c("+30 V","+60 V","+90 V")),nCompounds)
      Library_RDT = data.table::as.data.table(cbind(LibMaster[,1],
                                                    LibMaster[,2],
                                                    'Cas #' = rep("NA",nCompounds), 
                                                    'Synonymns' = rep("NA",nCompounds),
                                                    'IUPAC/ Formal Name' = rep("NA",nCompounds),
                                                    'Formula' = rep("NA",nCompounds),
                                                    'AccurateMolecularMass' = c(LibMaster[,3][[1]]),
                                                    'Class' = rep("NA",nCompounds), 
                                                    'Canonical SMILES' = rep("NA",nCompounds), 
                                                    'InChi Code' = rep("NA",nCompounds),
                                                    'InChIKey' = rep("NA",nCompounds),
                                                    'MW_gen' = c(LibMaster[,3][[1]]),
                                                    'AccurateMass_gen' = c(LibMaster[,3][[1]]),
                                                    'PrecursorMZ_gen' = c(LibMaster[,3][[1]]),
                                                    'Energies' = Energies_306090,
                                                    NumPeaksList,
                                                    NumSpectra,
                                                    PeakLists,
                                                    'SMILES' = rep("NA",nCompounds),
                                                    'InChIKey_gen' = rep("NA",nCompounds),
                                                    MassCaliError,
                                                    DimerProb,
                                                    DimerErrorProb,
                                                    BP,
                                                    'theoBP' = rep(NaN,nCompounds),
                                                    'theoBP_MolForm' = rep("NA",nCompounds),
                                                    PotentialBPs,
                                                    PotentialErrors,
                                                    FragmentationMetrics,
                                                    PotentialErrorsFM1,
                                                    Structure_gen,
                                                    'RefinedAnnotations' = rep("NA",nCompounds),
                                                    'NoiseMetric' = rep("NA",nCompounds),
                                                    'formulasubscript' = rep("NA",nCompounds),
                                                    'polarity' = rep("NA",nCompounds),
                                                    'sourceGas' = rep("NA",nCompounds),
                                                    'IsotopeRatioSim' = rep(NaN,nCompounds),
                                                    'pmMajIsoMZ30V' = rep(NaN,nCompounds),
                                                    'pmMajIsoAb30V' = rep(NaN,nCompounds),
                                                    'bpMajIsoMZ30V' = rep(NaN,nCompounds),
                                                    'bpMajIsoAb30V' = rep(NaN,nCompounds),
                                                    'mfMajIsoMZ30V' = rep(NaN,nCompounds),
                                                    'mfMajIsoAb30V' = rep(NaN,nCompounds),
                                                    'mfMZ30V' = rep(NaN,nCompounds)
      ))
      
      
      RDT_database(Library_RDT)
      
      shinyjs::click("download_library")
      
      # the else statement should *technically* be above the click(download_library), but because I don't have anything, there's no point
    } else{

      ## Definition of monoisotpic mass for atoms of interest
      child_path = paste0(parent_path,"/asm_MIM_Definitions.R")
      source(child_path)
      
      # step 3a: generating structures for each compound from its SMILES (assumed correct)
      
      SMILES = character(nCompounds)  # remove salt from smiles (splitting at period)
      InChIKey_gen = character(nCompounds)
      Structure_gen = character(nCompounds)
      Structure_genH = character(nCompounds)
      
      
      for (i in 1:nCompounds){
        a = strsplit(as.character(LibMaster[i,"Canonical_SMILES"]),"\\.")[[1]]  # get rid of salt from SMILES. Ideally this won't be necessary
        
        if(is.na(a[1])==FALSE){
          SMILES[i] = a[1];
          sink("temp.smiles")
          cat(paste0(a[1],"\n"));
          sink()
          
          c1 = paste0("obabel -ismiles temp.smiles -osdf -Otemp.sdf -h --gen2D")
          if(user_OS=="windows"){
            system(c1,show.output.on.console = FALSE)
          } else {
            system(c1)
          }
          
          lsdf = readLines("temp.sdf")
          Structure_genH[i] = list(lsdf[1:(length(lsdf)-1)])
          unlink("temp.sdf")
          
          c1 = paste0("obabel -ismiles temp.smiles -osdf -Otemp.sdf --AddPolarH --gen2D")
          if(user_OS=="windows"){
            system(c1,show.output.on.console = FALSE)
          } else {
            system(c1)
          }
          
          lsdf = readLines("temp.sdf")
          Structure_gen[i] = list(lsdf[1:(length(lsdf)-1)])
          unlink("temp.sdf")
          
          c2 = paste0("obabel -ismiles temp.smiles -oinchikey -Otemp.inchikey")
          if(user_OS=="windows"){
            system(c2,show.output.on.console = FALSE)
          } else {
            system(c2)
          }
          InChIKey_gen[i] = readLines("temp.inchikey")
          unlink("temp.inchikey")
          
          unlink("temp.smiles")
        }
      }
      
      # step 3b: Generating mass values for each compound from Structure_genH
      
      Formula_gen = character(nCompounds)
      AccurateMass_gen = numeric(nCompounds)
      PrecursorMZ_gen = numeric(nCompounds)
      MW_gen = numeric(nCompounds)
      
      
      for (i in 1:nCompounds){
        Formula_gen[i] = a = asm_struc2formula(Structure_genH[i][[1]]);
        AccurateMass_gen[i] = asm_MonoisotopicMass(formula = asm_ListFormula(a))
        
        if(input$ion_mode=="Positive"){
          PrecursorMZ_gen[i] = asm_MonoisotopicMass(formula = asm_ListFormula(paste0(a,"+H"))) # this adds a hydrogen before computing precursor mass
        } else {
          PrecursorMZ_gen[i] = asm_MonoisotopicMass(formula = asm_ListFormula(a)) - asm_MonoisotopicMass(formula = asm_ListFormula("H")) # this subtracts a hydrogen before computing precursor mass
        }
        
        MW_gen[i] = asm_MonoisotopicMass(formula = asm_ListFormula(a),
                                         isotopes = c(C=anC,H=anH,D=anD,O=anO,N=anN,S=anS,P=anP,Br=anBr,Cl=anCl,F=anF,Si=anSi, I=anI, Na=anNa, K=anK));
      }
      
      # step 4: Collecting spectra from folders, creating new columns for data table library structure.
      
      # copy and pasted from the "basic" section, which worked, so I hope it works here too :P
      
      nEnergyFolders <- sum(contains_folder_one, contains_folder_two, contains_folder_three)
      mz_res <- mz_tolerance()
      eFixed <- character(nEnergyFolders)
      
      # a bunch of definitions copy pasted from the original script
      
      
      
      
      peaks = character(nEnergyFolders)       # individual peak list
      numPeaks = numeric(nEnergyFolders)      # number of peaks per list
      Energies <- vector("list", nCompounds)        # likely fixed at nEnergyFolders
      PeakLists <- vector("list", nCompounds)       # combined peak lists for a given entry
      NumSpectra = numeric(1);                # likely fixed at nEnergyFolders
      NumPeaksList <- vector("list", nCompounds)    # likely fixed at nEnergyFolders
      DimerProb = numeric(nCompounds)         # check if dimer
      DimerErrorProb = numeric(nCompounds)    # abundance ratio of dimer : base peak
      MassCaliError = numeric(nCompounds)     # mass error
      PotentialBPs = numeric(nCompounds)      # number of "potential" base peaks
      BP = numeric(nCompounds)                # m/z value of base peak in +30 V spectrum
      PotentialErrors = numeric(nCompounds)   # potential error spectra (mass calibration)
      formulasubscript = character(nCompounds)# formula with subscript # added by Stephen
      polarity = character(nCompounds)        # the library's polarity (positive or negative)
      sourceGas = character(nCompounds)       # the library's source gas (He, N, etc.)
      IsotopeRatioSim = numeric(nCompounds)   # the similarity between measured and computed protonated molecule envelope
      CollapsedSpectra = character(nCompounds) # a placeholder for collapsed mass spectra
      
      FragmentationMetrics = numeric(nCompounds) # a set of internal metrics to measure fragmentation consistency
      PotentialErrorsFM1 = numeric(nCompounds) # potential errors based on fragmentation inconsistency
      
      
      #Read in the files
      
       LOW_spectra <- "folder_one"
       MID_spectra <- NULL
       HIGH_spectra <- NULL
       
       if(contains_folder_two){
         MID_spectra <- "folder_two"
       }
       if(contains_folder_three){
         HIGH_spectra <- "folder_three"
       }
       
       inputed_spectra <- c(LOW_spectra, MID_spectra, HIGH_spectra)
       

       for (i in seq_along(inputed_spectra)) {
         uploaded_spectra_file <- input[[inputed_spectra[i]]]
         
         if (is.null(uploaded_spectra_file)) {
           next
         }
         
         for (j in seq_len(nrow(uploaded_spectra_file))) {
           
           filename <- uploaded_spectra_file$name[j]
           h <- strsplit(filename, "\\.")[[1]][1]
           k <- which(LibMaster[, Code] == h)
           preMZ = PrecursorMZ_gen[k]
           data <- readLines(uploaded_spectra_file$datapath[j])
           b <- asm_ListCreator_v1.02(data, preMZ)
         
          #############################################################################################################################
           # identical() says that they are identical when this code is part of it, and FALSE without it, but *I* don't see any differences, and setdiff gives a list of 0       
          
          if (is.null(PeakLists[[k]])) {
            PeakLists[[k]] <- vector("list", nEnergyFolders)
            NumPeaksList[[k]] <- numeric(nEnergyFolders)
          }
           #########################################################################################################################  
           
           if (i == 1) {
             MassCaliError[k] <- as.numeric(b[[2]])
             DimerProb[k] <- as.numeric(b[[3]])
             DimerErrorProb[k] <- as.numeric(b[[5]])
             PotentialBPs[k] <- as.numeric(b[[4]])
             BP[k] <- as.numeric(b[[6]])
             
             if (abs(as.numeric(b[[2]])) > mz_res) {
               PotentialErrors[k] <- 1
             }
           }
           
           NumSpectra[k] <- nEnergyFolders
           PeakLists[[k]][[i]] <- b[[1]]
           NumPeaksList[[k]][i] <- length(data)
           Energies[[k]] <- list(eFixed)
         }
       }
       
       #print(Energies)
      
      # step 5a: Building collapsed mass spectra
       
       if(input$selected_build_type == "Collapsed"){
         
         for(i in 1:nCompounds){
           CollapsedSpectra = list(asm_CollapsedSpectra(PeakLists[i]))
           for(j in 1:nEnergyFolders){
             PeakLists[i][[1]][j][[1]] = CollapsedSpectra[[1]];
           }
         }
       }
       
       # step 5b: Checks for spectral 'consistency' as outlined in application notes 
       
       # yeah idk here; I'm not 100% sure what the code is doing, so idk what to change
       
       if(input$selected_build_type == "Traditional"){
       
    #    for(i in 1:nCompounds){
    #      FragmentationMetrics[i] = asm_fragConsistencyChecker(PeakLists[i][[1]])
    #    }
    #    
    #    EnergyNumeric = numeric(nEnergyFolders)
    #    for(i in 1:nEnergyFolders){
    #      if (Energies[[1]][i]==RequireFolderStructure[1]){
    #        b = 1
    #      } else if (Energies[[1]][i]==RequireFolderStructure[2]){
    #        b = 2
    #      } else if (Energies[[1]][i]==RequireFolderStructure[3]){
    #        b = 3
    #      }
    #      EnergyNumeric[i] = as.numeric(b)
    #    }
    #    
    #    EnergyOrder = order(EnergyNumeric)
    #    
    #    for (i in 1:nCompounds){
    #      PotentialErrorsFM1[i] = sum(abs(FragmentationMetrics[[i]]-EnergyOrder))
    #    }
    # }
       
       }
       
       
       # Step 6: Computing possible peak annotations
       
       PossibleAnnotations = character(nCompounds)
       for(i in 1:nCompounds){
         PossibleAnnotations[i] = list(asm_AllPeaksGenerator(Structure_genH[[i]],input$ion_mode))
       }
       
       # Step 7a: Annotating peaks
       
       RefinedAnnotations = character(nCompounds)
       for(i in 1:nCompounds){
         c = character(nEnergyFolders)
         for(j in 1:nEnergyFolders){
           spec_mz = PeakLists[[i]][j][[1]][,1]
           spec_ab = PeakLists[[i]][j][[1]][,2]
           struc_info = PossibleAnnotations[[i]]
           mc_error = mz_res; #MassCaliError[i]
           c[j] = list(asm_PeakAnnotator(spec_mz,spec_ab,struc_info,mc_error))
         }
         RefinedAnnotations[i] = list(c)
       }
       
       # Step 7b: Identifying noisy spectra (unannotated peak intensity > 0.45%)
       
       NoiseMetric = character(nCompounds)
       
       for(i in 1:nCompounds){
         c = character(nEnergyFolders)
         for(j in 1:nEnergyFolders){
           unannotatedPeaks = which(RefinedAnnotations[[i]][[j]]=="")
           c[j] = sum(PeakLists[[i]][j][[1]][unannotatedPeaks,2]) / sum(PeakLists[[i]][j][[1]][,2])
         }
         NoiseMetric[i] = list(c)
       }
       
       # Step 7c: Compute theoretical base peak m/z value for each compound using the molecular formula annotation and isopattern. If annotation not available, use measured BP.
       
       bpMajIsoMZ30V = numeric(nCompounds);
       bpMajIsoAb30V = numeric(nCompounds);
       
       theoBP = numeric(nCompounds);
       theoBP_MolForm = character(nCompounds);
       noAnnoAvailable = NULL;
       for(i in 1:nCompounds){
         bp_index = which.max(PeakLists[[i]][1][[1]][,2])
         anno = RefinedAnnotations[i][[1]][[1]][bp_index][[1]][1]
         suppressWarnings(
           if(anno==""){
             noAnnoAvailable = c(noAnnoAvailable,i)
             theoBP[i] = NA;
             bpMajIsoMZ30V[i] = 0;
             bpMajIsoAb30V[i] = 0;
           } else {
             
             anno = gsub("\\(2\\)H","D",anno)
             anno_string = strsplit(anno,"")[[1]]
             bracketStart = which(anno_string=="(")
             bracketEnd = which(anno_string==")")
             
             thingsToRemove = NULL;
             for(j in 1:length(bracketStart)){
               thingsToRemove = c(thingsToRemove,seq(bracketStart[j],bracketEnd[j]))
             }
             
             anno_clean = anno_string[-thingsToRemove]
             anno_update = paste(anno_clean,collapse="")
             
             tBP = isopattern(isotopes,anno_update,charge=0)
             theoBP[i] = as.numeric(tBP[[1]][1,1])[[1]]
             theoBP_MolForm[i] = anno_update
             
             iPattern = tBP;
             iPattern = iPattern[[1]]
             iPattern[,2] = iPattern[,2]*100/iPattern[1,2]
             iPattern = iPattern[-1,]
             
             bpMajIsoMZ30V[i] = iPattern[which.max(iPattern[,2]),1]
             bpMajIsoAb30V[i] = max(iPattern[,2])
             
           }
         )
       }
       
       sink("SpectraWithoutBPannotation.txt")
       for(i in noAnnoAvailable){
         cat(LibMaster[i,1][[1]]);
         cat("\n");
       }
       sink()
       
       
       pmMajIsoMZ30V = numeric(nCompounds);
       pmMajIsoAb30V = numeric(nCompounds);
       
       
       # Step 7d. Compute isotope ratio errors for protonated molecule using the molecular formula annotation and isopattern
       
       for(i in 1:nCompounds){
         if(input$ion_mode=="Positive"){
           formula = paste0(Formula_gen[i][[1]],"+H");
         } else{
           formula = paste0(Formula_gen[i][[1]],"-H")
         }
         
         iPattern = isopattern(isotopes,formula,charge=0)
         
         relevant_peaks = which(iPattern[[1]][,2]>1)
         theoAb = iPattern[[1]][relevant_peaks,2]
         
         refAb = numeric(length(relevant_peaks))
         for(j in 1:length(relevant_peaks)){
           potentialMZs = which(abs(PeakLists[[i]][1][[1]][,1]-iPattern[[1]][relevant_peaks[j],1])<=mz_res)
           if(length(potentialMZs)==0) next
           refAb[j] = max(PeakLists[[i]][1][[1]][potentialMZs,2])
         }
         IsotopeRatioSim[i] = asm_CosSim(theoAb,refAb);
         
         iPattern = iPattern[[1]]
         iPattern[,2] = iPattern[,2]*100/iPattern[1,2]
         
         iPattern = iPattern[-1,]
         pmMajIsoMZ30V[i] = iPattern[which.max(iPattern[,2]),1]
         pmMajIsoAb30V[i] = max(iPattern[,2])
         
         
         
       }
       
       # Step 7e. Compute theoretical m/z value for Major Fragment 1 for each compound using the molecular formula annotation and isopattern. If annotation not available, use measured BP
       
       
       mfMZ30V = numeric(nCompounds);
       mfMajIsoMZ30V = numeric(nCompounds);
       mfMajIsoAb30V = numeric(nCompounds);
       
       noAnnoAvailable = NULL;
       for(i in 1:nCompounds){
         mf_index1 = which((PeakLists[[i]][1][[1]][,2]/max(PeakLists[[i]][1][[1]][,2]))>0.05)
         mf_index2 = which(PeakLists[[i]][1][[1]][,1] < (PrecursorMZ_gen[i]-1))
         mf_index3 = intersect(mf_index1,mf_index2)
         mf_index4 = which.max(PeakLists[[i]][1][[1]][mf_index3,2])
         
         mf_index = mf_index3[mf_index4]
         
         if(length(mf_index)>0){
           anno = RefinedAnnotations[i][[1]][[1]][mf_index][[1]][1]
         } else {
           anno = ""
         }
         
         suppressWarnings(
           if(anno==""){
             mfMajIsoMZ30V[i] = 0;
             mfMajIsoAb30V[i] = 0;
           } else {
             
             anno_string = strsplit(anno,"")[[1]]
             bracketStart = which(anno_string=="(")
             bracketEnd = which(anno_string==")")
             
             thingsToRemove = NULL;
             for(j in 1:length(bracketStart)){
               thingsToRemove = c(thingsToRemove,seq(bracketStart[j],bracketEnd[j]))
             }
             
             anno_clean = anno_string[-thingsToRemove]
             anno_update = paste(anno_clean,collapse="")
             
             iPattern = isopattern(isotopes,anno_update,charge=0)
             
             mfMZ30V[i] = iPattern[[1]][1,1]
             
             iPattern = iPattern[[1]]
             iPattern[,2] = iPattern[,2]*100/iPattern[1,2]
             
             iPattern = iPattern[-1,]
             
             mfMajIsoMZ30V[i] = iPattern[which.max(iPattern[,2]),1]
             mfMajIsoAb30V[i] = max(iPattern[,2])
             
           }
         )
       }
       
       
       # Step 8a. Generating database in data.table format (internal use)
       
       source("source/asm_Functions/sst_makesubscript.R", local = TRUE)
       
       # print(Library_RDT)
       for (i in 1:length(formulasubscript)){
         word = as.character(Formula_gen[i])
         formulasubscript[i] = makesubscript(word)
       }
       
       polarity = rep(input$ion_mode,nCompounds);
       sourceGas = rep(input$gas_phase,nCompounds);
       
       
       
       Library_RDT = as.data.table(cbind(LibMaster,
                                         Formula_gen,
                                         MW_gen,
                                         AccurateMass_gen,
                                         PrecursorMZ_gen,
                                         Energies,
                                         NumPeaksList,
                                         NumSpectra,
                                         PeakLists,
                                         SMILES,
                                         InChIKey_gen,
                                         MassCaliError,
                                         DimerProb,
                                         DimerErrorProb,
                                         BP,
                                         theoBP,
                                         theoBP_MolForm,
                                         PotentialBPs,
                                         PotentialErrors,
                                         FragmentationMetrics,
                                         PotentialErrorsFM1,
                                         Structure_gen,
                                         RefinedAnnotations,
                                         NoiseMetric,
                                         formulasubscript,
                                         polarity,
                                         sourceGas,
                                         IsotopeRatioSim,
                                         pmMajIsoMZ30V,
                                         pmMajIsoAb30V,
                                         bpMajIsoMZ30V,
                                         bpMajIsoAb30V,
                                         mfMajIsoMZ30V,
                                         mfMajIsoAb30V,
                                         mfMZ30V))
       
       LibraryCats = colnames(Library_RDT)
       iName = which(LibraryCats=="Name")
       
       iFormula = which(LibraryCats=="Formula_gen");
       colnames(Library_RDT)[iFormula]="Formula"
       
       iInChIKey = which(LibraryCats=="InChIKey_gen");
       colnames(Library_RDT)[iInChIKey]="InChIKey"
       
       
       Energies_306090 = rep(list(c("+30 V","+60 V","+90 V")),nCompounds)
       Library_RDT_DIT_3.22 = cbind(Library_RDT[,1],
                                    Library_RDT[,3],
                                    'CAS #' = rep("NA",dim(Library_RDT)[1]),
                                    Library_RDT[,4],
                                    Library_RDT[,5],
                                    Library_RDT[,8],
                                    'Accurate Molecular Mass' = Library_RDT[,10], 
                                    Library_RDT[,6],
                                    Library_RDT[,2],
                                    'InChi Code' = rep("NA",dim(Library_RDT)[1]),
                                    Library_RDT[,17],
                                    Library_RDT[,9],
                                    Library_RDT[,10],
                                    Library_RDT[,11],
                                    Energies = Energies_306090,
                                    Library_RDT[,13],
                                    Library_RDT[,14],
                                    Library_RDT[,15],
                                    Library_RDT[,16],
                                    'InChIKey_gen' = Library_RDT[,17],
                                    Library_RDT[,18],
                                    Library_RDT[,19],
                                    Library_RDT[,20],
                                    Library_RDT[,21],
                                    Library_RDT[,22],
                                    Library_RDT[,23],
                                    Library_RDT[,24],
                                    Library_RDT[,25],
                                    Library_RDT[,26],
                                    Library_RDT[,27],
                                    Library_RDT[,28],
                                    Library_RDT[,29],
                                    Library_RDT[,30],
                                    Library_RDT[,31],
                                    Library_RDT[,32],
                                    Library_RDT[,33],
                                    Library_RDT[,34],
                                    Library_RDT[,35],
                                    Library_RDT[,36],
                                    Library_RDT[,37],
                                    Library_RDT[,38],
                                    Library_RDT[,39],
                                    Library_RDT[,40],
                                    Library_RDT[,41]
       );
       
       
       
       setnames(Library_RDT_DIT_3.22,old="IUPAC_Formal_Name",new="IUPAC/ Formal Name")
       setnames(Library_RDT_DIT_3.22,old="Accurate Molecular Mass.AccurateMass_gen",new="Accurate Molecular Mass")
       setnames(Library_RDT_DIT_3.22,old="Canonical_SMILES",new="Canonical SMILES")
       setnames(Library_RDT_DIT_3.22,old="InChIKey_gen.InChIKey",new="InChIKey_gen")
       
       RDT_database(Library_RDT_DIT_3.22)
       
       # Step 8b: Creating a list of the codes to review for spectral issues
       
       a1 = which(abs(MassCaliError)>0.005)
       a2 = which(IsotopeRatioSim < IsotopeRatioThreshold)
       a = which(DimerErrorProb > DEThreshold)
       b = which(PotentialErrorsFM1!=0)
       maxNE = numeric(nCompounds)
       
       
       for(i in 1:nCompounds){
         maxNE[i] = max(as.numeric(NoiseMetric[[i]][1]),as.numeric(NoiseMetric[[i]][2]),as.numeric(NoiseMetric[[i]][3]))
       }
       c = which(maxNE>NoiseThreshold)
       d = unique(c(a1,a,b,c,a2))
       d = sort(d)
       
       if(length(d)>0){
         RevisionSheet = paste0(database_name(),"_spec2review.txt")
         sink(RevisionSheet)
         for(i in 1:length(d)){
           
           comment = NULL;
           if (d[i] %in% a1){
             comment = paste0(comment,"Mass Cali Error-")
           }
           if (d[i] %in% a){
             comment = paste0(comment,"Dimer Error-")
           }
           if (d[i] %in% b){
             comment = paste0(comment,"Fragmentation Calibration Error-")
           }
           if (d[i] %in% c){
             comment = paste0(comment,"Potential Noise-")
           }
           if (d[i] %in% a2){
             comment = paste0(comment,"Dissimilar isotopic pattern Molecular ion-")
           }
           comment = paste0(comment,"\n")
           notif <- (paste0(d[i],"\t",LibMaster[d[i],Code],"\t",LibMaster[d[i],Name],"\t",comment))
           cat(notif)
           showNotification(ui = notif, type = "message", duration = NULL)
         }
         sink()
       }
       # Step 8c: Creating a list of the codes to review for missing structure information
       
       RevisionSheet2 = paste0(database_name(),"_missingStructures.txt")
       sink(RevisionSheet2)
       for(i in 1:nCompounds){
         SBlock = "";  SBlock = as.character(unlist(Library_RDT[i,"Structure_gen"]))
         if(length(grep("nan",SBlock))!=0){
           cat(paste0(LibMaster[i,Code],"\t",LibMaster[i,Name],"\n"))
           
           showNotification(ui = paste0(LibMaster[i,Code],"\t",LibMaster[i,Name]))
         }
       }
       sink()
       
       
       #Step 9: Generating database in General Purpose text format (sdf)
       
       if(input$create_SDF == "Yes"){
         Library = Library_RDT
         SDFfilename = paste0(database_name(),".SDF")
         sink(SDFfilename)
         for(i in 1:nCompounds){
           cname = "";   cname = as.character(Library[i,"Name"])
           cname = asm_GreekLetterConverter(cname)
           fname = "";   fname = as.character(Library[i,"IUPAC_Formal_Name"]);
           fname = asm_GreekLetterConverter(fname);
           
           syns = "";    syns = strsplit(as.character(Library[i,"Synonyms"]),";")[[1]]
           accMass = 0;  accMass = Library[i,"AccurateMass_gen"]
           preMZ = 0;    preMZ = Library[i,"PrecursorMZ_gen"]
           mw = 0;       mw = Library[i,"MW_gen"]
           inchi = "";   inchi = as.character(Library[i,"InChIKey"])
           #casno = "";   casno = as.character(Library[i,"CAS #"])
           formula = ""; formula = as.character(Library[i,"Formula"])
           ID = "";      ID = as.character(Library[i,"Code"])
           SBlock = "";  SBlock = as.character(unlist(Library[i,"Structure_gen"]))
           
           e = Library[i,Energies][[1]]
           
           for(j in 1:length(e)){
             if(length(grep("nan",SBlock))!=0){
               cat("Spectrum with No Structure\n\n")
               
               cat("No Structure\n")
               cat("0  0  0  0  0  0  0  0  0  0  0\n")
             } else {
               cat(SBlock,sep="\n")
             }
             
             cat(">  <NAME> \n")
             cat(paste(cname," ",e[j],"\n\n",sep=""))  # List the common name. Change to fname for formal name
             cat(">  <ION_MODE> \n", input$ion_mode ,"\n\n") # CONSTANTS FOR THIS DATA SET
             cat(">  <PRECURSOR_TYPE> \n[M+H]+ \n\n") # CONSTANTS FOR THIS DATA SET
             cat(">  <COLISION_GAS> \n", input$gas_phase ,"\n\n") # CONSTANTS FOR THIS DATA SET
             
             cat(paste(">  <PrecursorMZ>\n", round(preMZ,4),"\n\n",sep="")) # round to 4 decimal places
             cat(paste(">  <Synonyms>\n",fname," ",e[j],"\n",sep=""))
             
             endk = length(syns)
             for(k in 1:endk){
               if(!is.na(syns[k])) {
                 csyns = asm_GreekLetterConverter(syns[k])
                 cat(paste(csyns," ",e[j],"\n",sep=""))
               }
             }
             cat("\n")
             
             cat(paste(">  <InChIKey>\n",inchi,"\n\n",sep=""))
             cat(paste(">  <Formula>\n",formula,"\n\n",sep=""))
             cat(paste(">  <MW>\n",mw,"\n\n",sep=""))
             cat(paste(">  <ExactMass>\n", round(accMass,4),"\n\n",sep=""))
             # if(!is.na(casno)){
             #   cat(paste(">  <CASNO>\n ",casno,"\n\n",sep=""))
             # }
             cat(paste(">  <ID>\n",ID," ",e[j],"\n\n",sep=""))
             cat(paste(">  <Num Peaks>\n ", Library[i,NumPeaksList[[1]]][j],"\n\n",sep="" ))
             
             cat(">  <MASS SPECTRAL PEAKS>\n")
             mz = Library[i,PeakLists][[1]][[j]][,1]
             ab = Library[i,PeakLists][[1]][[j]][,2]
             an = Library[i,RefinedAnnotations][[1]][[j]]
             for(k in 1:length(mz)){
               cat(paste0(round(mz[k],4)," ",round(max(0,ab[k]),4)," \"")) # round to 4 decimal places
               # # printing annotations in the sdf file.. does not work with MS Search - remove?
               # for(l in 1:length(an[k][[1]])){
               #   cat(paste0(an[k][[1]][l]," "))
               # }
               cat("\" \n")
             }
             cat("\n")
             cat("$$$$\n")
             
           }
           
         }
         sink()
       }
       
       
       # Step 10: Generating formula lists in text format (txt)
       
       if(input$create_formula_list == "Yes"){
         Library = Library_RDT
         txtfilename = paste0(database_name(),"_molecularFormula_list.txt")
         sink(txtfilename)
         for(i in 1:nCompounds){
           cname = "";   cname = as.character(Library[i,"Name"])
           cname = asm_GreekLetterConverter(cname)
           formula = ""; formula = as.character(Library[i,"Formula"])
           cat(paste0(cname,"\t",formula,"\n"))
         }
         sink()
         
         txtfilename = paste0(database_name(),"_BPFormula_list.txt")
         sink(txtfilename)
         for(i in 1:nCompounds){
           cname = "";   cname = as.character(Library[i,"Name"])
           cname = asm_GreekLetterConverter(cname)
           formula = ""; formula = as.character(Library[i,"theoBP_MolForm"])
           a = strsplit(formula,"\\+")[[1]]
           if(length(a)==2){
             formula = a[1]
             cat(paste0(cname,"\t",formula,"\n"))
             next
           }
           
           a = strsplit(formula,"\\-")[[1]];
           if(length(a)==2){
             formula = a[1];
             cat(paste0(cname,"\t",formula,"\n"))
             next
           }
           
           cat(paste0(cname,"\t",formula,"\n"))
           
         }
         sink()
       }
       
       
       shinyjs::click("download_library")
       
      #print("no error")
      #return(NULL)
    }
    
  }) # ends the "results" section
  
  
  
  output$download_library <- downloadHandler(
    
    filename = function() {
      paste0(database_name(), ".rds")
    },
    content = function(file){
      saveRDS(RDT_database(),file)
    }
  )
  
  
  
  
  # for page two
  
  output$text <- renderText({ 
    "Hello world" # put all of the "about" text here 
  }) 
  
}
shinyApp(ui = ui, server = server)
