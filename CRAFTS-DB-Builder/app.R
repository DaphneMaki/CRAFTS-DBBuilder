library(shiny)
library(shinyjs)

#needed functions, idk if I can use "source" so just putting them here

asm_ListCreator_v1.02 <- function(x,preMZ){
  
  test = which(x=="");
  if(length(test)>=1){
    x = x[-test]
  }
  
  data = x;
  a = NULL;
  for(i in 1:length(data)){
    b = strsplit(data[i],"")[[1]][1]
    if(is.na(suppressWarnings(as.numeric(b)))){
      a = c(a,i);
    } 
  }
  if(length(a)>0){
    data = data[-a]  
  }
  
  x = data
  
  rData = NULL
  mz = numeric(length(x))
  ab = numeric(length(x))
  
  
  
  for(i in 1:length(x)){
    a = strsplit(x[i],"\t")[[1]];
    mz[i] = as.numeric(a[1]);
    ab[i] = as.numeric(a[2]);
    #rData = paste(rData,a[1]," ",a[2],";",sep="")
  }
  
  bp = which(ab==max(ab))     # index of observed base peak
  j = which(ab > 0.6*max(ab)) # there might be more than one possible base peaks # 0.6 was arbitrary.. maybe should be a variable?
  
  mass_cali = preMZ - mz[j];
  min_mass_cali = mass_cali[order(abs(mass_cali))[1]]
  
  k = which(abs(preMZ-mz)==min(abs(preMZ-mz))) # index of likely molecular ion
  
  pdMH = 2*(preMZ-1.007276)+1.007276; # mass of a proton
  
  pdimer = max(0,(1-min(abs(pdMH-mz))))
  if(pdimer>0.8){
    l = which(abs(pdMH-mz)==min(abs(pdMH-mz)))
    pd_ratio = ab[l]/ab[k]
  } else {
    pd_ratio = 0 
  }
  
  a = list(cbind(mz,ab),min_mass_cali,pdimer,length(j),pd_ratio, mz[bp])
  
  return(a)
  
}


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
                                  label = "Please upload all files from your first subfolder", 
                                  accept = c(".txt"),  # defaults to only accept txt files, but it's easily bypassed so I need to make more checks
                                  multiple = TRUE), # allows the user to upload multiple files
                      uiOutput("folder_one_dropdown") # creates a drop down for all the uploaded files
               ),
               
               column(4,fileInput("folder_two",
                                  label = "Please upload all files from your second subfolder", 
                                  accept = c(".txt"), # defaults to only accept txt files, but it's easily bypassed so I need to make more checks
                                  multiple = TRUE), # allows the user to upload multiple files
                      uiOutput("folder_two_dropdown") # creates a drop down for all the uploaded files
               ), 
               
               column(4,fileInput("folder_three",
                                  label = "Please upload all files from your third subfolder",
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
             
             textInput("file_name", "Enter name for generated database files."),
             
             actionButton("run", "Run", class = "btn-block btn-lg btn-primary"),
             
             div(
               style = "position: absolute;
               left: -9999px;
               width: 1px;
               height: 1px;
               overflow: hidden;",
               downloadButton("download_rds", "Download RDS")
             ),
             
             #hidden(
               #downloadButton("download_rds", "Download RDS"),
               #),
             
             tableOutput("results") # this was for testing reactive elements and should be deleted when I'm done
    ), # end of page one
    
    # start of page two
    tabPanel("About",  textOutput("text") # add anything that needs to be added (i.e tables, graphs, ect) to page two here
    )
    
  )
) 
server <- function(input, output, session) {
  
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
    meta_dat
    
  })
  
  #display the codes from the metadata
  
  output$codes <- renderUI({
    
    meta_dat <- uploaded_metadata()
    
    req(meta_dat)
    
    # this part is the one that actually grabs the codes and displays them as a list
    codes <- meta_dat$Code
    
    if(length(codes) == 0){
      showNotification("No codes found in metadata",type = "error", duration = NULL)
      return(NULL)
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
        duration = NULL
      )
      
      return(NULL)
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
        duration = NULL
      )
      
      return(NULL)
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
        duration = NULL
      )
      return(NULL)
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
        duration = NULL
      )
    }
    
    if (length(missing_files) > 0) {
      showNotification(
        paste("The following codes with metadata in the master file do not have corresponding spectra in folder one: ",
              paste(missing_files, collapse = ", ")),
        type = "error",
        duration = NULL
      )
    }
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
        duration = NULL
      )
    }
    
    if (length(missing_files) > 0) {
      showNotification(
        paste("The following codes with metadata in the master file do not have corresponding spectra in folder two: ",
              paste(missing_files, collapse = ", ")),
        type = "error",
        duration = NULL
      )
    }
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
        duration = NULL
      )
    }
    
    if (length(missing_files) > 0) {
      showNotification(
        paste("The following codes with metadata in the master file do not have corresponding spectra in folder three: ",
              paste(missing_files, collapse = ", ")),
        type = "error",
        duration = NULL
      )
    }
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
  
  
  
  # the main part of the program, aka what happens when the "run" button is pressed
  RDT_database <- reactiveVal(NULL)
  
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
      showNotification( "Please upload files for LOW fragmentation spectra", type = "error", duration = NULL)
      return(NULL)
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
    
    
    #trying stuff out to read in the files; we'll see how it goes
    
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
    Library_RDT = as.data.table(cbind(LibMaster[,1],
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

    
    shinyjs::click("download_rds")
    
  }) # ends the "results" section
  
  
  
   output$download_rds <- downloadHandler(
     
     filename = function() {
       paste0(database_name(), ".rds")     },
     content = function(file){
       saveRDS(RDT_database(),file)
     }
     
       
   )
  
  
  # in this case, right now the code is outputing a table of the metadata, or an error if it's not a csv or xlsx file
  #output$results <- renderTable({
  #  results()
 # }) # This was for testing reactive elements and should be deleted when I'm done
  
  
  # for page two
  
  output$text <- renderText({ 
    "Hello world" # put all of the "about" text here 
  }) 
  
}
shinyApp(ui = ui, server = server)
