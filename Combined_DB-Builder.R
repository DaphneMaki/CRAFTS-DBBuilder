# Basic DB builder

# Setup

header <- "CRAFTS LAB Basic Databse1 Builder\nRevised December 5th, 2025.\n\nA basic builder with no quality checks."
cat(header)
cat("\n\n")

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

operation <- TRUE
while (operation){

type <- TRUE

while(type){
  cat("There are two types of Databases that can be made with this program.", "\n", "1. Baisc", "\n", "2. Full (Requires Open Babel)") 
  selected_DB_type <- readline(prompt="Please Select the Type of Database you wish to build: ")
  basic <- c("1", "BASIC", "B", "ONE")
  full <- c("2", "FULL", "F", "TWO")
  valid_types <- c(full, basic)
  
  upper_selected_type <- toupper(selected_DB_type)
  
  if(!upper_selected_type %in% valid_types){
    cat("\n")
    cat("INVALID INPUT. Try again.","\n", "\n") 
  } 
  
  ## Step 0: USER Data
  potential_master_files <- list.files(".",pattern=".xlsx")
  if(length(potential_master_files)<1){
    operation = asm_ErrorFlagFatal("Step 0. No potential master_files in current directory.")
    break
  } else {
    
    isError = TRUE
    while (isError){
      cat(paste0("There are ", length(potential_master_files), " potential master files currently in the directory.\n\n"))
      for(i in 1:length(potential_master_files)){
        cat(paste0(i,": ",potential_master_files[i],"\n"))
      }
      cat("\n")
      
      a <- readline(prompt="Choose Master File (integer value) for creating database: ")
      if (a %in% as.character(seq(1,length(potential_master_files)))){
        master_file = potential_master_files[as.numeric(a)]
        isError = FALSE
        cat("\n")
      } else {
        cat("\n")
        cat("INVALID INPUT. Try again.\n")
      }
    }
  }
  
  potential_main_folders = list.dirs(".",recursive=FALSE)
  a = which(potential_main_folders == "./source")
  if(length(a)>0) potential_main_folders = potential_main_folders[-a]
  a = which(potential_main_folders == "./.git")
  if(length(a)>0) potential_main_folders = potential_main_folders[-a]
  
  if(length(potential_main_folders)<1){
    operation = asm_ErrorFlagFatal("Step 0. No potential main_folders in current directory.")
    break
  } else {
    
    isError = TRUE
    while (isError){
      cat(paste0("There are ", length(potential_main_folders), " potential main folders currently in the directory.\n\n"))
      for(i in 1:length(potential_main_folders)){
        folder_name = strsplit(potential_main_folders[i],"/")[[1]][2]
        cat(paste0(i,": ",folder_name,"\n"))
      }
      cat("\n")
      
      a <- readline(prompt="Choose Main Folder (integer value) for creating database: ")
      if (a %in% as.character(seq(1,length(potential_main_folders)))){
        main_folder = strsplit(potential_main_folders[as.integer(a)],"/")[[1]][2]
        isError = FALSE
        cat("\n")
      } else {
        cat("\n")
        cat("INVALID INPUT. Try again.\n")
      }
    }
  }
  
  potential_RequireFolderStructure = list(c("LOW","MID","HIGH"),c("1","2","3"), c("+30 V","+60 V","+90 V"))
  isError = TRUE
  while (isError){
    cat(paste0("Subfolder structures currently allowed by the DB builder:\n\n"))
    for(i in 1:length(potential_RequireFolderStructure)){
      cat(paste0(i,": "))
      for(j in 1:length(potential_RequireFolderStructure[[i]])){
        cat(paste0(potential_RequireFolderStructure[[i]][j]," "))
      }
      cat("\n")
    }
    cat("\n")
    
    a <- readline(prompt="Choose subfolder structure (integer value) for creating database: ")
    if (a %in% as.character(seq(1,length(potential_RequireFolderStructure)))){
      RequireFolderStructure = potential_RequireFolderStructure[[as.numeric(a)]]
      isError = FALSE
      cat("\n")
    } else {
      cat("\n")
      cat("INVALID INPUT. Try again.\n")
    }
  }
  
  mz_res = 0.005 # default value
  isError = TRUE
  while (isError){
    
    a <- readline(prompt="Enter m/z tolerance in Daltons (e.g. 0.005): ")
    b <- suppressWarnings(as.numeric(a))
    if (!is.na(b)){
      if(b>0){
        mz_res = b
        isError = FALSE
      } else {
        cat("INVALID INPUT. The m/z tolerance must be greater than 0 Daltons. Try again.\n")
      }
    } else {
      cat("INVALID INPUT. Try again.\n")
    }
  }
  
  if(upper_selected_type %in% basic){
  
    ## MAIN Loop for basic builder
    type <- FALSE
      
  
      
      build_style = "Traditional"
      
      output_name = "Output_DARTMS_Database"
      isError = TRUE
      while (isError){
        
        a <- readline(prompt="Enter name for generated database files (e.g. output_database): ")
        output_name = a
        isError = FALSE
      }
      
      ## MAIN CODE
      # Preliminary Data Validation (make sure the spectra are txt files)
      asm_jsp2txt(main_folder)
      
      ## Definition of monoisotpic mass for atoms of interest
      child_path = paste0(parent_path,"/asm_MIM_Definitions.R")
      source(child_path)
      
      cat("Step 1. Initiating database metadata by reading master_file.\n")
      LibMaster = as.data.table(read_excel(master_file))
      nCompounds = dim(LibMaster)[1]
      
      cat('Step 2. Confirming correct number of spectra exist in each "energy" subdirectory of the main_folder.\n\n')
      EnergyFolders = list.dirs(main_folder,recursive = FALSE)
      nEnergyFolders = length(EnergyFolders)
      ReplicateList = NULL;
      
      if(nEnergyFolders<1) {
        operation = asm_ErrorFlagFatal("Step 2. Improper main_folder format. No subdirectories with spectra.")
        break
      }
      d = 0;
      for(i in 1:nEnergyFolders){
        InFolderSpectra = list.files(EnergyFolders[i],pattern = ".txt")
        nInFolderSpectra = length(InFolderSpectra)
        
        a = unlist(strsplit(InFolderSpectra,".txt"))
        a2 = grep("-",a)
        if(length(a2)>0){
          ReplicateList = c(ReplicateList,strsplit(a[a2],"-")[[1]][1])
          a = a[-a2]
        }
        
        b = a %in% LibMaster[,Code]
        c = which(b==FALSE);
        if(length(c)>0){
          message = paste("The following spectra in ",
                          EnergyFolders[i],
                          " do not have metadata in the  master file:\n",
                          sep="");
          d = d + 1;
          for(j in 1:length(c)){
            message = paste(message,a[c[j]],"\n",sep="")
          }
          cat(message);
          cat("\n")
        }
        
        b = LibMaster[,Code] %in% a;
        c = which(b==FALSE);
        if(length(c)>0){
          message = paste("The following codes with metadata in the master file",
                          " do not have spectra in ",
                          EnergyFolders[i],
                          ":\n",
                          sep="");
          d = d + 1;
          for(j in 1:length(c)){
            message = paste(message,LibMaster[c[j],Code],"\n",sep="")
          }
          cat(message);
          cat("\n")
        }
        
      }
      
      if (d>0){
        operation = asm_ErrorFlagFatal("Step 2: Folder and metadata errors.")
        break
      }
      
      
      cat('\nStep 3. Collecting spectra from folders.\n')
      
      eFixed = character(nEnergyFolders)
      for(i in 1:nEnergyFolders){
        a = strsplit(EnergyFolders[i],"/")[[1]]
        eFixed[i] = a[2]
      }
      
      if(RequireFolderStructure[1] %in% eFixed == FALSE){
        operation = asm_ErrorFlagFatal("Step C2: No LOW fragmentation spectra folder found (required).")
        break
      }
      
      if(sum(eFixed %in% RequireFolderStructure) != nEnergyFolders){
        operation = asm_ErrorFlagFatal("Step C2: Spectra subfolder labeling inconsistent with specifications.")
        break
      }
      
      eFixed = RequireFolderStructure[RequireFolderStructure %in% eFixed]
      
      peaks = character(nEnergyFolders)       # individual peak list
      numPeaks = numeric(nEnergyFolders)      # number of peaks per list
      Energies = character(nCompounds)        # likely fixed at nEnergyFolders
      PeakLists = character(nCompounds)       # combined peak lists for a given entry
      NumSpectra = numeric(1);                # likely fixed at nEnergyFolders
      NumPeaksList = character(nCompounds)    # likely fixed at nEnergyFolders
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
      
      pb <- txtProgressBar(min=0,max=(nInFolderSpectra*nEnergyFolders),initial=0,char="=",style=3)
      counter = 0
      for (i in 1:nInFolderSpectra) {
        h = strsplit(InFolderSpectra[i],"\\.")[[1]][1] # split at the period (get just the code)
        k = which(LibMaster[,Code]==h)
        
        preMZ = LibMaster[k,3][[1]];
        for(j in 1:nEnergyFolders) {
          
          txtDirectory = paste(main_folder,"/",eFixed[j],"/",InFolderSpectra[i],sep="")
          # cat(paste(file.info(txtDirectory)$size, "\n",sep="")); # diagnostic for over-sized spectra files
          
          data = readLines(txtDirectory)
          b = asm_ListCreator_v1.02(data,preMZ) ## updated to acquire base peak information
          peaks[j] = b[1];
          if(j==1){
            MassCaliError[k] = as.numeric(b[[2]])
            DimerProb[k] = as.numeric(b[[3]])
            DimerErrorProb[k] = as.numeric(b[[5]])
            PotentialBPs[k] = as.numeric(b[[4]])
            BP[k] = as.numeric(b[[6]])
            if(abs(as.numeric(b[[2]]))>mz_res){
              PotentialErrors[k]=1
            }
          }
          .npeaks = length(data)
          numPeaks[j] = .npeaks;
          
          counter = counter + 1;
          setTxtProgressBar(pb,counter)
          
        }
        
        Energies[k] = list(eFixed)
        PeakLists[k] = list(peaks)
        NumPeaksList[k] = list(numPeaks)
        NumSpectra[k] = length(EnergyFolders);
      }
      
      cat('\n\nStep 4. Creating library for the DIT.\n')
      
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
      
      RDSfilename = paste0(output_name,".RDS")
      saveRDS(Library_RDT,RDSfilename)

     } # ends if basic
  
  if(upper_selected_type %in% full){
    type <- FALSE
    
    # Full DB builder
    
    ## MAIN Loop for full builder
      
           potential_gas_phase = c("He","N2")
      isError = TRUE
      while (isError){
        cat(paste0("Under which gas phase were the spectra collected?\n\n"))
        for(i in 1:length(potential_gas_phase)){
          cat(paste0(i,": ",potential_gas_phase[i],"\n"))
        }
        cat("\n")
        
        a <- readline(prompt="Choose gas phase (integer value) for creating database: ")
        if (a %in% as.character(seq(1,length(potential_gas_phase)))){
          gas_phase = potential_gas_phase[as.numeric(a)]
          isError = FALSE
          cat("\n")
        } else {
          cat("\n")
          cat("INVALID INPUT. Try again.\n")
        }
      }
      
      potential_ion_mode = c("Positive","Negative")
      isError = TRUE
      while (isError){
        cat(paste0("Under which ion mode were the spectra collected?\n\n"))
        for(i in 1:length(potential_ion_mode)){
          cat(paste0(i,": ",potential_ion_mode[i],"\n"))
        }
        cat("\n")
        
        a <- readline(prompt="Choose ion mode (integer value) for creating database: ")
        if (a %in% as.character(seq(1,length(potential_ion_mode)))){
          ion_mode = potential_ion_mode[as.numeric(a)]
          isError = FALSE
          cat("\n")
        } else {
          cat("\n")
          cat("INVALID INPUT. Try again.\n")
        }
      }
      
      
      potential_build_style = c("Traditional","Collapsed")
      isError = TRUE
      while (isError){
        cat(paste0("Build a traditional (low/med/high) or collapsed spectral library?\n\n"))
        for(i in 1:length(potential_build_style)){
          cat(paste0(i,": ",potential_build_style[i],"\n"))
        }
        cat("\n")
        
        a <- readline(prompt="Choose build type (integer value) for creating database: ")
        if (a %in% as.character(seq(1,length(potential_build_style)))){
          build_style = potential_build_style[as.numeric(a)]
          isError = FALSE
          cat("\n")
        } else {
          cat("\n")
          cat("INVALID INPUT. Try again.\n")
        }
      }
      
      output_name = "Output_DARTMS_Database"
      isError = TRUE
      while (isError){
        
        a <- readline(prompt="Enter name for generated database files (e.g. output_database): ")
        output_name = a
        isError = FALSE
      }
      
      ## MAIN CODE
      # Preliminary Data Validation (make sure the spectra are txt files)
      asm_jsp2txt(main_folder)
      
      ## Definition of monoisotpic mass for atoms of interest
      child_path = paste0(parent_path,"/asm_MIM_Definitions.R")
      source(child_path)
      
      cat("Step 1. Initiating database metadata by reading master_file.\n")
      LibMaster = as.data.table(read_excel(master_file))
      nCompounds = dim(LibMaster)[1]
      
      cat('Step 2. Confirming correct number of spectra exist in each "energy" subdirectory of the main_folder.\n\n')
      EnergyFolders = list.dirs(main_folder,recursive = FALSE)
      nEnergyFolders = length(EnergyFolders)
      ReplicateList = NULL;
      
      if(nEnergyFolders<1) {
        operation = asm_ErrorFlagFatal("Step 2. Improper main_folder format. No subdirectories with spectra.")
        break
      }
      d = 0;
      for(i in 1:nEnergyFolders){
        InFolderSpectra = list.files(EnergyFolders[i],pattern = ".txt")
        nInFolderSpectra = length(InFolderSpectra)
        
        a = unlist(strsplit(InFolderSpectra,".txt"))
        a2 = grep("-",a)
        if(length(a2)>0){
          ReplicateList = c(ReplicateList,strsplit(a[a2],"-")[[1]][1])
          a = a[-a2]
        }
        
        b = a %in% LibMaster[,Code]
        c = which(b==FALSE);
        if(length(c)>0){
          message = paste("The following spectra in ",
                          EnergyFolders[i],
                          " do not have metadata in the  master file:\n",
                          sep="");
          d = d + 1;
          for(j in 1:length(c)){
            message = paste(message,a[c[j]],"\n",sep="")
          }
          cat(message);
          cat("\n")
        }
        
        b = LibMaster[,Code] %in% a;
        c = which(b==FALSE);
        if(length(c)>0){
          message = paste("The following codes with metadata in the master file",
                          " do not have spectra in ",
                          EnergyFolders[i],
                          ":\n",
                          sep="");
          d = d + 1;
          for(j in 1:length(c)){
            message = paste(message,LibMaster[c[j],Code],"\n",sep="")
          }
          cat(message);
          cat("\n")
        }
        
      }
      
      if (d>0){
        operation = asm_ErrorFlagFatal("Step 2: Folder and metadata errors.")
        break
      }
      
      
      cat('Step 3a. Generating structures for each compound from its SMILES (assumed correct).\n')
      # Confirm generated and given data is consistent.
      # https://openbabel.org/docs/dev/Command-line_tools/babel.html for obabel system commands
      
      SMILES = character(nCompounds)  # remove salt from smiles (spliting at period)
      InChIKey_gen = character(nCompounds)
      Structure_gen = character(nCompounds)
      Structure_genH = character(nCompounds)
      
      pb <- txtProgressBar(min=0,max=nCompounds,initial=0,char="=",style=3)
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
        setTxtProgressBar(pb, i)
      }
      
      cat('\nStep 3b. Generating mass values for each compound from Structure_genH.\n')
      Formula_gen = character(nCompounds)
      AccurateMass_gen = numeric(nCompounds)
      PrecursorMZ_gen = numeric(nCompounds)
      MW_gen = numeric(nCompounds)
      
      pb <- txtProgressBar(min=0,max=nCompounds,initial=0,char="=",style=3)
      for (i in 1:nCompounds){
        Formula_gen[i] = a = asm_struc2formula(Structure_genH[i][[1]]);
        AccurateMass_gen[i] = asm_MonoisotopicMass(formula = asm_ListFormula(a))
        
        if(ion_mode=="Positive"){
          PrecursorMZ_gen[i] = asm_MonoisotopicMass(formula = asm_ListFormula(paste0(a,"+H"))) # this adds a hydrogen before computing precursor mass
        } else {
          PrecursorMZ_gen[i] = asm_MonoisotopicMass(formula = asm_ListFormula(a)) - asm_MonoisotopicMass(formula = asm_ListFormula("H")) # this subtracts a hydrogen before computing precursor mass
        }
        
        MW_gen[i] = asm_MonoisotopicMass(formula = asm_ListFormula(a),
                                         isotopes = c(C=anC,H=anH,D=anD,O=anO,N=anN,S=anS,P=anP,Br=anBr,Cl=anCl,F=anF,Si=anSi, I=anI, Na=anNa, K=anK));
        setTxtProgressBar(pb, i)
      }
      
      cat('\nStep 4. Collecting spectra from folders, creating new columns for data table library structure.\n')
      
      eFixed = character(nEnergyFolders)
      for(i in 1:nEnergyFolders){
        a = strsplit(EnergyFolders[i],"/")[[1]]
        eFixed[i] = a[2]
      }
      
      if(RequireFolderStructure[1] %in% eFixed == FALSE){
        operation = asm_ErrorFlagFatal("Step C2: No LOW fragmentation spectra folder found (required).")
        break
      }
      
      if(sum(eFixed %in% RequireFolderStructure) != nEnergyFolders){
        operation = asm_ErrorFlagFatal("Step C2: Spectra subfolder labeling inconsistent with specifications.")
        break
      }
      
      eFixed = RequireFolderStructure[RequireFolderStructure %in% eFixed]
      
      peaks = character(nEnergyFolders)       # individual peak list
      numPeaks = numeric(nEnergyFolders)      # number of peaks per list
      Energies = character(nCompounds)        # likely fixed at nEnergyFolders
      PeakLists = character(nCompounds)       # combined peak lists for a given entry
      NumSpectra = numeric(1);                # likely fixed at nEnergyFolders
      NumPeaksList = character(nCompounds)    # likely fixed at nEnergyFolders
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
      
      pb <- txtProgressBar(min=0,max=(nInFolderSpectra*nEnergyFolders),initial=0,char="=",style=3)
      counter = 0
      for (i in 1:nInFolderSpectra) {
        h = strsplit(InFolderSpectra[i],"\\.")[[1]][1] # split at the period (get just the code)
        k = which(LibMaster[,Code]==h)
        
        preMZ = PrecursorMZ_gen[k];
        for(j in 1:nEnergyFolders) {
          
          txtDirectory = paste(main_folder,"/",eFixed[j],"/",InFolderSpectra[i],sep="")
          # cat(paste(file.info(txtDirectory)$size, "\n",sep="")); # diagnostic for over-sized spectra files
          
          data = readLines(txtDirectory)
          b = asm_ListCreator_v1.02(data,preMZ) ## updated to acquire base peak information
          peaks[j] = b[1];
          if(j==1){
            MassCaliError[k] = as.numeric(b[[2]])
            DimerProb[k] = as.numeric(b[[3]])
            DimerErrorProb[k] = as.numeric(b[[5]])
            PotentialBPs[k] = as.numeric(b[[4]])
            BP[k] = as.numeric(b[[6]])
            if(abs(as.numeric(b[[2]]))>mz_res){
              PotentialErrors[k]=1
            }
          }
          .npeaks = length(data)
          numPeaks[j] = .npeaks;
          
          counter = counter + 1;
          setTxtProgressBar(pb,counter)
          
        }
        
        Energies[k] = list(eFixed)
        PeakLists[k] = list(peaks)
        NumPeaksList[k] = list(numPeaks)
        NumSpectra[k] = length(EnergyFolders);
      }
      
      if(build_style=="Collapsed"){
        
        cat("\nStep 5. Building collapsed mass spectra.\n")
        
        pb <- txtProgressBar(min=0,max=nEnergyFolders*nCompounds,initial=0,style=3)
        counter = 0
        for(i in 1:nCompounds){
          CollapsedSpectra = list(asm_CollapsedSpectra(PeakLists[i]))
          for(j in 1:nEnergyFolders){
            PeakLists[i][[1]][j][[1]] = CollapsedSpectra[[1]];
            counter = counter + 1;
            setTxtProgressBar(pb,counter)
          }
        }
        
      } else {
        
        cat("\nStep 5. Checks for spectral 'consistency' as outlined in application notes.\n")
        
        pb <- txtProgressBar(min=0,max=2*nCompounds,initial=0,style=3)
        counter = 0
        for(i in 1:nCompounds){
          FragmentationMetrics[i] = asm_fragConsistencyChecker(PeakLists[i][[1]])
          counter = counter + 1
          setTxtProgressBar(pb,counter)
        }
        
        EnergyNumeric = numeric(nEnergyFolders)
        for(i in 1:nEnergyFolders){
          if (Energies[[1]][i]==RequireFolderStructure[1]){
            b = 1
          } else if (Energies[[1]][i]==RequireFolderStructure[2]){
            b = 2
          } else if (Energies[[1]][i]==RequireFolderStructure[3]){
            b = 3
          }
          EnergyNumeric[i] = as.numeric(b)
        }
        
        EnergyOrder = order(EnergyNumeric)
        
        for (i in 1:nCompounds){
          PotentialErrorsFM1[i] = sum(abs(FragmentationMetrics[[i]]-EnergyOrder))
          counter = counter + 1
          setTxtProgressBar(pb,counter)
        }
      }
      
      cat('\nStep 6. Computing possible peak annotations.\n')
      PossibleAnnotations = character(nCompounds)
      pb <- txtProgressBar(min=0,max=nCompounds, initial=0,style=3)
      for(i in 1:nCompounds){
        PossibleAnnotations[i] = list(asm_AllPeaksGenerator(Structure_genH[[i]],ion_mode))
        setTxtProgressBar(pb,i)
      }
      
      cat('\nStep 7a. Annotating peaks.\n')
      RefinedAnnotations = character(nCompounds)
      pb <-txtProgressBar(min=0,max=nCompounds,initial=0,char="=",style=3)
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
        setTxtProgressBar(pb,i)
      }
      
      cat(paste0('\nStep 7b. Identifying noisy spectra (unannotated peak intensity > ',NoiseThreshold,'%).\n'))
      NoiseMetric = character(nCompounds)
      
      pb <-txtProgressBar(min=0,max=nCompounds,initial=0,char="=",style=3)
      for(i in 1:nCompounds){
        c = character(nEnergyFolders)
        for(j in 1:nEnergyFolders){
          unannotatedPeaks = which(RefinedAnnotations[[i]][[j]]=="")
          c[j] = sum(PeakLists[[i]][j][[1]][unannotatedPeaks,2]) / sum(PeakLists[[i]][j][[1]][,2])
        }
        NoiseMetric[i] = list(c)
        setTxtProgressBar(pb,i)
      }
      
      cat("\nStep 7c. Compute theoretical base peak m/z value for each compound using the molecular formula annotation and isopattern. If annotation not available, use measured BP.\n")
      
      bpMajIsoMZ30V = numeric(nCompounds);
      bpMajIsoAb30V = numeric(nCompounds);
      
      pb <-txtProgressBar(min=0,max=nCompounds,initial=0,char="=",style=3)
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
        setTxtProgressBar(pb,i)
      }
      
      sink("SpectraWithoutBPannotation.txt")
      for(i in noAnnoAvailable){
        cat(LibMaster[i,1][[1]]);
        cat("\n");
      }
      sink()
      
      
      pmMajIsoMZ30V = numeric(nCompounds);
      pmMajIsoAb30V = numeric(nCompounds);
      
      cat("\nStep 7d. Compute isotope ratio errors for protonated molecule using the molecular formula annotation and isopattern.\n")
      pb <-txtProgressBar(min=0,max=nCompounds,initial=0,char="=",style=3)
      for(i in 1:nCompounds){
        if(ion_mode=="Positive"){
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
        
        
        
        setTxtProgressBar(pb,i)
      }
      
      cat("\nStep 7e. Compute theoretical m/z value for Major Fragment 1 for each compound using the molecular formula annotation and isopattern. If annotation not available, use measured BP.\n")
      
      mfMZ30V = numeric(nCompounds);
      mfMajIsoMZ30V = numeric(nCompounds);
      mfMajIsoAb30V = numeric(nCompounds);
      
      pb <-txtProgressBar(min=0,max=nCompounds,initial=0,char="=",style=3)
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
        setTxtProgressBar(pb,i)
      }
      
      cat('\nStep 8a. Generating database in data.table format (internal use).\n')
      
      ## SST - add a subscripted column
      # Load helper function to makesubscript
      source("source/asm_Functions/sst_makesubscript.R", local = TRUE)
      
      # print(Library_RDT)
      for (i in 1:length(formulasubscript)){
        word = as.character(Formula_gen[i])
        formulasubscript[i] = makesubscript(word)
      }
      
      polarity = rep(ion_mode,nCompounds);
      sourceGas = rep(gas_phase,nCompounds);
      
      
      
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
      
      RDSfilename = paste0(output_name,".RDS")
      saveRDS(Library_RDT_DIT_3.22,RDSfilename)
      
      cat('Step 8b. Creating a list of the codes to review for spectral issues.\n')
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
        RevisionSheet = paste0(output_name,"_spec2review.txt")
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
          cat(paste0(d[i],"\t",LibMaster[d[i],Code],"\t",LibMaster[d[i],Name],"\t",comment))
        }
        sink()
      }
      
      cat('Step 8c. Creating a list of the codes to review for missing structure information.\n')
      RevisionSheet2 = paste0(output_name,"_missingStructures.txt")
      sink(RevisionSheet2)
      for(i in 1:nCompounds){
        SBlock = "";  SBlock = as.character(unlist(Library_RDT[i,"Structure_gen"]))
        if(length(grep("nan",SBlock))!=0){
          cat(paste0(LibMaster[i,Code],"\t",LibMaster[i,Name],"\n"))
        }
      }
      sink()
      
      potential_continue = c("Yes","yes","Y","y")
      cat("\n")
      a <- readline(prompt="Do you want to export the library as an SDF file (for MS Search)? (yes/no) ")
      if (a %in% potential_continue){
        cat('Step 9. Generating database in General Purpose text format (sdf).\n')
        Library = Library_RDT
        SDFfilename = paste0(output_name,".SDF")
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
            cat(">  <ION_MODE> \n", ion_mode ,"\n\n") # CONSTANTS FOR THIS DATA SET
            cat(">  <PRECURSOR_TYPE> \n[M+H]+ \n\n") # CONSTANTS FOR THIS DATA SET
            cat(">  <COLISION_GAS> \n", gas_phase ,"\n\n") # CONSTANTS FOR THIS DATA SET
            
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
      
      potential_continue = c("Yes","yes","Y","y")
      cat("\n")
      a <- readline(prompt="Do you want to export the library as a formula list (for Mass Mountaineer)? (yes/no) ")
      if (a %in% potential_continue){
        cat('Step 10. Generating formula lists in text format (txt).\n')
        Library = Library_RDT
        txtfilename = paste0(output_name,"_molecularFormula_list.txt")
        sink(txtfilename)
        for(i in 1:nCompounds){
          cname = "";   cname = as.character(Library[i,"Name"])
          cname = asm_GreekLetterConverter(cname)
          formula = ""; formula = as.character(Library[i,"Formula"])
          cat(paste0(cname,"\t",formula,"\n"))
        }
        sink()
        
        txtfilename = paste0(output_name,"_BPFormula_list.txt")
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
      
  } # ends if full DB statement 
    
    ## End code (check if another library is to be built?)
    potential_continue = c("Yes","yes","Y","y")
    potential_exit = c("No","no","N","n")
    isError = TRUE
    cat("\n")
    while (isError){
      a <- readline(prompt="Do you want to create another database? (yes/no) ")
      if (a %in% potential_continue){
        isError = FALSE
      } else if (a %in% potential_exit){
        cat("\nExiting CRAFTS Lab DB Builder program.\n\n")
        operation = FALSE
        isError =  FALSE
      } else {
        cat("INVALID INPUT. Do you want to create another database? (yes/no) ")
      }
    } #ends while Error loop
  } # ends type loop
} # ends operation loop
