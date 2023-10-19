#### Load Packages ####
# loads the packages used for the script to load packages
packages_to_install <- c("tidyverse", "ggrepel", "shiny", "readxl")
for (package_name in packages_to_install) {
  if(!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}
ui <- fluidPage(
  #App title
  headerPanel("Agilent MS Input"),
  #File Inputs
  sidebarLayout(
    sidebarPanel(
      h4("Browse for Files"),
      fileInput("file1", "Choose File(s)", multiple = TRUE, accept = ".csv"),
      fileInput("file2", "Upload a Input Parameter Template Excel File"),
      h4("Selecting the Checkbox Only Outputs Masses with a Negative Mass Defect (e.g 18.9984)"),
      checkboxInput("checkbox1", "Turn On Mass Defect Filtering"),
      h4("Selecting the Checkbox Filters for Relative Intensities Above 100"),
      checkboxInput("checkbox2", "Turn On Intensity Filtering")
    ),
    mainPanel(
      #Parameter input
      h4("Write in Input Parameters"),
      textInput("FilePath", label = "File Path to Folder with Exported Files (e.g. E:/Documents/Exposrted_csv_files):"),
      textInput("Author", label = "Enter Author Name:"),
      textInput("LicenseType", label = "License (e.g. CC0):"),
      textInput("Date", label = "Date of Experiment (e.g. YYYY.MM.DD):"),
      textInput("CollisionGas", label = "Collision Gas (e.g. Nitrogen):"),
      textInput("SourceTemp", label = "Source Temperature (e.g. 50 C):"),
      textInput("ResSetting", label = "Resolution Setting (e.g. 20000 R):"),
      textInput("CapTemp", label = "Capillary Temperature (e.g. 275 C):"),
      textInput("IsoWidth", label = "Isolation Width (e.g. 4 Daltons):"),
      textInput("IonType", label = "Ion Type (e.g. [M-H]-):"),
      textInput("IonMode", label = "Ion Mode (e.g. Negative/Positive):"),
      textInput("InstrumentName", label = "Instrument Name (e.g. Agilent 6546 LC/Q-TOF):"),
      textInput("InstrumentType", label = "Instrument Type (e.g. LC-ESI-QTOF):"),
      # action button that submits the user inputs into the Shiny App function
      actionButton("Submit_Button", "Submit"),
      # text output of the user input
      textOutput("filepath_output"),
      textOutput("author_output"),
      textOutput("license_output"),
      textOutput("date_output"),
      textOutput("collision_output"),
      textOutput("sourcetemp_output"),
      textOutput("ressetting_output"),
      textOutput("captemp_output"),
      textOutput("isowidth_output"),
      textOutput("iontype_output"),
      textOutput("ionmode_output"),
      textOutput("instname_output"),
      textOutput("insttype_output")
    ) 
  )
)
shinyServer <- function(input, output){
  processing_done <- reactiveVal(FALSE)
  observeEvent(input$Submit_Button, {
    if (!is.null(input$file2)) {
      parameters_template <- read_excel(input$file2$datapath)
    }
    # save written user inputs if available
    user_inputs <- reactiveValuesToList(input)
    user_FilePath <- reactiveVal(input$FilePath)
    # creates a list of the user inputs submitted by the action button
    saveRDS(user_inputs, file = "user_inputs.rds")
    # reads in the list as a variable
    rds_list <- readRDS("user_inputs.rds")
    #### FUNCTION USING FILES AS INPUT ####
    UseFilesAsInputForTextFiles <- function (dataframe_file1){
      for (i in 1:length(dataframe_file1$name)) {
        i_row <- dataframe_file1[i,]
        file <- i_row$datapath
        # reads in the csv file, skipping the first row as they are not typically uniform due to "lazy loading"
        # uses this for pulling the spectral data (mass & abundance) in the function below
        data_file <- read_csv(file, skip = 1)
        if (ncol(data_file) == 1) {
          data_file <- data_file %>% 
            separate("#Point,X(Thomsons),Y(Counts)", into = c("#Point", "Daltons", "Counts"), sep = ",") %>% 
            rename(Number=1)
        }
        if (ncol(data_file) == 2) {
          data_file <- data_file %>% 
            separate("X(Thomsons),Y(Counts)", into = c("Daltons", "Counts"), sep = ",") %>% 
            rename(Number=1)
        }
        if (ncol(data_file) == 4) {
          data_file <- data_file %>%
            select(-ncol(data_file)) %>% 
            rename(Number= 1) %>%
            rename(Daltons = "X(Thomsons)") %>% 
            rename(Counts = "Y(Counts)")
        }
        if ("#Point" %in% colnames(data_file)) {
          data_file <- data_file %>% 
            rename(Number = "#Point")
        }
        if ("X(Thomsons)" %in% colnames(data_file)) {
          data_file <- data_file %>% 
            rename(Daltons = "X(Thomsons)")
        }
        if ("Y(Counts)" %in% colnames(data_file)) {
          data_file <- data_file %>% 
            rename(Counts = "Y(Counts)")
        }
        data_file <- data_file %>% 
          mutate(Daltons = as.numeric(Daltons)) %>% 
          mutate(Counts = as.numeric(Counts))
        out_spectrum <- data_file[c("Daltons", "Counts")] %>% mutate(Daltons = sprintf("%.4f", round(Daltons,4)) ,
                                                                     Counts = sprintf("%.4f", round(Counts,4))) %>% 
          mutate(Daltons = as.numeric(Daltons)) %>% 
          mutate(Counts = as.numeric(Counts))
        data <- read_csv(file)
        # isolates the weird first row
        header_in <- colnames(data)
        # fixes the first row, combines them into one string
        if (ncol(data) == 2) {
          header_string <- paste0(header_in[1],header_in[2]) %>% noquote() %>% trimws()
        }
        if (ncol(data) == 3) {
          header_string <- paste0(header_in[1],header_in[2],header_in[3]) %>% noquote() %>% trimws()
        }
        if (ncol(data) == 4) {
          header_string <- paste0(header_in[1],header_in[2],header_in[3],header_in[4]) %>% noquote() %>% trimws()
        }
        # save the first row land pull the mass and collision energy from the first row of columns now all combined
        header <- header_string
        match <- regexpr("\\[z=1\\]", header)
        if (match != -1) {
          precursor_mass_string <- paste( "Precursor Mass:", str_extract(header,"[0-9.]*(?=\\[)"))
        } else {
          precursor_mass <- paste( "Precursor Mass:", str_extract(header,"\\((\\d+\\.\\d{4})"))
          precursor_mass_string <- str_replace_all(precursor_mass, "\\(", "")
        }
        # create strings, pulling the relevant information from the files and user inputs to be loaded into the text files
        CE_string <-  paste("Collision Energy:", str_extract(header, "(?<=CID@).[0-9.][0-9.][0-9.]"), "eV")
        if (!is.null(input$file2)) {
          Ion <- as.character(parameters_template[10,2])
          Ion_mode <- paste0("Ion Mode: ", Ion)
          Ion_Type <- as.character(parameters_template[9,2])
          Ion_Type_string <- paste("IonType:", Ion_Type)
          InstType <- as.character(parameters_template[12,2])
          Inst_Type <- paste("Instrument Type:", InstType)
          colgas <- as.character(parameters_template[4,2])
          Col_gas <- paste("Collision Gas:", colgas)
          resolution <- as.character(parameters_template[6,2])
          Res_setting <- paste("Resolution Setting:", resolution)
          capTEMP <- as.character(parameters_template[7,2])
          Cap_temp_string <- paste("Capillary Temperature:", capTEMP)
          isolation <- as.character(parameters_template[8,2])
          Iso_Width <- paste("Isolation Width:", isolation)
          sourceTEMP <- as.character(parameters_template[5,2])
          source_temp_string <- paste("Source Temperature:", sourceTEMP)
          nameofINST <- as.character(parameters_template[11,2])
          Ins_name <- paste("Instrument Name:", nameofINST)
          auth <-  as.character(parameters_template[1,2])
          Author <- paste("Author:", auth)
          dateACQ <- as.character(parameters_template[3,2])
          acq_date <- paste("Date of Acquisition:", dateACQ)
          LISC <- as.character(parameters_template[2,2])
          License <- paste("License:", LISC)
        } else {
          Ion_Type_string <- paste("IonType:", rds_list$IonType)
          Inst_Type <- paste("Instrument Type:", rds_list$InstrumentType)
          Col_gas <- paste("Collision Gas:", rds_list$CollisionGas)
          Res_setting <- paste("Resolution Setting:", rds_list$ResSetting)
          Cap_temp_string <- paste("Capillary Temperature:", rds_list$CapTemp)
          Iso_Width <- paste("Isolation Width:", rds_list$IsoWidth)
          source_temp_string <- paste("Source Temperature:", rds_list$SourceTemp)
          Ins_name <- paste("Instrument Name:",  rds_list$InstrumentName)
          Author <- paste("Author:", rds_list$Author)
          acq_date <- paste("Date of Acquisition:", input$Date)
          License <- paste("License:", input$LicenseType)
        }
        
        # create a string to name the text file with
        file_name <- i_row$name
        file_name_only <- sub("\\.csv$","", file_name)
        output_filename <- paste0(file_name_only, ".txt")
        name_string <- str_extract(file_name_only, "^[A-Za-z0-9]+")
        # loads in the strings from before into the text file
        write_lines(c(name_string, Author, License, acq_date,  CE_string, precursor_mass_string, Ins_name, Inst_Type, Col_gas, Res_setting, Cap_temp_string, Iso_Width, Ion_Type_string, source_temp_string, num_peaks), file = output_filename, sep = "\r\n")
        # loads in the spectral data
        write.table(out_spectrum, 
                    output_filename,
                    eol = "\n",
                    sep = " ",
                    quote = FALSE,
                    append = TRUE,
                    col.names = FALSE,
                    row.names = FALSE,) 
      }
    }
    CreateOutputSpectrumFileInput <- function(file){
      data_file <- read_csv(file, skip = 1)
      if (ncol(data_file) == 1) {
        data_file <- data_file %>% 
          separate("#Point,X(Thomsons),Y(Counts)", into = c("#Point", "Daltons", "Counts"), sep = ",") %>% 
          rename(Number=1)
      }
      if (ncol(data_file) == 2) {
        data_file <- data_file %>% 
          separate("X(Thomsons),Y(Counts)", into = c("Daltons", "Counts"), sep = ",") %>% 
          rename(Number=1)
      }
      if (ncol(data_file) == 4) {
        data_file <- data_file %>%
          select(-ncol(data_file)) %>% 
          rename(Number= 1) %>%
          rename(Daltons = "X(Thomsons)") %>% 
          rename(Counts = "Y(Counts)")
      }
      if ("#Point" %in% colnames(data_file)) {
        data_file <- data_file %>% 
          rename(Number = "#Point")
      }
      if ("X(Thomsons)" %in% colnames(data_file)) {
        data_file <- data_file %>% 
          rename(Daltons = "X(Thomsons)")
      }
      if ("Y(Counts)" %in% colnames(data_file)) {
        data_file <- data_file %>% 
          rename(Counts = "Y(Counts)")
      }
      data_file <- data_file %>% 
        mutate(Daltons = as.numeric(Daltons)) %>% 
        mutate(Counts = as.numeric(Counts))
      out_spectrum <- data_file[c("Daltons", "Counts")] %>% mutate(Daltons = sprintf("%.4f", round(Daltons,4)) ,
                                                                   Counts = sprintf("%.4f", round(Counts,4))) %>% 
        mutate(Daltons = as.numeric(Daltons)) %>% 
        mutate(Counts = as.numeric(Counts))
      return(out_spectrum)
    }
    GetNameStringsFromFile <- function() {
      Ion <- as.character(parameters_template[10,2])
      Ion_mode <- paste0("Ion Mode: ", Ion)
      Ion_Type <- as.character(parameters_template[9,2])
      Ion_Type_string <- paste("IonType:", Ion_Type)
      InstType <- as.character(parameters_template[12,2])
      Inst_Type <- paste("Instrument Type:", InstType)
      colgas <- as.character(parameters_template[4,2])
      Col_gas <- paste("Collision Gas:", colgas)
      resolution <- as.character(parameters_template[6,2])
      Res_setting <- paste("Resolution Setting:", resolution)
      capTEMP <- as.character(parameters_template[7,2])
      Cap_temp_string <- paste("Capillary Temperature:", capTEMP)
      isolation <- as.character(parameters_template[8,2])
      Iso_Width <- paste("Isolation Width:", isolation)
      sourceTEMP <- as.character(parameters_template[5,2])
      source_temp_string <- paste("Source Temperature:", sourceTEMP)
      nameofINST <- as.character(parameters_template[11,2])
      Ins_name <- paste("Instrument Name:", nameofINST)
      auth <-  as.character(parameters_template[1,2])
      Author <- paste("Author:", auth)
      dateACQ <- as.character(parameters_template[3,2])
      acq_date <- paste("Date of Acquisition:", dateACQ)
      LISC <- as.character(parameters_template[2,2])
      License <- paste("License:", LISC)
      filestring_list <- list(
        Author, License, acq_date, Ins_name, Inst_Type, Col_gas, Res_setting, Cap_temp_string, Iso_Width, Ion_Type_string, source_temp_string
      )
      return(filestring_list)
    }
    #### FUNCTION USING WRITTEN PARAMETERS ####
    UseWrittenAsInputForTextFiles <- function(filepath) {
      files <- list.files(path = filepath, pattern = ".csv", ignore.case = TRUE)
      for (file in files) {
        # reads in the csv file, skipping the first row as they are not typically uniform due to "lazy loading"
        # uses this for pulling the spectral data (mass & abundance) in the function below
        data_file <- read_csv(file, skip = 1)
        # pull in the rest of the data (including the weird first rows)
        data <- read_csv(file)
        # isolates the weird first row
        header_in <- colnames(data)
        # fixes the first row, combines them into one string
        header = paste0(header_in[1],header_in[2]) %>% noquote() %>% trimws()
        # renames the columns
        old_cols[2] <- "Daltons"
        old_cols[3] <- "Counts"
        old_cols[1] <- "Number"
        colnames(data_file) <- old_cols
        # create strings, pulling the relevant information from the files and user inputs to be loaded into the text files
        precursor_mass_string <- paste( "Precursor Mass:", str_extract(header,"[0-9.]*(?=\\[)"))
        CE_string <-  paste("Collision Energy:", str_extract(header, "(?<=CID@).[0-9.][0-9.][0-9.]"), "eV")
        if (!is.null(input$file2)) {
          Ion <- as.character(parameters_template[10,2])
          Ion_mode <- paste0("Ion Mode: ", Ion)
          Ion_Type <- as.character(parameters_template[9,2])
          Ion_Type_string <- paste("IonType:", Ion_Type)
          InstType <- as.character(parameters_template[12,2])
          Inst_Type <- paste("Instrument Type:", InstType)
          colgas <- as.character(parameters_template[4,2])
          Col_gas <- paste("Collision Gas:", colgas)
          resolution <- as.character(parameters_template[6,2])
          Res_setting <- paste("Resolution Setting:", resolution)
          capTEMP <- as.character(parameters_template[7,2])
          Cap_temp_string <- paste("Capillary Temperature:", capTEMP)
          isolation <- as.character(parameters_template[8,2])
          Iso_Width <- paste("Isolation Width:", isolation)
          sourceTEMP <- as.character(parameters_template[5,2])
          source_temp_string <- paste("Source Temperature:", sourceTEMP)
          nameofINST <- as.character(parameters_template[11,2])
          Ins_name <- paste("Instrument Name:", nameofINST)
          auth <-  as.character(parameters_template[1,2])
          Author <- paste("Author:", auth)
          dateACQ <- as.character(parameters_template[3,2])
          acq_date <- paste("Date of Acquisition:", dateACQ)
          LISC <- as.character(parameters_template[2,2])
          License <- paste("License:", LISC)
        } else {
          Ion_Type_string <- paste("IonType:", rds_list$IonType)
          Inst_Type <- paste("Instrument Type:", rds_list$InstrumentType)
          Col_gas <- paste("Collision Gas:", rds_list$CollisionGas)
          Res_setting <- paste("Resolution Setting:", rds_list$ResSetting)
          Cap_temp_string <- paste("Capillary Temperature:", rds_list$CapTemp)
          Iso_Width <- paste("Isolation Width:", rds_list$IsoWidth)
          source_temp_string <- paste("Source Temperature:", rds_list$SourceTemp)
          Ins_name <- paste("Instrument Name:",  rds_list$InstrumentName)
          Author <- paste("Author:", rds_list$Author)
          acq_date <- paste("Date of Acquisition:", input$Date)
          License <- paste("License:", input$LicenseType)
        }
        file_name_only <- sub("\\.csv$","", file)
        output_filename <- paste0(file_name_only, ".txt")
        name_string <- str_extract(file_name_only, "^[A-Za-z0-9]+")
        # isolates the mass (Daltons) and abundance (Counts) from the csv file loaded in without the first row
        out_spectrum <- data_file[c("Daltons", "Counts")] %>% mutate(Daltons = sprintf("%.4f", round(Daltons,4)) ,
                                                                     Counts = sprintf("%.4f", round(Counts,4)))
        # converts the isolated spectral data to numeric
        out_spectrum$Daltons <- as.numeric(out_spectrum$Daltons)
        out_spectrum$Counts <- as.numeric(out_spectrum$Counts)
        # calculates the number of peaks by counting the number of rows
        num_rows <- as.numeric(nrow(out_spectrum))
        # creates a string from the number of peaks
        num_peaks <- paste("Num Peaks:", num_rows)
        # loads in the strings from before into the text file
        write_lines(c(name_string, Author, License, acq_date,  CE_string, precursor_mass_string, Ins_name, Inst_Type, Col_gas, Res_setting, Cap_temp_string, Iso_Width, Ion_Type_string, source_temp_string, num_peaks), file = output_filename, sep = "\r\n")
        # loads in the spectral data
        write.table(out_spectrum, 
                    output_filename,
                    eol = "\n",
                    sep = " ",
                    quote = FALSE,
                    append = TRUE,
                    col.names = FALSE,
                    row.names = FALSE,) 
      }
    }
    
    CreateOuputSpectrumWrittenInput <- function(data_file) {
      # isolates the mass (Daltons) and abundance (Counts) from the csv file loaded in without the first row
      out_spectrum <- data_file[c("Daltons", "Counts")] %>% mutate(Daltons = sprintf("%.4f", round(Daltons,4)) ,
                                                                   Counts = sprintf("%.4f", round(Counts,4)))
      # converts the isolated spectral data to numeric
      out_spectrum$Daltons <- as.numeric(out_spectrum$Daltons)
      out_spectrum$Counts <- as.numeric(out_spectrum$Counts)
      return(out_spectrum)
    }
   
    GetNameStringsFromWrittenInputs <- function() {
      Ion_Type_string <- paste("IonType:", rds_list$IonType)
      Inst_Type <- paste("Instrument Type:", rds_list$InstrumentType)
      Col_gas <- paste("Collision Gas:", rds_list$CollisionGas)
      Res_setting <- paste("Resolution Setting:", rds_list$ResSetting)
      Cap_temp_string <- paste("Capillary Temperature:", rds_list$CapTemp)
      Iso_Width <- paste("Isolation Width:", rds_list$IsoWidth)
      source_temp_string <- paste("Source Temperature:", rds_list$SourceTemp)
      Ins_name <- paste("Instrument Name:",  rds_list$InstrumentName)
      Author <- paste("Author:", rds_list$Author)
      acq_date <- paste("Date of Acquisition:", input$Date)
      License <- paste("License:", input$LicenseType)
      filestring_list <- list(
        Author, License, acq_date, Ins_name, Inst_Type, Col_gas, Res_setting, Cap_temp_string, Iso_Width, Ion_Type_string, source_temp_string
      )
      return(filestring_list)
    }
    GetMass_CE_NameWrittenInput <- function(file) {
      data <- read_csv(file)
      # isolates the weird first row
      header_in <- colnames(data)
      # fixes the first row, combines them into one string
      header = paste0(header_in[1],header_in[2]) %>% noquote() %>% trimws()
      # renames the columns
      old_cols[2] <- "Daltons"
      old_cols[3] <- "Counts"
      old_cols[1] <- "Number"
      colnames(data_file) <- old_cols
      # create strings, pulling the relevant information from the files and user inputs to be loaded into the text files
      precursor_mass_string <- paste( "Precursor Mass:", str_extract(header,"[0-9.]*(?=\\[)"))
      CE_string <-  paste("Collision Energy:", str_extract(header, "(?<=CID@).[0-9.][0-9.][0-9.]"), "eV")
      # get name of the compound from file name
      file_name_only <- sub("\\.csv$","", file)
      output_filename <- paste0(file_name_only, ".txt")
      name_string <- str_extract(file_name_only, "^[A-Za-z0-9]+")
      Mass_CE_Name <- list(precursor_mass_string, CE_string, name_string)
      return(Mass_CE_Name)
    }
    ##### if checkbox 1 is checked, turn on mass defect filtering, else no mass defect filtering #####
    if (input$checkbox1) {
      # use files for input
      if (!is.null(input$file1)) {
        dataframe_file1 <- input$file1
        for (i in 1:length(dataframe_file1$name)) {
          i_row <- dataframe_file1[i,]
          file <- i_row$datapath
          # reads in the csv file, skipping the first row as they are not typically uniform due to "lazy loading"
          # uses this for pulling the spectral data (mass & abundance) in the function below
          CreateOutputSpectrumFileInput(file)
          # only keeps  masses with a negative mass defect
          #filters for masses with only a mass defect
          out_spectrum <- out_spectrum %>% mutate(masskep = if_else(Daltons - round(Daltons) < 0, Daltons, NA), Daltons) %>% 
            na.omit(out_spectrum) %>% 
            select(Daltons, Counts)
          #read in entire dataframe
          data <- read_csv(file)
          # isolates the weird first row
          header_in <- colnames(data)
          # fixes the first row, combines them into one string
          header = paste0(header_in[1],header_in[2]) %>% noquote() %>% trimws()
          # renames the columns
          old_cols[2] <- "Daltons"
          old_cols[3] <- "Counts"
          old_cols[1] <- "Number"
          colnames(data_file) <- old_cols
          # create strings, pulling the relevant information from the files and user inputs to be loaded into the text files
          precursor_mass_string <- paste( "Precursor Mass:", str_extract(header,"[0-9.]*(?=\\[)"))
          CE_string <-  paste("Collision Energy:", str_extract(header, "(?<=CID@).[0-9.][0-9.][0-9.]"), "eV")
          # get name of the compound from file name
          file_name_only <- sub("\\.csv$","", file)
          output_filename <- paste0(file_name_only, ".txt")
          name_string <- str_extract(file_name_only, "^[A-Za-z0-9]+")
          Mass_CE_Name <- list(precursor_mass_string, CE_string, name_string)
          # create other strings, pulling the relevant information from the files and user inputs to be loaded into the text files
          if (!is.null(input$file2)) {
            filestring_list <- GetNameStringsFromFile()
          } else {
            filestring_list <- GetNameStringsFromWrittenInputs()
          }
          # calculates the number of peaks by counting the number of rows
          num_rows <- as.numeric(nrow(out_spectrum))
          # creates a string from the number of peaks
          num_peaks <- paste("Num Peaks:", num_rows)
          # create a string to name the text file with
          file_name <- i_row$name
          file_name_only <- sub("\\.csv$","", file_name)
          output_filename <- paste0(file_name_only, ".txt")
          name_string <- str_extract(file_name_only, "^[A-Za-z0-9]+")
          # loads in the strings from before into the text file
          write_lines(c(Mass_CE_Name, filestring_list, num_peaks), file = output_filename, sep = "\r\n")
          # loads in the spectral data 
          write.table(out_spectrum, 
                      output_filename,
                      eol = "\n",
                      sep = " ",
                      quote = FALSE,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE,) 
        }
      } else  { # use written inputs 
        filepath <- rds_list$FilePath
        files <- list.files(path = filepath, pattern = ".csv", ignore.case = TRUE)
        for (file in files) {
          # reads in the csv file, skipping the first row as they are not typically uniform due to "lazy loading"
          # uses this for pulling the spectral data (mass & abundance) in the function below
          data_file <- read_csv(file, skip = 1)
          CreateOuputSpectrumWrittenInput(data_file)
          # only keeps  masses with a negative mass defect
          #filters for masses with only a mass defect
          #filters for masses with only a mass defect
          out_spectrum <- out_spectrum %>% mutate(masskep = if_else(Daltons - round(Daltons) < 0, Daltons, NA), Daltons) %>% 
            na.omit(out_spectrum) %>% 
            select(Daltons, Counts)
          # pull in the rest of the data (including the weird first rows)
          if (!is.null(input$file2)) {
            filestring_list <- GetNameStringsFromFile()
          } else {
            filestring_list <- GetNameStringsFromWrittenInputs()
          }
          # Get Mass, CE and Name Strings from file 
          GetMass_CE_NameWrittenInput(file)
          # calculates the number of peaks by counting the number of rows
          num_rows <- as.numeric(nrow(out_spectrum))
          # creates a string from the number of peaks
          num_peaks <- paste("Num Peaks:", num_rows)
          # loads in the strings from before into the text file
          write_lines(c(Mass_CE_Name, filestring_list, num_peaks), file = output_filename, sep = "\r\n")
          # loads in the spectral data
          write.table(out_spectrum, 
                      output_filename,
                      eol = "\n",
                      sep = " ",
                      quote = FALSE,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE,) 
        }
      }
    } else { # do normal function
      # use files for input (normal function)
      if (!is.null(input$file1)) {
        dataframe_file1 <- input$file1
        UseFilesAsInputForTextFiles(dataframe_file1)
      } else  { # use written inputs (normal function)
        filepath <- rds_list$FilePath
        UseWrittenAsInputForTextFiles(filepath)
      }
    }
    ##### if checkbox 2 is checked, turn on intensity filtering, else no spectra intensity filtering #####
    if (input$checkbox2) {
      # use files for input
      if (!is.null(input$file1)) {
        dataframe_file1 <- input$file1
        for (i in 1:length(dataframe_file1$name)) {
          i_row <- dataframe_file1[i,]
          file <- i_row$datapath
          # reads in the csv file, skipping the first row as they are not typically uniform due to "lazy loading"
          # uses this for pulling the spectral data (mass & abundance) in the function below
          CreateOutputSpectrumFileInput(file)
          # filter intensity/ abundances for onyl mases greater than or equal to 100
          out_spectrum <- out_spectrum %>% 
            filter(Counts >= 100)
          #read in entire dataframe
          data <- read_csv(file)
          # isolates the weird first row
          header_in <- colnames(data)
          # fixes the first row, combines them into one string
          header = paste0(header_in[1],header_in[2]) %>% noquote() %>% trimws()
          # renames the columns
          old_cols[2] <- "Daltons"
          old_cols[3] <- "Counts"
          old_cols[1] <- "Number"
          colnames(data_file) <- old_cols
          # create strings, pulling the relevant information from the files and user inputs to be loaded into the text files
          precursor_mass_string <- paste( "Precursor Mass:", str_extract(header,"[0-9.]*(?=\\[)"))
          CE_string <-  paste("Collision Energy:", str_extract(header, "(?<=CID@).[0-9.][0-9.][0-9.]"), "eV")
          # get name of the compound from file name
          file_name_only <- sub("\\.csv$","", file)
          output_filename <- paste0(file_name_only, ".txt")
          name_string <- str_extract(file_name_only, "^[A-Za-z0-9]+")
          Mass_CE_Name <- list(precursor_mass_string, CE_string, name_string)
          if (!is.null(input$file2)) {
            filestring_list <- GetNameStringsFromFile()
          } else {
            filestring_list <- GetNameStringsFromWrittenInputs()
          }
          # calculates the number of peaks by counting the number of rows
          num_rows <- as.numeric(nrow(out_spectrum))
          # creates a string from the number of peaks
          num_peaks <- paste("Num Peaks:", num_rows)
          # create a string to name the text file with
          file_name <- i_row$name
          file_name_only <- sub("\\.csv$","", file_name)
          output_filename <- paste0(file_name_only, ".txt")
          name_string <- str_extract(file_name_only, "^[A-Za-z0-9]+")
          # loads in the strings from before into the text file
          write_lines(c(Mass_CE_Name, filestring_list, num_peaks), file = output_filename, sep = "\r\n")
          # loads in the spectral data 
          write.table(out_spectrum, 
                      output_filename,
                      eol = "\n",
                      sep = " ",
                      quote = FALSE,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE,)
        } 
      } else  { # use written inputs
        filepath <- rds_list$FilePath
        files <- list.files(path = filepath, pattern = ".csv", ignore.case = TRUE)
        for (file in files) {
          # reads in the csv file, skipping the first row as they are not typically uniform due to "lazy loading"
          # uses this for pulling the spectral data (mass & abundance) in the function below
          data_file <- read_csv(file, skip = 1)
          CreateOuputSpectrumWrittenInput(data_file)
          # filter intensity/ abundances for onyl mases greater than or equal to 100
          out_spectrum <- out_spectrum %>% 
            filter(Counts >= 100)
          # pull in the rest of the data (including the weird first rows)
          if (!is.null(input$file2)) {
            filestring_list <- GetNameStringsFromFile()
          } else {
            filestring_list <- GetNameStringsFromWrittenInputs()
          }
          # Get Mass, CE and Name Strings from file 
          GetMass_CE_NameWrittenInput(file)
          # loads in the strings from before into the text file
          write_lines(c(Mass_CE_Name, filestring_list, num_peaks), file = output_filename, sep = "\r\n")
          # loads in the spectral data
          write.table(out_spectrum, 
                      output_filename,
                      eol = "\n",
                      sep = " ",
                      quote = FALSE,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE,)
        }
      }
    } else { # do normal function
      # use files for input (normal function)
      if (!is.null(input$file1)) {
        dataframe_file1 <- input$file1
        UseFilesAsInputForTextFiles(dataframe_file1)
      } else  { # use written inputs (normal function)
        filepath <- rds_list$FilePath
        UseWrittenAsInputForTextFiles(filepath)
      }     
    }
    #### both check boxes are selected, turn on mass defect filtering and turn on intensity filtering, else no filtering of any kind #####
    if (input$checkbox1 && input$checkbox2) {
      # use files for input
      if (!is.null(input$file1)) {
        dataframe_file1 <- input$file1
        for (i in 1:length(dataframe_file1$name)) {
          i_row <- dataframe_file1[i,]
          file <- i_row$datapath
          # reads in the csv file, skipping the first row as they are not typically uniform due to "lazy loading"
          # uses this for pulling the spectral data (mass & abundance) in the function below
          CreateOutputSpectrumFileInput(file)
          #filters for masses with only a mass defect
          # filter intensity/ abundances for onyl mases greater than or equal to 100
          out_spectrum <- out_spectrum %>% mutate(masskep = if_else(Counts >= 100,
                                                                    if_else(Daltons - round(Daltons) < 0, Daltons, NA), Daltons)) %>% 
            na.omit(out_spectrum) %>% 
            select(Daltons, Counts)
          #read in entire dataframe
          data <- read_csv(file)
          # isolates the weird first row
          header_in <- colnames(data)
          # fixes the first row, combines them into one string
          header = paste0(header_in[1],header_in[2]) %>% noquote() %>% trimws()
          # renames the columns
          old_cols[2] <- "Daltons"
          old_cols[3] <- "Counts"
          old_cols[1] <- "Number"
          colnames(data_file) <- old_cols
          # create strings, pulling the relevant information from the files and user inputs to be loaded into the text files
          precursor_mass_string <- paste( "Precursor Mass:", str_extract(header,"[0-9.]*(?=\\[)"))
          CE_string <-  paste("Collision Energy:", str_extract(header, "(?<=CID@).[0-9.][0-9.][0-9.]"), "eV")
          # get name of the compound from file name
          file_name_only <- sub("\\.csv$","", file)
          output_filename <- paste0(file_name_only, ".txt")
          name_string <- str_extract(file_name_only, "^[A-Za-z0-9]+")
          Mass_CE_Name <- list(precursor_mass_string, CE_string, name_string)
          # create other strings, pulling the relevant information from the files and user inputs to be loaded into the text files
          if (!is.null(input$file2)) {
            filestring_list <- GetNameStringsFromFile()
          } else {
            filestring_list <- GetNameStringsFromWrittenInputs()
          }
          # calculates the number of peaks by counting the number of rows
          num_rows <- as.numeric(nrow(out_spectrum))
          # creates a string from the number of peaks
          num_peaks <- paste("Num Peaks:", num_rows)
          # create a string to name the text file with
          file_name <- i_row$name
          file_name_only <- sub("\\.csv$","", file_name)
          output_filename <- paste0(file_name_only, ".txt")
          name_string <- str_extract(file_name_only, "^[A-Za-z0-9]+")
          # loads in the strings from before into the text file
          write_lines(c(Mass_CE_Name, filestring_list, num_peaks), file = output_filename, sep = "\r\n")
          # loads in the spectral data 
          write.table(out_spectrum, 
                      output_filename,
                      eol = "\n",
                      sep = " ",
                      quote = FALSE,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE,) 
        }
      } else  { # use written inputs 
        filepath <- rds_list$FilePath
        files <- list.files(path = filepath, pattern = ".csv", ignore.case = TRUE)
        for (file in files) {
          # reads in the csv file, skipping the first row as they are not typically uniform due to "lazy loading"
          # uses this for pulling the spectral data (mass & abundance) in the function below
          data_file <- read_csv(file, skip = 1)
          CreateOuputSpectrumWrittenInput(data_file)
          #filters for masses with only a mass defect
          # filter intensity/ abundances for onyl mases greater than or equal to 100
          Counts >= 100
          # pull in the rest of the data (including the weird first rows)
          if (!is.null(input$file2)) {
            filestring_list <- GetNameStringsFromFile()
          } else {
            filestring_list <- GetNameStringsFromWrittenInputs
          }
          # Get Mass, CE and Name Strings from file 
          GetMass_CE_NameWrittenInput(file)
          # calculates the number of peaks by counting the number of rows
          num_rows <- as.numeric(nrow(out_spectrum))
          # creates a string from the number of peaks
          num_peaks <- paste("Num Peaks:", num_rows)
          # loads in the strings from before into the text file
          write_lines(c(Mass_CE_Name, filestring_list, num_peaks), file = output_filename, sep = "\r\n")
          # loads in the spectral data
          write.table(out_spectrum, 
                      output_filename,
                      eol = "\n",
                      sep = " ",
                      quote = FALSE,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE,) 
        }
      }
      
    } else { # do normal function (no filtering)
      # use files for input (normal function)
      if (!is.null(input$file1)) {
        dataframe_file1 <- input$file1
        UseFilesAsInputForTextFiles(dataframe_file1)
      } else  { # use written inputs (normal function)
        filepath <- rds_list$FilePath
        UseWrittenAsInputForTextFiles(filepath)
      }
    }
    if (processing_done()) {
      stopApp()
    }
  })
  # creates the outputs in the shiny UI so the user can confirm their inputs in real time
  output$filepath_output <- 
    renderText(input$FilePath)
  output$author_output <- 
    renderText(input$Author)
  output$license_output <-
    renderText(input$LicenseType)
  output$date_output <-
    renderText(input$Date)
  output$collision_output <-
    renderText(input$CollisionGas)
  output$sourcetemp_output <-
    renderText(input$SourceTemp)
  output$ressetting_output <-
    renderText(input$ResSetting)
  output$captemp_output <-
    renderText(input$CapTemp)
  output$isowidth_output <-
    renderText(input$IsoWidth)
  output$iontype_output <-
    renderText(input$IonType)
  output$iontype_output <-
    renderText(input$IonMode)
  output$instname_output <-
    renderText(input$InstrumentName)
  output$insttype_output <-
    renderText(input$InstrumentType)
}
# runs the function with the user inputs
shinyApp(ui,shinyServer)