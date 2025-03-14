#' This function pull images from SuBastian sealog given an OCSS number
#' 
#' To use this function, run this script and the function will be imported to the environment. 
#' 
#' CruiseData and CruiseSandbox should always be connected and accessible.
#' 
#' It might be better to change the wd (working directory), pathway to CruiseData and CruiseSandbox at line 61, 66-67 in your local version to avoid specifying everytime.
#' 
#' @description
#' This will pull all images (ASNAP, FRAMEGRAB, HIGHLIGHT, SAMPLE, OBSERVATION) taken from 5 minutes prior to 1 minutes after the sampling time point. The function will create a "cache" folder in working directory to store the image metadata from each dive, keeping those files will reduce the operation time. The function will create a folder "Preview" (if still not exists) and copy all images from defined time range into the folder. Both Scicam and Sitcam images will be copied. The function will delete all existing image from the "Preview" folder everytime before copying new images into it.
#' 
#' @import tidyverse
#' @import readxl
#' 
#' The function takes following argument:
#' @param Specimen.OCSS 
#' @param lran Range limit prior to sampling time point. Default to 5 min.
#' @param rran Range limit after sampling time point. Default to 1 min.
#' @param wd Working directory.
#' @param CruiseData Path to Fkt Drive "CruiseData". Eg. for Mac: "/Volumes/CruiseData", for Windows: "Y:/" (double check with your own laptop)
#' @param CruiseSandbox Path to Fkt Drive "CruiseSandbox". Eg. for Mac: "/Volumes/CruiseSandbox", for Windows: "Z:/" (double check with your own laptop)
#' 
#' @example pull_from_sealog(655)
#' 
#' @export
pull_from_sealog <- function(Specimen.OCSS, lran = 5, rran = 1, wd = NULL, CruiseData = NULL, CruiseSandbox = NULL) {
    options(warn = -1)
    options(message = -1)
    
    Specimen.OCSS <- as.numeric(Specimen.OCSS)
    
    nin <- Negate("%in%")
    
    # Check dependencies
    if (!require(tidyverse)) {
        install <- 0
        while (nin(install, c("y", "n"))) {
            install <- readline(prompt = "Couldn't find {tidyverse}, install from CRAN? <y/n>: ")
        }
        if (install == "n") {
            stop("This function require {tidyverse} for data wrangling. Go check https://www.tidyverse.org")
        } else {
            install.packages("tidyverse")
            require(tidyverse)
        }
    }
    
    if (!require(readxl)) {
        install <- 0
        while (nin(install, c("y", "n"))) {
            install <- readline(prompt = "Couldn't find {readxl}, install from CRAN? <y/n>: ")
        }
        if (install == "n") {
            stop("This function require {readxl} to read spreadsheets. Go check https://readxl.tidyverse.org")
        } else {
            install.packages("readxl")
            require(readxl)
        }
    }
    
    #### == Settings == ####
    
    # Working Place
    if (is.null(wd)) {wd <- "12 Dive Documents/Script"} # Windows user has to change all back slash into normal slash "/". Back slash will escape next symbol in R.
    preview_folder <- paste(wd, "Preview", sep = "/")
    dir.create(preview_folder)
    
    # Path to Drive
    if (is.null(CruiseData)) {CruiseData <- "/Volumes/CruiseData"}
    if (is.null(CruiseSandbox)) {CruiseSandbox <-"/Volumes/CruiseSandbox"}
    
    CruiseID <- "Fkt250220"
    DiveLoc <- "Vehicles/Subastian"
    
    # Clean Preview Folder
    pre_previews <- list.files(preview_folder, full.names = T)
    file.remove(pre_previews)
    
    # Path to Specimen Log
    SpecimenLog.folder <- paste(CruiseSandbox, "ParticipantData/02_Specimen_Data/Specimen_log", sep = "/")

    files <- list.files(SpecimenLog.folder, pattern = "^FKt250220 Master Specimen Log", full.names = T) 
    
    mtime <- file.mtime(files)
    
    SpecimenLog_Loc <- files[which(mtime == max(mtime))]

    # Path to Evento Log
    EventLog.Loc <- paste(CruiseSandbox, "ParticipantData/02_Specimen_Data/Subastien_event_log/EventLog.xlsx", sep = "/")
    
    
    CruiseID_K <- gsub("k", "K", CruiseID) # Sealog CruiseId has captial K as "FKt"
    DiveLoc_B <- gsub("b", "B", DiveLoc) # Same: Subastian vs SuBastian
    
    #### == Source == ####
    
    # SPECIMEN LOG
    # source(paste0(wd, "/SpecimenLog.R"))
    Specimen.Log <- suppressMessages(read_xlsx(SpecimenLog_Loc))
    
    Specimen.Log_cleaned <- Specimen.Log %>% 
        mutate(
            Dive_no = factor(gsub(pattern = "O", replacement = "0", x = Dive_no)), # S0 and SO are mixed
            Event_no = as.numeric(Event_no)
        )
    
    # EVENT LOG
    # source(paste0(wd, "/EventLog.R"))
    EventLog <- suppressMessages(read_xlsx(EventLog.Loc, sheet = "EventLog"))
    
    EventLog_cleaned <- EventLog %>% mutate(
        `Dive Number` = gsub(pattern = "O", replacement = "0", x = `Dive Number`) %>% factor(),
        `Dive-Event` = gsub(pattern = "O", replacement = "0", x = `Dive-Event`) %>% factor()
    )
    
    Dive.masterFolders <- list.files(paste0(paste(CruiseData, CruiseID, sep = "/"), '/', DiveLoc), pattern = "_S0[0-9]{3}", full.names = T) %>% 
        data.frame('FolderPath' = .) %>%
        mutate(DiveID = str_extract(FolderPath, "S0[0-9]{3}"))
    
    # Extract Specimen Metadata
    if (nin(Specimen.OCSS, Specimen.Log_cleaned$`OCSS unique no.`)) {stop("OCSS number not found in specimen log.")}
    
    Specimen.LogEntry <- Specimen.Log_cleaned %>%
        filter(`OCSS unique no.` == Specimen.OCSS)
    Specimen.DiveNo <- Specimen.LogEntry$Dive_no %>% as.character()
    Specimen.EventNo <- Specimen.LogEntry$Event_no %>% as.numeric()
    
    if (is.na(Specimen.EventNo)) {
        stop("Non-numeric entry of event id. Check specimen log.")
    }
    
    Specimen.DiveEvent <- paste0(Specimen.DiveNo, "E", Specimen.EventNo)
    Specimen.EventEntry <- EventLog_cleaned %>% filter(`Dive-Event` == Specimen.DiveEvent)
    Specimen.UTC <- Specimen.EventEntry$`DateTime [calculated automatically]` %>% dmy_hm()
    
    # Extract correspond Dive File Metadata
    ImagesFolder <- paste(CruiseData, CruiseID_K, DiveLoc_B, paste(CruiseID_K, Specimen.DiveNo, sep = "_"), "Sealog/Images", sep = "/")
    
    # Initiate Cache Folder
    cacheFolder <- paste0(wd, "/cache")
    dir.create(cacheFolder)
    
    # Pull Metadata from Cache if available
    cache.list <- list.files(cacheFolder)
    if (paste0(Specimen.DiveNo, ".rds") %in% cache.list) {
        ImageFileList <- read_rds(paste0(wd, "/cache/", Specimen.DiveNo, ".rds"))
    } else {
        ImageFileList <- list.files(ImagesFolder)
        print("Pulling image file metadata and building cache, this may take a while.")
        ImageFileList <- data.frame("FileName" = ImageFileList) %>%
            mutate(
                UTC_original = str_extract(string = FileName, pattern = "[0-9]{8}_[0-9]{4}") %>% ymd_hm()
            )
        write_rds(ImageFileList, file = paste0(wd, "/cache/", Specimen.DiveNo, ".rds"))
    }
    
    # Define Pull Range and Target Files
    PullRange <- c(lran, rran)
    LeftLim <- Specimen.UTC - minutes(PullRange[1])
    RightLim <- Specimen.UTC + minutes(PullRange[2])
    
    Specimen.ImageList.rel <- ImageFileList %>% 
        filter(UTC_original > LeftLim & UTC_original < RightLim) %>%
        mutate(FullPath = paste(ImagesFolder, FileName, sep = "/"))
    
    # Pull Files
    walk2(Specimen.ImageList.rel$FullPath, Specimen.ImageList.rel$FileName, \(x, y) {
        file.copy(x, paste(preview_folder, y, sep = "/"))
    })
    
    cat(paste0(
        "Images successfully pulled.", "\n",
        "OCSS_", Specimen.OCSS, " Dive: ", Specimen.DiveNo, " Event: ", Specimen.EventNo
    ))
    
    options(warn = 0)
    options(message = 0)
}
