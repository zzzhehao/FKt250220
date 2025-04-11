#' This function search for a given keyword in the entire SuBastian sealog from given dives (already set as S0797 to S0804, still waiting for S0805 sealog files).
#' 
#' @description 
#' To import this function, simple run this script (use the "Source" button in RStudio). For usage see <Example>. The keyword must be included in quotation pair `'` or `"` to be passed as character string. The function only search for simple match of the input keyword and sealog text. Sadly no ambiguous search. Therefore it might be smart to search for singular like "coral" instead of plural "corals". Cases are ignored.
#' 
#' @details
#' The searched fields are <Free text>, <Comment>, <Note>, <Description>, <Event subtype> (where biologist could click what type of animal it is), basically all places where people were typing text in. At the end of the function a brief overview of the event time (UTC), text input, event type (Observation/Sample/etc.), dive number and event ID (if applicable) will be displayed. Pulled images will be copied to "Preview" folder under your R's working directory. Use `getwd()` and `setwd()` to manage working directory.
#' 
#' @author Zhehao Hu
#' 
#' @example Search_Sealog("dragonfish")

Search_Sealog <- function (Searchtxt) {    
    #### Change / Check this line:
    toCruiseData <- "/Volumes/CruiseData" # Pathway to the drive "CruiseData", Windows user should have something like `"Z:/"` and has to replace all backslash with normal slash
    DiveNo <- c(797:805) # When S0805 sealog is in drive, just change 804 to 805
    #### =======
    
    options(warn = -1)
    options(message = -1)
    
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
    
    DiveID <- paste0("S0", DiveNo)
    nin <- Negate("%in%")
    
    CruiseID <- "FKt250220"
    toDiveFolder <- "Vehicles/SuBastian"
    DiveFolders <- paste(toCruiseData, CruiseID, toDiveFolder, paste(CruiseID, DiveID, sep = "_"), sep = "/")
    toCSVs <- paste(DiveFolders, "Sealog", paste(CruiseID, DiveID, "eventOnlyExport.csv", sep = "_"), sep = "/")
    
    # Paths and Settings
    
    SealogFile <- paste0("Sealog_S0", min(DiveNo), "_S0", max(DiveNo), ".rds")
    SealogPath <- paste0("./cache/", SealogFile)
    cacheFiles <- list.files("./cache", pattern = ".rds")
    
    if (SealogFile %in% cacheFiles) {
        Sealog <- readRDS(SealogPath)
    } else {
        print("No local sealog found, pulling from drive. This might take a while.")
        
        Sealog <- map_dfr(toCSVs, \(x) {
            read.csv(x, header = T, sep = ",") %>% mutate(Dive = str_split_i(string = x, pattern = "_", i = 3))
        })
        
        Sealog <- Sealog %>%
            mutate(
                All_text = paste(Dive, event_free_text, event_option.comment, event_option.description, event_option.event_comment, event_option.notes, event_option.subtype, sep = ";"),
                UTC_Original = gsub("T|Z", " ", ts) %>% ymd_hms())
        
        write_rds(Sealog, file = SealogPath)
    }
    
    CSearchtxt <- toupper(Searchtxt)
    lSearchtxt <- tolower(Searchtxt)
    
    Sealog.request <- Sealog %>% 
        filter(str_detect(All_text, Searchtxt) | str_detect(All_text, CSearchtxt) | str_detect(All_text, lSearchtxt)) %>%
        dplyr::select(c("UTC_Original", "event_value", "All_text", "Dive", "event_option.event_id"))
    
    print(Sealog.request$All_text)
    
    pullImage <- "a"
    while (pullImage == "a") {
        pullImage <- readline("Type a row number to pull image from the event. Type `n` to exit. \n>>>")
        if (pullImage == "n") {return()}
        if (is.na(as.numeric(pullImage))) {
            pullImage <- "a"
        } else {
            request.rowNo <- as.numeric(pullImage)
            if (request.rowNo > nrow(Sealog.request)) {
                print(paste0("Requested number exceeds the search result. There are only ", nrow(Sealog.request), " results."))
                pullImage <- "a"
            } else {
                break
            }
        }
    } 
    
    # Pull request time point
    Sealog.pull.entry <- Sealog.request[request.rowNo,]
    UTC_original <- Sealog.pull.entry$UTC_Original
    
    # Initiate Cache Folder
    cacheFolder <- paste0("./cache")
    dir.create(cacheFolder)
    cache.list <- list.files(cacheFolder)
    
    # Extract correspond Dive File Metadata
    ImagesFolder <- paste(DiveFolders, "Sealog", "Images", sep = "/")
    ImagesFolder <- paste("/Volumes/ZH_1_1/FKt250220/Sealog", DiveID, "Sealog/Images", sep = "/")
    
    
    # Pull Metadata from Cache if available
    ImageMeta <- map_dfr(DiveID, \(x) {
        if (paste0(x, ".rds") %in% cache.list) {
            ImageFileList <- read_rds(paste0("./cache/", x, ".rds"))
            return(ImageFileList)
        } else {
            ImageFileList <- list.files(ImagesFolder[which(DiveID == x)])
            print("Pulling image file metadata and building cache, this may take a while.")
            ImageFileList <- data.frame("FileName" = ImageFileList) %>%
                mutate(UTC_intermed = str_extract(string = FileName, pattern = "[0-9]{8}_[0-9]{9}")) %>%
                mutate(UTC_original = paste0(
                    substr(UTC_intermed, 1, 4), "-", 
                    substr(UTC_intermed, 5, 6), "-", 
                    substr(UTC_intermed, 7, 8), " ", 
                    substr(UTC_intermed, 10, 11), ":", 
                    substr(UTC_intermed, 12, 13), ":", 
                    substr(UTC_intermed, 14, 15), ".", 
                    substr(UTC_intermed, 16, 18), sep = "") %>% ymd_hms(),
                    Dive = x
                )
            write_rds(ImageFileList, file = paste0("./cache/", x, ".rds"))
            return(ImageFileList)
        }
    })
    
    PreviewFolder <- "./Preview"
    dir.create(PreviewFolder)
    Preview.files <- list.files(PreviewFolder, full.names = T)
    file.remove(Preview.files)
    

    pull_images <- function(UTC) {
        
        lran <- 1
        rran <- 2
        # Define Pull Range and Target Files
        Pull.UTC <- UTC
        llim <- Pull.UTC - minutes(lran)
        rlim <- Pull.UTC + minutes(rran)
        
        ImageFolder <- ImagesFolder[str_detect(ImagesFolder, Sealog.pull.entry$Dive)]
        
        Preview.files <- ImageMeta %>% 
            filter(UTC_original > llim & UTC_original < rlim) %>%
            filter(!str_detect(FileName, "SITTOO")) %>%
            mutate(FullPath = paste(ImageFolder, FileName, sep = "/"))
        
        walk2(Preview.files$FullPath, Preview.files$FileName, \(x, y) {
            file.copy(x, paste(PreviewFolder, y, sep = "/"))
        })
    }
    
    pull_images(UTC_original)
    UTC_adjust <- UTC_original
    
    fin <- "non"
    while (nin(fin, c("n"))) {
        fin <- readline(paste0("Currently showing images from left limit: ", UTC_adjust - minutes(1), " to right limit: ", UTC_adjust + minutes(2), ". \nType `p`/`m` (plus/minus) and a number (in min) to shift the time frame (e.g. `p1`), or `n` to exit."))
        if (fin == "n") {break}

        if (str_detect(fin, "m") | str_detect(fin, "p")) {
            shift <- fin %>% 
                gsub(pattern = "p", replacement = "", x = .) %>%
                gsub(pattern = "m", replacement = "", x = .)
            shift <- as.numeric(shift) %>% hms::hms(minutes = .)
            if (str_detect(fin, "p")) {
                UTC_adjust <- UTC_adjust + shift
            } else {
                UTC_adjust <- UTC_adjust - shift
            }
            pull_images(UTC_adjust)
            fin <- "non"
        }
    }
    
    print(paste("Text entry:", Sealog.pull.entry$All_text))
    print(Sealog.pull.entry %>% dplyr::select(-c("All_text"))) %>%
        `colnames<-`(c("UTC", "Event type", "Dive", "Event ID"))
    
    options(warn = 0)
    options(message = 0)
}
