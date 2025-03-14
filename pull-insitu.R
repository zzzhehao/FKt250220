library(tidyverse)
library(readxl)

#### == Settings == ####

# Working Place
wd <- "12 Dive Documents/Script"
preview_folder <- paste(wd, "Preview", sep = "/")
    dir.create(preview_folder)

# Clean Preview Folder
pre_previews <- list.files(preview_folder, full.names = T)
file.remove(pre_previews)

SpecimenLog_Loc <- "/Volumes/CruiseSandbox/ParticipantData/02_Specimen_Data/Specimen_log/FKt250220 Master Specimen Log_updated11Mar2025_v2.xlsx"
EventLog.Loc <- "/Volumes/CruiseSandbox/ParticipantData/02_Specimen_Data/Subastien_event_log/EventLog.xlsx"

CruiseData <- "/Volumes/CruiseData"
CruiseID <- "Fkt250220"
DiveLoc <- "Vehicles/Subastian"

Specimen.OCSS <- 288

CruiseID_K <- gsub("k", "K", CruiseID) # Sealog CruiseId has captial K as "FKt"
DiveLoc_B <- gsub("b", "B", DiveLoc) # Same: Subastian vs SuBastian

#### == Source == ####

# SPECIMEN LOG
# source(paste0(wd, "/SpecimenLog.R"))
Specimen.Log <- read_xlsx(SpecimenLog_Loc)

str(Specimen.Log)

Specimen.Log_cleaned <- Specimen.Log %>% 
    mutate(
        Dive_no = factor(gsub(pattern = "O", replacement = "0", x = Dive_no)), # S0 and SO are mixed
        Event_no = as.numeric(Event_no)
    )

# EVENT LOG
# source(paste0(wd, "/EventLog.R"))
EventLog <- read_xlsx(EventLog.Loc, sheet = "EventLog")

EventLog_cleaned <- EventLog %>% mutate(
    `Dive Number` = gsub(pattern = "O", replacement = "0", x = `Dive Number`) %>% factor(),
    `Dive-Event` = gsub(pattern = "O", replacement = "0", x = `Dive-Event`) %>% factor()
)

Dive.masterFolders <- list.files(paste0(paste(CruiseData, CruiseID, sep = "/"), '/', DiveLoc), pattern = "_S0[0-9]{3}", full.names = T) %>% 
    data.frame('FolderPath' = .) %>%
    mutate(DiveID = str_extract(FolderPath, "S0[0-9]{3}"))

# Extract Specimen Metadata

Specimen.LogEntry <- Specimen.Log_cleaned %>%
    filter(`OCSS unique no.` == Specimen.OCSS)
Specimen.DiveNo <- Specimen.LogEntry$Dive_no %>% as.character()
Specimen.EventNo <- Specimen.LogEntry$Event_no %>% as.character()
Specimen.DiveEvent <- paste0(Specimen.DiveNo, "E", Specimen.EventNo)
Specimen.EventEntry <- EventLog_cleaned %>% filter(`Dive-Event` == Specimen.DiveEvent)
Specimen.UTC <- Specimen.EventEntry$`DateTime [calculated automatically]` %>% dmy_hm()

# Extract correspond Dive File Metadata

ImagesFolder <- paste(CruiseData, CruiseID_K, DiveLoc_B, paste(CruiseID_K, Specimen.DiveNo, sep = "_"), "Sealog/Images", sep = "/")

cacheFolder <- paste0(wd, "/cache")
dir.create(cacheFolder)

cache.list <- list.files(cacheFolder)
if (paste0(Specimen.DiveNo, ".rds") %in% cache.list) {
    ImageFileList <- read_rds(paste0(wd, "/cache/", Specimen.DiveNo, ".rds"))
} else {
    ImageFileList <- list.files(ImagesFolder)
    print("Pulling image file metadata, this may take a while.")
    ImageFileList <- data.frame("FileName" = ImageFileList) %>%
        mutate(
            UTC_original = str_extract(string = FileName, pattern = "[0-9]{8}_[0-9]{4}") %>% ymd_hm()
        )
    write_rds(ImageFileList, file = paste0(wd, "/cache/", Specimen.DiveNo, ".rds"))
}

# Define Pull Range and Target Files
PullRange <- c(5, 1)
LeftLim <- Specimen.UTC - minutes(PullRange[1])
RightLim <- Specimen.UTC + minutes(PullRange[2])

Specimen.ImageList.rel <- ImageFileList %>% 
    filter(UTC_original > LeftLim & UTC_original < RightLim) %>%
    mutate(FullPath = paste(ImagesFolder, FileName, sep = "/"))

# Pull Files
walk2(Specimen.ImageList.rel$FullPath, Specimen.ImageList.rel$FileName, \(x, y) {
    file.copy(x, paste(preview_folder, y, sep = "/"))
    # print(paste(preview_folder, y, sep = "/"))
    # print(x)
})
