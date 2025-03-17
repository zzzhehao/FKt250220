library(tidyverse)
library(readxl)

SpecimenLog.folder <- "/Volumes/CruiseSandbox/ParticipantData/02_Specimen_Data/Specimen_log"

files <- list.files(SpecimenLog.folder, pattern = "^FKt250220 Master Specimen Log", full.names = T) 

mtime <- file.mtime(files)

SpecimenLog_Loc <- files[which(mtime == max(mtime))]
Specimen.Log <- read_xlsx(SpecimenLog_Loc)

str(Specimen.Log)

Specimen.Log_cleaned <- Specimen.Log %>% 
    mutate(
        Dive_no = factor(gsub(pattern = "O", replacement = "0", x = Dive_no)), # S0 and SO are mixed
        Event_no = as.numeric(Event_no),
        Dry = as.numeric(Dry)
    )

names(Specimen.Log_cleaned)
Subsample.index <- c(7:16)
Subsample.prefix <- names(Specimen.Log_cleaned)[Subsample.index]

Specimen.Subsample.Label <- Specimen.Log_cleaned %>% 
    dplyr::select(c(`OCSS unique no.`, Subsample.prefix)) %>%
    pivot_longer(cols = c(2:ncol(.)), names_to = "Subsample", values_to = "SeqID") %>%
    filter(!is.na(SeqID)) %>%
    mutate(
        Subsample = case_when(
            Subsample == "Dry" ~ "D",
            .default = Subsample
        ),
        Subsample.label = paste(Subsample, SeqID, sep = "")) %>%
    group_by(`OCSS unique no.`) %>%
    summarize(Subsample.label.char = paste(Subsample.label, collapse = ", "))

Specimen.sheet <- left_join(Specimen.Log_cleaned, Specimen.Subsample.Label, by = "OCSS unique no.")
