library(tidyverse)
library(readxl)

EventLog.Loc <- "/Volumes/CruiseSandbox/ParticipantData/02_Specimen_Data/Subastien_event_log/EventLog.xlsx"

EventLog <- read_xlsx(EventLog.Loc, sheet = "EventLog")

EventLog_cleaned <- EventLog %>% mutate(
    `Dive Number` = gsub(pattern = "O", replacement = "0", x = `Dive Number`) %>% factor(),
    `Dive-Event` = gsub(pattern = "O", replacement = "0", x = `Dive-Event`) %>% factor()
)

