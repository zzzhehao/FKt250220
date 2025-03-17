pull_HL <- function(DiveNo) {
    require(tidyverse)
    
    DiveNo <- "S0797"
    
    nin <- Negate("%in%")
    
    wd <- "."
    HL.log.fileName <- paste0(wd, "/cache/Highlight_log_", DiveNo, ".rds")
    interwd <- "wd"
    # HL.target.folder <- "Highlight_segment"
    HLFolder <- "/Volumes/T7/SSI Media/SuBastian/DiveHighlight"
    HL.inter.folder <- paste(HLFolder, interwd, sep = "/")
    
    copy.cmd.sh <- paste(HLFolder, interwd, "copy.sh", sep = "/")
    
    interVids <- list.files(HL.inter.folder, pattern = ".mov", full.names = T)
    file.remove(interVids)
    
    # read HL log
    files <- list.files(paste(wd, "cache", sep = "/"), full.names = T)
    
    if (HL.log.fileName %in% files) {
        HL.log <- read_rds(HL.log.fileName)
    } else {
        stop("No HL log found.")
    } 
    
    HL.request <- HL.log %>% filter(taskStatus == "pending")
    if (nrow(HL.request) == 0) {
        print("Nothing to execute.")
        return()
    }
    
    confirm <- "a"
    while(nin(confirm, c("y", "n"))) {
        confirm <- readline(paste("Found request from serial number ", min(HL.request$OpSerialNo), " to ", max(HL.request$OpSerialNo), ", execute all? <y/n> \n>>> ", sep = ""))
        if (confirm == "y") {
            exe.start <- min(HL.request$OpSerialNo)
            exe.end <- max(HL.request$OpSerialNo)
        }
        if (confirm == "n") {
            stop("Haven't construct this part yet.")
        }
    }
    
    for (exe in exe.start:exe.end) {
        HL.log <- read_rds(HL.log.fileName)
        HL.request <- HL.log %>% filter(taskStatus == "pending")
        # exe = 6
        request.entry <- HL.request %>% filter(OpSerialNo == exe)
        write(request.entry$CopyCmd, copy.cmd.sh)
    
        system(paste0("sh '", copy.cmd.sh, "'")) # Execute copy cmd
        if (nrow(request.entry) == 0) {next}
        if (request.entry$VidCount > 1) {
            # Do video merge
            interVids <- request.entry$InterFilePath %>% str_split(",", simplify = T) %>% .[1,]
            write(paste0("file '", interVids, "'"), paste(HLFolder, interwd, "mergeScheme.txt", sep = "/"))
            mergeCmd <- paste0("ffmpeg -f concat -safe 0 -i '", paste(HLFolder, interwd, "mergeScheme.txt", sep = "/"), "' -c copy '", request.entry$vidFileTargetPath, "'")
            system(mergeCmd)
            file.remove(interVids)
        }
        if (request.entry$VidCount == 1) {
            file.rename(request.entry$InterFilePath, request.entry$vidFileTargetPath)
        }
    
        HL.log$taskStatus[exe] <- "complete"
        write_rds(HL.log, HL.log.fileName)
    }
}
