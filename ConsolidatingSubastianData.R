# This script is originally written by Oenone Scott, modified by Zhehao Hu

library(tidyverse)

# Path to Subastian sealog folder
to.drive <- "/Volumes/Public/FKt250220/FKt250220_Sealog"
dives <- paste0("S0", 797:805)

all <- map(dives, \(dive) { # apply following process to all dives
    # Locate dive RVDAS folder
    to.data <- paste(to.drive, dive, "OpenRVDAS", sep = "/" )
    
    # Files in interest
    files <- list.files(
        to.data, 
        full.names = T, 
        pattern = regex(paste("sb_ctd_sbe49_depth_corr", "sb_ctd_sbe49", "sb_oxygen_corr", "sb_sprint", sep = "|"))
    )
    
    dive.con <- map(files, \(file) { # apply following process to all files
        file.connection <- file
    
        ### Check if header has same length as data body
        # Note: _sb_sprint_ file has only 16 header but 17 columns in data (every row except header is ended with a comma which creates a ghost column at the end) 
        # Note: this might not be an isolated case. Hence I provide a generic approach of forcing header to match with data entry length to prevent future work.
        sep.delim <- ","
        file.plain <- read.delim(file, header = F)[[1]]
        header.len <- file.plain[1] %>% str_split(pattern = ",", simplify = T) %>% length()
        body.len <- file.plain[2] %>% str_split(pattern = ",", simplify = T) %>% length()
        if (header.len < body.len) {
            warning(paste0("Processing file ", file, ": Header has less columns than data actually contains (header has ", header.len, " but data has ", body.len, "). Adding ", body.len - header.len, " ghost columns at the header end."))
            header.fixed <- paste0(file.plain[1], rep(sep.delim, body.len - header.len))
            file.plain.fixed <- c(header.fixed, file.plain[-1])
        
            file.connection <- textConnection(file.plain.fixed)
        }
        if (header.len > body.len) {
            warning(paste0("Processing file ", file, ": Header has more columns than data actually contains. (Header has ", header.len, " but data has only ", body.len, ". One or more columns might be missing in the data. Removing the last ", header.len - body.len, " header columns."))
            
            header.fixed <- file.plain[1] %>% 
                str_split(",", simplify = T) %>%
                as.vector() %>%
                .[1:body.len] %>% 
                paste(collapse = ",")
            file.plain.fixed <- c(header.fixed, file.plain[-1])
            file.connection <- textConnection(file.plain.fixed)
        }
    
        # Extract data source
        source <- file %>% str_split("/", simplify = T) %>%
            as.vector() %>%
            .[length(.)] %>%
            str_split("_", simplify = T) %>%
            as.vector() %>%
            .[2:(length(.)-1)] %>% # remove cruise ID and Dive ID
            paste(collapse = "_")
        
        ## Read data as normal
        temp <- read.table(file.connection, header = T, sep = ",")

        # S0805 ctd sbe49 has a corrupted first line and Temperature_C column has mistery indention
        if (dive == "S0805" & source == "sb_ctd_sbe49") {
            temp <- temp[2:nrow(temp),] %>% mutate(Temperature_C = as.numeric(Temperature_C))
        }
    
        # time needs to be sorted out 
        times <- paste(substring(temp$Timestamp, 1,10), substring(temp$Timestamp, 12,19), sep = " ") # Extract time in POSIX readable format
        times.st <- strptime(x = times, format =  "%F %T", tz = "UTC") # Convert to POSIX
        times.min <- round.POSIXt(times.st, "mins") # Round to minute (evtl. floor to minute?)
    
        # Update data frame
        temp <- temp %>% mutate(Timestamp = times.min)

        # now average the rest of the data by time
        # group data by timestamp and average accross each column
        temp.averaged <- temp %>% 
            mutate(across(where(is.integer), as.numeric)) %>%
            group_by(Timestamp) %>% 
            summarize(across(where(is.numeric), mean)) # This step will remove all columns that is not numeric

        # Rename colnames to indicate source
        colnames(temp.averaged)[2:ncol(temp.averaged)] <- paste(source, names(temp.averaged)[2:ncol(temp.averaged)], sep = ".")
        return(temp.averaged)
    }) %>% 
        reduce(full_join, by = "Timestamp") %>% # Full join all data at the end to ensure maximum time coverage
        mutate(Dive = dive)
})

file.name <- paste(dives, "consolidated.csv", sep = "_")
export_path <- paste0("17 Dive Data/Consolidated openRVDAS/", file.name)

walk(1:length(dives), \(x) {
    write.csv(all[[x]], export_path[x], sep = ",", row.names = F)
})

write.csv(bind_rows(all), "17 Dive Data/Consolidated openRVDAS/concatenated.csv", sep = ",", row.names = F)
