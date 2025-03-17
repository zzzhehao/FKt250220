request_HL <- function(DiveNo, overide.serial.no = NULL, continue = T) {
    # DiveNo <- "S0797"
    require(tidyverse)
    require(timechange)
    nin <- Negate("%in%")

    wd <- "."
    HL.log.fileName <- paste0(wd, "/cache/Highlight_log_", DiveNo, ".rds")

    # Path to Drive
    CruiseData <- "/Volumes/CruiseData"
    CruiseSandbox <-"/Volumes/CruiseSandbox"
    interwd <- "wd"
    HLFolder <- "/Volumes/T7/SSI Media/SuBastian/DiveHighlight"
    # HLFolders <- "/Volumes/T7/SSI Media/SuBastian/DiveHighlight"
    # HLFolder <- paste(HLFolders, DiveNo, sep = "/")
    dir.create(HLFolder)
    HLThumbnailFolder <- "/Volumes/T7/SSI Media/SuBastian/DiveHighlight_Thumbnail"
    dir.create(HLThumbnailFolder)

    CruiseID <- "FKt250220"
    DiveLoc <- "Vehicles/SuBastian"
    ImageLocIn <- "Sealog/Images"
    
    DiveFolders <- paste(CruiseData, CruiseID, DiveLoc, sep = "/")
    Dives <- list.files(DiveFolders, pattern = "S[0-9]{4}$")
    ImagesFolder <- paste(DiveFolders, paste(CruiseID, DiveNo, sep = "_"), ImageLocIn, sep = "/")

    HL.target.folder <- HLFolder
    HL.inter.folder <- paste(HL.target.folder, interwd, sep = "/")
    # HL.inter.folder <- "Highlight_segment"

    # Initiate Cache Folder
    cacheFolder <- paste(wd, "cache", sep = "/")
    dir.create(cacheFolder)
    dir.create(HL.target.folder)
    dir.create(HL.inter.folder)

    if (!is.null(overide.serial.no)) {
        files <- list.files(paste(wd, "cache", sep = "/"), pattern = ".rds", full.names = T)
        if (HL.log.fileName %in% files) {
            HL.log <- read_rds(HL.log.fileName)
            # start.at.event <- overide.serial.no
        } else {
            stop("HL log not found, overide not possible.")
        }
    } else {
        if (continue) {
            files <- list.files(paste(wd, "cache", sep = "/"), pattern = ".rds", full.names = T)
            if (HL.log.fileName %in% files) {
                HL.log <- read_rds(HL.log.fileName)
                start.at.event <- max(HL.log$OpSerialNo) + 1
                print(paste("Last entry was at event", start.at.event - 1, ", now starting at", start.at.event))
            } else {
                print("HL log file not found, initializing.")
                HL.log <- data.frame(
                    FileName = NULL,
                    UTC_original = NULL,
                    eventType = NULL,
                    HL.start = NULL,
                    HL.end = NULL,
                    Title = NULL,
                    Note = NULL,
                    EventRootID = NULL,
                    OpDateTime = NULL,
                    OpSerialNo = NULL,
                    taskStatus = NULL,
                    vidFileTargetPath = NULL,
                    CopyCmd = NULL,
                    VidCount = NULL,
                    InterFilePath = NULL
                )
                start.at.event <- 1
            }
        } 
        if (!continue) {
            c <- "A"
            while (nin(c, c("y", "n"))) {
                c <- readline("Starting from beginning will overwrite the current progress, continue? <y/n> \n>>> ")
                if (c == "n") {
                    stop("User abort")
                } 
                if (c == "y") {
                    file.remove(HL.log.fileName)
                    print("Initializing HL log.")
                    HL.log <- data.frame(
                        FileName = NULL,
                        UTC_original = NULL,
                        eventType = NULL,
                        HL.start = NULL,
                        HL.end = NULL,
                        Title = NULL,
                        Note = NULL,
                        EventRootID = NULL,
                        OpDateTime = NULL,
                        OpSerialNo = NULL,
                        taskStatus = NULL,
                        vidFileTargetPath = NULL,
                        CopyCmd = NULL,
                        VidCount = NULL,
                        InterFilePath = NULL
                    )
                    start.at.event <- 1
                }
            }
        }
    }

    # Pull Metadata from Cache if available
    cache.list <- list.files(cacheFolder)
    if (paste0(DiveNo, ".rds") %in% cache.list) {
        ImageFileList <- read_rds(paste0(wd, "/cache/", DiveNo, ".rds"))
    } else {
        ImageFileList <- list.files(ImagesFolder)
        print("Pulling image file metadata and building cache, this may take a while.")
        ImageFileList <- data.frame("FileName" = ImageFileList) %>%
            mutate(
                UTC_original = str_extract(string = FileName, pattern = "[0-9]{8}_[0-9]{4}") %>% ymd_hm()
            )
        write_rds(ImageFileList, file = paste0(wd, "/cache/", DiveNo, ".rds"))
    }

    ImageFileList.rel <- ImageFileList %>%
        mutate(
            CAM = str_split_i(string = FileName, pattern = "_", i = 1),
            eventType = str_split_i(string = FileName, pattern = "_", i = 4), 
            eventTypeSub = str_split_i(string = FileName, pattern = "_", i = 5)) %>%
        mutate(eventType = case_when(
            !is.na(eventTypeSub) ~ paste(eventType, eventTypeSub, sep = "_") %>% gsub(pattern = ".jpg", replacement = "", x = .),
            .default = gsub(pattern = ".jpg", replacement = "", x = eventType)
        )) %>% 
        filter(CAM == "SCICAM")

    EventTypeInt <- c("SOI_HIGHLIGHT", "SAMPLE", "SCIENCE_FRAMEGRAB")

    ImageFileList.int <- ImageFileList.rel %>% filter(eventType %in% EventTypeInt)

    if (nrow(ImageFileList.int) < start.at.event) {
        print("All event requested. Or overide serial number exceeds the total highlight count.")
        return(NULL)
    }


    if (!is.null(overide.serial.no)) {
        if (overide.serial.no > nrow(HL.log)) {
            stop("Overide serial number exceeds HL log entries count. Nothing to overide.")
        }
        Event.entry <- ImageFileList.int %>% filter(FileName == HL.log$FileName[overide.serial.no])
    } else {
        Event.entry <- ImageFileList.int[start.at.event,]
        overide.serial.no <- start.at.event
    }

    PreviewFolder <- "Highlight_Preview"
    dir.create(PreviewFolder)
    Preview.files <- list.files(PreviewFolder, full.names = T)
    file.remove(Preview.files)

    lran <- minutes(1)
    rran <- minutes(2)

    HL.time <- Event.entry[["UTC_original"]] %>% ymd_hms()

    llim <- HL.time - lran
    rlim <- HL.time + rran

    Preview.files <- ImageFileList %>% 
        filter(UTC_original > llim & UTC_original < rlim) %>%
        filter(!str_detect(FileName, "SITTOO")) %>%
        mutate(FullPath = paste(ImagesFolder, FileName, sep = "/"))

    walk2(Preview.files$FullPath, Preview.files$FileName, \(x, y) {
        file.copy(x, paste(PreviewFolder, y, sep = "/"))
    })

    cat("==============================\nAdjust left or right range by `l/r` + h:m:s in UTC (eg. '12:13:00').\nType `y` to proceed, type `s` to skip this highlight, type `c` to clear preview folder.\n==============================\n", sep = " ")

    input1 <- "n"

    while (input1 != "y") {
        input1 <- readline(paste0(
            "Event Serial No: ", start.at.event, 
            ", Start: ", llim, " End: ", rlim, 
            ", HL type: ", Event.entry$eventType, 
            ", ", rlim - llim %>% as.period(),
            "\n>>> ", sep = " "))
        if (input1 %in% c("s", "sy")) { # Skip
            title <- "Skipped event"
            note <- ""
        
            HL.entry <- Event.entry %>%
                dplyr::select(c("FileName", "UTC_original", "eventType")) %>%
                mutate(
                    HL.start = llim, 
                    HL.end = rlim, 
                    Title = title,
                    Note = note,
                    EventRootID = gsub(patter = ".jpg", replacement = "", x = FileName),
                    OpDateTime = Sys.time(),
                    OpSerialNo = case_when(
                        is.null(HL.log$OpSerialNo) ~ 1,
                        .default = max(HL.log$OpSerialNo) + 1
                    ),
                    taskStatus = "skipped",
                    vidFileTargetPath = NA,
                    CopyCmd = NA,
                    VidCount = 0,
                    InterFilePath = NA
                )
        
            HL.log.new <- bind_rows(HL.log, HL.entry)
            write_rds(HL.log.new, HL.log.fileName)

            if (input1 != "sy") {
                next_HL <- readline("Highlight skipped. Proceed to next highlight? <y/n/new DiveNo>\n>>> ")
            } else {
                next_HL <- "y"
            }
        
            if (next_HL == "y") {
                request_HL(DiveNo)
                return()
            } else {
                return()
            }
        }
        if (input1 == "c") {
            Preview.files <- list.files(PreviewFolder)
            file.remove(Preview.files)
        }
        if (str_detect(input1, "l")) {
            if (str_detect(input1, "^l[0-9]{6}$")) {
                input1 <- paste(substr(input1, 1, 3), ":", substr(input1, 4, 5), ":", substr(input1, 6, 7), sep = "")
                hms_input <- gsub("l", "", input1) %>% as.character()
                hms_input <- str_split(hms_input, ":", n = 3, simplify = T)
                hi <- hms_input[1,1] %>% as.numeric()
                mi <- hms_input[1,2] %>% as.numeric()
                si <- hms_input[1,3] %>% as.numeric()
                llim <- time_update(llim, hour = hi, minute = mi, second = si)
            }
        }
        if (str_detect(input1, "r")) {
            if (str_detect(input1, "^r[0-9]{6}$")) {
                input1 <- paste(substr(input1, 1, 3), ":", substr(input1, 4, 5), ":", substr(input1, 6, 7), sep = "")
                hms_input <- gsub("r", "", input1) %>% as.character()
                # hms_input <- gsub("\\.", ":", hms_input) %>% as.character()
                hms_input <- str_split(hms_input, ":", n = 3, simplify = T)
                hi <- hms_input[1,1] %>% as.numeric()
                mi <- hms_input[1,2] %>% as.numeric()
                si <- hms_input[1,3] %>% as.numeric()
                rlim <- time_update(rlim, hour = hi, minute = mi, second = si)
            }
        }

        Preview.files <- ImageFileList %>% 
        filter(UTC_original > llim & UTC_original < rlim) %>%
            filter(!str_detect(FileName, "SITTOO")) %>%
            mutate(FullPath = paste(ImagesFolder, FileName, sep = "/"))

        walk2(Preview.files$FullPath, Preview.files$FileName, \(x, y) {
            file.copy(x, paste(PreviewFolder, y, sep = "/"))
        })
    }

    title <- readline("Title\n>>> ")
    note <- readline("Notes\n>>> ")

    HL.entry <- Event.entry %>%
        dplyr::select(c("FileName", "UTC_original", "eventType")) %>%
        mutate(
            HL.start = llim, 
            HL.end = rlim, 
            Title = title,
            Note = note,
            EventRootID = gsub(patter = ".jpg", replacement = "", x = FileName),
            OpDateTime = Sys.time(),
            OpSerialNo = case_when(
                is.null(HL.log$OpSerialNo) ~ 1,
                !is.null(HL.log$OpSerialNo) & is.null(overide.serial.no) ~ max(HL.log$OpSerialNo) + 1,
                !is.null(overide.serial.no) ~ start.at.event # both synced earlier
            ),
            taskStatus = "pending",
        ) %>% mutate(
            vidFileTargetPath = paste0(paste(
                HL.target.folder, 
                paste(
                    DiveNo, 
                    OpSerialNo,
                    Title, 
                    FileName %>% 
                        gsub(pattern = "SCICAM_", replacement = "", x = .) %>%
                        gsub(pattern = ".jpg", replacement = "", x = .), 
                    sep = "_"), 
                sep = "/"), ".mov")
        )

    # Video
    VideoLocIn <- "Video/SCICAM"

    Vid.exTime <- function(Filename) {
        Filename %>% str_split_i("_", i = 2) %>% gsub(pattern = "T", replacement = "", x = .) %>% str_extract("[0-9]{14}") %>% paste0(substr(., 1, 4), "-", substr(., 5, 6), "-", substr(., 7, 8), " ", substr(., 9, 10), ":", substr(., 11, 12), ":", substr(., 13, 14)) %>% substr(., 15, 36) %>% ymd_hms()
    }

    # Pull Metadata from Cache if available
    cache.list <- list.files(cacheFolder)
    VideoFolder <- paste(DiveFolders, paste(CruiseID, DiveNo, sep = "_"), VideoLocIn, sep = "/")
    if (paste0(DiveNo, "v.rds") %in% cache.list) {
        VideoFileList <- read_rds(paste0(wd, "/cache/", DiveNo, "v.rds"))
    } else {
        VideoFileList <- list.files(VideoFolder)
        print("Pulling video file metadata and building cache, this may take a while.")
        VideoFileList <- data.frame("FileName" = VideoFileList) %>%
            mutate(
                UTC_start = Vid.exTime(FileName)
            )
        write_rds(VideoFileList, file = paste0(wd, "/cache/", DiveNo, "v.rds"))
    }

    Vid.prior.start <- HL.entry$HL.start > VideoFileList$UTC_start
    Vid.piror.end <- HL.entry$HL.end > VideoFileList$UTC_start

    Vid.start <- VideoFileList[sum(HL.entry$HL.start > VideoFileList$UTC_start),]
    Vid.end <- VideoFileList[sum(HL.entry$HL.end > VideoFileList$UTC_start),]

    # Check if required video segment exceeds single file
    singleFile <- Vid.start$FileName == Vid.end$FileName

    if (singleFile) {
        Vid <- VideoFileList[sum(HL.entry$HL.end > VideoFileList$UTC_start),]

        Vidt <- Vid %>% mutate(
            invid_start = llim - UTC_start,
            invid_end = rlim - UTC_start,
        ) %>% mutate(
            invid_ind_start = invid_start %>% as.numeric() %>% hms::hms(minutes = .) %>% as.character() %>% substr(1, 8),
            invid_ind_end = invid_end %>% as.numeric() %>% hms::hms(minutes = .) %>% as.character() %>% substr(1, 8),
            vidFileFullPath = paste(VideoFolder, FileName, sep = "/"),
            vidFileIntertPath = paste(HL.inter.folder, FileName, sep = "/"),
        ) %>% mutate(
            ffmpeg.cmd = paste("ffmpeg -i '", vidFileFullPath, "' -ss ", invid_ind_start, " -to ", invid_ind_end, " -c copy '", vidFileIntertPath, "'", sep = "")
        )
    } else {
        ind <- Vid.prior.start + Vid.piror.end
        st <- sum(ind == 2)
        ed <- st + sum(ind == 1)
        Vid <- VideoFileList[st:ed,]

        # Calculate in-vid time indicator
    Vidt <- Vid %>% mutate(UTC_end = UTC_start + minutes(10)) %>%
        mutate(
            invid_start = case_when(
                llim > UTC_start ~ llim - UTC_start,
                .default = NULL),
            invid_end = case_when(
                rlim < UTC_end ~ rlim - UTC_start,
                .default = NULL
            )
        ) %>% 
        mutate(
            invid_ind_start = case_when(
                !is.na(invid_start) ~ invid_start %>% as.numeric() %>% hms::hms(minutes = .) %>% as.character() %>% substr(1, 8),
                .default = hms::hms(minutes = 0) %>% as.character() %>% substr(1, 8)
            ),
            invid_ind_end = case_when(
                !is.na(invid_end) ~ invid_end %>% as.numeric() %>% hms::hms(minutes = .) %>% as.character() %>% substr(1, 8),
                .default = hms::hms(minutes = 0) %>% as.character() %>% substr(1, 8)
            ),
            vidFileFullPath = paste(VideoFolder, FileName, sep = "/"),
            vidFileIntertPath = paste(HL.inter.folder, FileName, sep = "/"),
        ) %>% 
        mutate(
            ffmpeg.cmd = case_when(
                is.na(invid_end) ~ paste("ffmpeg -i '", vidFileFullPath, "' -ss ", invid_ind_start, " -c copy '", vidFileIntertPath, "'", sep = ""),
                !is.na(invid_end) ~ paste("ffmpeg -i '", vidFileFullPath, "' -ss ", invid_ind_start, " -to ", invid_ind_end, " -c copy '", vidFileIntertPath, "'", sep = "")
            )
        )
    }
    
    HL.entry$CopyCmd <- paste(Vidt$ffmpeg.cmd, collapse = "\n")
    HL.entry$InterFilePath <- paste(Vidt$vidFileIntertPath, collapse = ",")
    HL.entry$VidCount <- nrow(Vidt)
    HL.entry <- HL.entry %>% mutate(VidDuration = (HL.end - HL.start) %>% as.period())

    if (!is.null(overide.serial.no)) {
        HL.log <- HL.log %>% filter(OpSerialNo != overide.serial.no)
    }
    HL.log.new <- bind_rows(HL.log, HL.entry)
    write_rds(HL.log.new, HL.log.fileName)

    thumbnail_check <- "a"
    while (nin(thumbnail_check, c("y", "n", "?"))) {
        thumbnail_check <- readline("Rename thumbnai? <y/n/?>\n>>> ")
        if (thumbnail_check == "y") {
            thumbnail <- list.files(HLThumbnailFolder, pattern = "^SCICAM_")
            thumbnailFullPath <- list.files(HLThumbnailFolder, pattern = "^SCICAM_", full.names = T)
            thumbnail.c <- length(thumbnail)
            if (thumbnail.c == 0) {
                thumbnail_check <- "no"
                print("No unnamed thumbnail found.")
            }
            if (thumbnail.c > 1) {
                thumbnail_check <- "toomuch"
                print("More than 1 unnamed thumbnail found.")
            }
            if (thumbnail.c == 1) {
                thumbnailName <- paste(DiveNo, HL.entry$OpSerialNo, HL.entry$Title, thumbnail, sep = "_")
                tmbrnc <- file.rename(thumbnailFullPath, paste(HLThumbnailFolder, thumbnailName, sep = "/"))
                if (tmbrnc) {print(paste("Renamed as", thumbnailName))}
            }
        }
        if (thumbnail_check == "?") {
            thumbnail_check <- "reset"
            print(paste("Copy ONE unnamed SCICAM picture to", HLThumbnailFolder, "and the the program will automatically rename the thumbnail. Type `y` to proceed or `n` to skip."))
        }
    }

    next_HL <- readline("Request ordered. Proceed to next highlight? <y>\n>>> ")
    if (next_HL == "y") {
        request_HL(DiveNo)
        return()
    }
    return()
}

# request_HL("S0797")
# request_HL("S0797", 3)
# read_rds("cache/Highlight_log_S0797.rds")
#