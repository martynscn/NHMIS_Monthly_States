cat(as.character(Sys.time()), "==","Script Download NHMIS monthly data for states started successfully\n")

# libraries ####
library("httr",quietly = TRUE,warn.conflicts = FALSE)
library("rjson",quietly = TRUE,warn.conflicts = FALSE)
library("plyr",quietly = TRUE,warn.conflicts = FALSE)
library("dplyr",quietly = TRUE,warn.conflicts = FALSE)
library("XML",quietly = TRUE,warn.conflicts = FALSE)
library("RCurl",quietly = TRUE,warn.conflicts = FALSE)
library("tibble", quietly = TRUE, warn.conflicts = FALSE)
library("RCurl", quietly = TRUE, warn.conflicts = FALSE)
library(stringr, quietly = TRUE, warn.conflicts = FALSE)

config <- config::get()
username <- config$NHMISInstanceUsername
password <- config$NHMISInstancePassword



# Server definition option ####
server_version <- config$ServerVersion #Enter "yes" or "no"
first_time <- "yes"
currentDataOnly <- "yes" # This means just the current dataset not necessarily incremental

if(server_version == "no") {
  setwd("~/R_projects/NHMIS_Monthly_Analysis/")
}
DXXs <- read.csv(file = "dataelementsid.csv", header = TRUE, sep = ",", as.is = TRUE)
# login <- read.csv(file = "DHIS2login.csv")
if(server_version == "yes") {
  setwd("data")
  source("/srv/shiny-server/e4e-apps/functions/collect_return_period_fxn2.R")
  # username <- as.character(login$username)
  
} else if (server_version == "no") {
  setwd("~/R_projects/NHMIS_Monthly_Analysis/data/")
  source("~/R_projects/R programming/DHIS2 data extract/Functions/collect_return_period_fxn2.R")
  # username <- as.character(login$ï..username)
  
}


files <- list.files()
no_of_files <- length(files) 

combined_file_name <- "combined.csv"
new_file_name <- "new.csv"
ind_file_name <- paste0("NHMIS_Monthly_State_Level","_",(no_of_files + 1),".csv")
current_data_name <- "currentData.csv"


# credentials ####
timeout <-240


ou <- c("s5DPBsdoE8b;LEVEL-1;LEVEL-2")
# priorYears <- c("2015","2016")
currentYear <- as.numeric(format.Date(Sys.Date(), "%Y"))

priorYears <- as.character(seq(2015,currentYear - 2))
# pe <- "201809;201810" 

if(first_time == "yes" || currentDataOnly == "yes") {
  pe <- paste(paste0(priorYears,collapse = ";"),
              collect_return_period2(sDate = "2017-1-1", eDate = Sys.Date(), oneYeartoOmit = 2018, omitMultipleYears = FALSE, extraction_type = "NHMIS_MONTHLY"),
              sep = ";",collapse = ";")
} else if (first_time == "no" && currentDataOnly == "no") {
  pe <- "LAST_MONTH"
}

dp <- "NAME"
oIS <- "NAME"
tl <- "true"
cols <- "dx"
rows <- "pe;ou"
ft <- ""
agg <- ""
mc <- ""
pamc <- ""
sm <- "true"
sd <- "false"
sr <- "true"
hm <- "false"
il <- "true"
her <- "true"
sh <- "false"
ind <- "false"
iis <- ""
al <- ""
rpd <- ""
uou <- ""

# defining the data elements ####
alldx <- c('MQP6Q5EnrMM','xTS5sF4TloW','OZH9GfZqZ7q','PV88LZCbiSF','pUZ0BKgsAXp','fP1E4qRJ57a','bGcL2xrAMSe','DyMyl4NeDxz','w6nOgEFHWMG','xIvjr5gGVpp','sq1t1xsNl1J','PJQ5Qd8OPck','TccZXCCJymC','k2Ncm24FnEA','m5LcaggVQJr','CyuNtbglHse','KWJ3cSuyzs4','r6WOvUlcQm6','GEd2F6skCpT','m5gTF1jKQjl','G7iWnz9RMy9','k2Cpuz9BQtD','K9K1G0XX4ax','HdtaLx63988','TCKBTltaIqq','XcWlUTGQCv3','mz0LIaWBAz9','Jc1WjNKrObY','JiqRpGPcjXz','YjZiHDKMWCJ','beZ1WYNONnH','yHw3A23QqP8','u4S0guxNQA1','ahcgdMr6xYw','SDmCprzAStG','obcu03IYMEz','ETqcBdGA1uj','hvYqrmR6MPq','NO1ASEzyurW','PFHeLBDLciz','HdtaLx63988.W50DgZrPrUk','HdtaLx63988.ItGQGDRFD4t','ouzURM9c1FI.W50DgZrPrUk','ouzURM9c1FI.ItGQGDRFD4t','mz0LIaWBAz9.HBvhntn3sam','mz0LIaWBAz9.k5pHlvkYR3T','mz0LIaWBAz9.jaAWWXYY0mP','mz0LIaWBAz9.VYVkx4fIeut','mz0LIaWBAz9.ohFoRKDEJyr','lyVV9bPLlVy.REPORTING_RATE','ktsIGwM8eKk','HH0iQLlqddM','iIbhvOXScP5','BWjIm3FvPSS','joWyNIq0XtY','pPTpnHWnpTu','PU87QXkUyyf','quCqGMTBTHi','l2eIpdluuTI','LwGvXW3TDYG','WAywN48snnd','C2uGy964KNe','vYRF9XQAt6t',
           'IDvOsgRb9vK','iXWVkRayCy9','WU44C0TDRUA','acmdhxLWZb2','hH5OJAHVSaD','wT67R90qCCY')
# skippeddx <- c("Jc1WjNKrObY","JiqRpGPcjXz","yHw3A23QqP8","ahcgdMr6xYw")
somedx <- alldx[1:length(alldx)]
errors <- NULL
errors2 <- NULL
indicatorNameList <- NULL
indicatorNameList2 <- NULL
allIndicators <- NULL
counter <- 1
fullcounter <- 1
unsuccesfuldxs <- NULL
unsuccesfulInds <- NULL
succesfulInds <- NULL
succesfuldxs <- NULL
alldx <- NULL
for(dx in somedx){
  last.dx <- NULL
  last.indicatorName <- NULL
  indicatorName <- DXXs[DXXs[,"ID"] == dx,"Name"][1]
  allIndicators <- c(allIndicators,indicatorName)
  alldx <- c(alldx, dx)
  
  tryCatch({
    # Try block ####
    # analytics url definition ####
  analytics_url <- paste0("https://dhis2nigeria.org.ng/dhis/api/29/analytics.json?",
                          if(dx != "") {paste0("&dimension=dx:",dx)},
                          if(ou != "") {paste0("&dimension=ou:",ou)},
                          if(pe != "") {paste0("&dimension=pe:",pe)},
                          if(ft != "") {paste0("&filter=",ft)},
                          if(agg != "") {paste0("&aggregationType=",agg)},
                          if(mc != "") {paste0("&measureCriteria=",mc)},
                          if(pamc != "") {paste0("&preAggregationMeasureCriteria	=",pamc)},
                          if(sm != "") {paste0("&skipMeta=",sm)},
                          if(sd != "") {paste0("&skipData=",sd)},
                          if(sr != "") {paste0("&skipRounding=",sr)},
                          if(hm != "") {paste0("&hierarchyMeta=",hm)},
                          if(il != "") {paste0("&ignoreLimit=",il)},
                          if(her != "") {paste0("&hideEmptyRows=",her)},
                          if(sh != "") {paste0("&showHierarchy=",sh)},
                          if(ind != "") {paste0("&includeNumDen=",ind)},
                          if(iis != "") {paste0("&inputIdScheme=",iis)},
                          if(al != "") {paste0("&approvalLevel=",al)},
                          if(rpd != "") {paste0("&relativePeriodDate=",rpd)},
                          if(uou != "") {paste0("&userOrgUnit=",uou)},
                          if(dp != "") {paste0("&displayProperty=",dp)},
                          if(oIS != "") {paste0("&outputIdScheme=",oIS)},
                          if(tl != "") {paste0("&tableLayout=",tl)},
                          if(cols != "") {paste0("&columns=",cols)},
                          if(rows != "") {paste0("&rows=",rows)})
  url <- URLencode(analytics_url)
  # Get request based on analytics url ####
  r <- httr::GET(url = url, httr::authenticate(username,password),httr::timeout(seconds = timeout))
  
  r <- httr::content(r, "text",encoding = "UTF-8")
  d <- jsonlite::fromJSON(r, flatten=TRUE)
  # indicatorName <- d[["headers"]][["column"]][length(d[["headers"]][["column"]])]
  Rdata <- as.data.frame(d$rows,stringsAsFactors=FALSE)
  names(Rdata) <- d$headers$name
  if("Organisation unit" %in% colnames(Rdata)) {
    Rdata$State <- gsub(" state","", Rdata$`Organisation unit`, ignore.case = TRUE)
    Rdata$State <- gsub("Federal Government","National",gsub("Federal Capital Territory","FCT", substr(Rdata$State,unlist(lapply(gregexpr(" ", Rdata$State, ignore.case = TRUE), function(k) if(k[[1]] == 3) {k[[1]]} else {k[[2]]}))+1, 100)))
    Rdata$`Organisation unit` <- Rdata$State
    Rdata <- Rdata[,!(names(Rdata) %in% c("State"))]
    
  if(sum(ifelse(grepl("Local Government Area", Rdata$`Organisation unit`), 1,0)) > 0) {
      Rdata$State <- "Nigeria"
      Rdata$`Organisation unit` <- gsub("Local Government Area","LGA", substr(Rdata$`Organisation unit`, unlist(lapply(gregexpr(" ",Rdata$`Organisation unit`, ignore.case = TRUE),function(k) if(k[[1]] == 3) {k[[1]]} else {k[[2]]}))+1,100))
      Rdata <- plyr::rename(Rdata, c("Organisation unit" = "LGA"))}}
  if("ou" %in% colnames(Rdata)) { 
    if(sum(ifelse(grepl("State",Rdata$ou),1,0)) > 0) {
      Rdata$State <- gsub(" state","", Rdata$ou, ignore.case = TRUE)
      Rdata$State <- gsub("Federal Government","National",gsub("Federal Capital Territory","FCT", substr(Rdata$State,unlist(lapply(gregexpr(" ", Rdata$State, ignore.case = TRUE), function(k) if(k[[1]] == 3) {k[[1]]} else {k[[2]]}))+1, 100)))
      Rdata$`Organisation unit` <- Rdata$State}
    if(sum(ifelse(grepl("Local Government Area", Rdata$ou), 1,0)) > 0) {
      Rdata$State <- sheet_name
      Rdata$ou <- gsub("Local Government Area","LGA", substr(Rdata$ou, unlist(lapply(gregexpr(" ",Rdata$ou, ignore.case = TRUE),function(k) if(k[[1]] == 3) {k[[1]]} else {k[[2]]}))+1,100))
      Rdata <- plyr::rename(Rdata, c("ou" = "LGA"))}}
  first_cols <- c("Period")
  Rdata <- Rdata[,c(first_cols,setdiff(colnames(Rdata),first_cols))]
  drops <- c("Period ID", "Period code", "Period description", "Organisation unit ID", "Organisation unit code", "Organisation unit description","ou")
  Rdata <- Rdata[, !(names(Rdata) %in% drops)]
  if(counter == 1){
    MergedData <- Rdata
  }else if (counter > 1) {
    MergedData <- left_join(x = MergedData, y = Rdata, by = c("Period","Organisation unit"))
    }
  counter <- counter + 1
  succesfulInds <- c(succesfulInds,indicatorName)
  succesfuldxs <- c(succesfuldxs,dx)
  }
  ,error = function(e) {
    errors <<- c(errors,dx);indicatorNameList <<- c(indicatorNameList,indicatorName)
    last.dx <<- dx; last.indicatorName <<- indicatorName
    unsuccesfuldxs <<- c(unsuccesfuldxs,dx); unsuccesfulInds <<- c(unsuccesfulInds,indicatorName)}
  )
  errors2 <- c(errors2,last.dx)
  indicatorNameList2 <- c(indicatorNameList2,last.indicatorName)
  fullcounter <- fullcounter + 1
}
# Sorting data ----
Rdata_sort <- MergedData
Rdata_sort <-
  Rdata_sort %>% arrange(factor(
    Period,
    levels = c(
      "2015",
      "2016",
      "2017",
      "January 2017",
      "February 2017",
      "March 2017",
      "April 2017",
      "May 2017",
      "June 2017",
      "July 2017",
      "August 2017",
      "September 2017",
      "October 2017",
      "November 2017",
      "December 2017",
      "January 2018",
      "February 2018",
      "March 2018",
      "April 2018",
      "May 2018",
      "June 2018",
      "July 2018",
      "August 2018",
      "September 2018",
      "October 2018",
      "November 2018",
      "December 2018",
      "2018",
      "January 2019",
      "February 2019",
      "March 2019",
      "April 2019",
      "May 2019",
      "June 2019",
      "July 2019",
      "August 2019",
      "September 2019",
      "October 2019",
      "November 2019",
      "December 2019",
      "2019"
    )
  ), `Organisation unit`)
errors_df <- data.frame(ID = errors, stringsAsFactors = FALSE)
unsuccesfuldxs_df <- data.frame(ID = unsuccesfuldxs, stringsAsFactors = FALSE)
succesfuldxs_df <- data.frame(ID = succesfuldxs, stringsAsFactors = FALSE)
all_dxs_df <- data.frame(ID = alldx, stringsAsFactors = FALSE)

# joining data elements to know successful or not ####
tryCatch({
  errors_dx_names <- left_join(x = errors_df, y = DXXs,by = "ID")
}, error = function(e){
  print("Empty errors_df")
})
tryCatch({
  unsuccesfuldxs_dx_names <- left_join(x = unsuccesfuldxs_df, y = DXXs,by = "ID")
},error = function(e){
  print("Empty unsuccesfuldxs_dx_names")
})

tryCatch({
  succesfuldxs_dx_names <- left_join(x = succesfuldxs_df, y = DXXs,by = "ID")
}, error = function(e){
  print("Empty succesfuldxs_dx_names")
})

tryCatch({
  all_dx_names <- left_join(x = all_dxs_df, y = DXXs,by = "ID") 
}, error= function(e){
  print("Empty all_dx_names")
})

#Finalise the storage ####
RRData <- add_column(.data = Rdata_sort, Date_exported = as.character(Sys.time()), .before = 1)
if(currentDataOnly == "no") {
  write.csv(RRData,new_file_name, row.names = FALSE)
  newData <- read.csv(file = new_file_name, as.is = TRUE, check.names = FALSE)
  if(first_time == "yes") {
    write.csv(as.data.frame(RRData),combined_file_name, row.names = FALSE)
  } else if (first_time == "no") {
    initialCombinedData <- read.csv(file = combined_file_name, as.is = TRUE, check.names = FALSE)
    combinedData <- bind_rows(initialCombinedData,newData)
    write.csv(combinedData,combined_file_name, row.names = FALSE)
  }
} else if (currentDataOnly == "yes") {
  write.csv(as.data.frame(RRData),file = current_data_name,row.names = FALSE)
}

write.csv(as.data.frame(RRData),file = ind_file_name,row.names = FALSE)
setwd("..")
cat(as.character(Sys.time()), "==","Script Download NHMIS Monthly State data completed successfully\n\n----------\n")
