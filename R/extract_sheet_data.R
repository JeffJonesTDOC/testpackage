# This function extracts all relevant data pieces from various sheets in the ROI excel file that were extracted using read_excel.
# Relevant data pieces are lvgoPrice, nYear (number of years of the analysis), study time period start and launch date,
# year 1 and year 0, minimum enrollment length (3, 6, or 9 months), client name, and supply cost (for DM).
#

extract_sheet_data <- function(roiSheet, program, study, dataSummarySheet, dataOverviewSheet,HTN_population) {
  # Extract the price of livongo before modifying the file in some cases.
  lvgoPrice = round(as.numeric(roiSheet[which(roiSheet[,1] == 'Price of Livongo'),3][1]),0)

  # Modify the sheets for some oddball cases involving Hypertension
  {
    if (program=="Hypertension") {
      if (study == "2YR" | study == "3YR" | study == "4YR") {
        if (HTN_population == "All") {roiSheet=roiSheet[1:which(roiSheet[,1]=="DMHTN"),]}
        if (HTN_population == "HTN & DM") {roiSheet=roiSheet[(which(roiSheet[,1]=="DMHTN")+3):which(roiSheet[,1]=="HTN only"),]}
        if (HTN_population == "HTN only") {roiSheet=roiSheet[(which(roiSheet[,1]=="HTN only")+3):nrow(roiSheet),]}
      }
      if (study == "YOY" | study == "1YR") {
        if (HTN_population == "HTN & DM") {
          roiSheet[1:7,] = roiSheet[21:27,]
          spendTableToUse = roiSheet[which(roiSheet[,15]=="Total costs")[1]:which(roiSheet[,15]=="Office visits")[1],15:24]
          roiSheet[which(roiSheet[,2]=="Total costs")[1]:which(roiSheet[,2]=="Office visits")[1],2:11] = spendTableToUse
        }
        if (HTN_population == "HTN only") {
          roiSheet[1:7,] = roiSheet[41:47,]
          spendTableToUse = roiSheet[which(roiSheet[,28]=="Total costs")[1]:which(roiSheet[,28]=="Office visits")[1],28:37]
          roiSheet[which(roiSheet[,2]=="Total costs")[1]:which(roiSheet[,2]=="Office visits")[1],2:11] = spendTableToUse
        }
      }
    }
  }

  # Extract all relevant variables from the excel file.
  {
    ##### nYear extract #####
    if (study=="YOY") {
      nYear = 1
    } else {
      nYear = as.numeric(substr(study,1,1))
    }

    ##### start and launch date extract #####
    studyTimePeriodStartDate=dataSummarySheet$Column1[which(dataSummarySheet$Client=="Study period start date")]
    studyTimePeriodLaunchDate=dataSummarySheet$Column1[which(dataSummarySheet$Client=="Study Launch Date")]
    # Sometimes, for various reasons, launch and start dates are missing or out of place. Default to 1900-01-01 in this case
    # in order to prevent breaks in other functions.
    if (studyTimePeriodStartDate == 0) {
      studyTimePeriodStartDate="1900-01-01"
      studyTimePeriodLaunchDate="1900-01-01"
    }
    studyTimePeriodLaunchDate = as.Date(studyTimePeriodLaunchDate,"%Y-%m-%d")

    ##### year 0 and year 1 extract #####
    year1 = (substr(dataSummarySheet$Column1[which(dataSummarySheet$Client=='Launch Date')],1,4))
    year0 = as.character(as.integer(year1)-1)
    yearIndex = 2
    while (yearIndex <= nYear) {
      assign(paste("year",yearIndex,sep=""),as.character(as.integer(year1)+(yearIndex)-1))
      yearIndex=yearIndex+1
    }

    ##### minimum enrollment length extract #####
    if (program == "Diabetes") {
      minEnrollmentLength = dataOverviewSheet[which(dataOverviewSheet[,1] == "joined_months"),2]
    } else if (program == "Hypertension") {
      minEnrollmentLength = dataOverviewSheet[which(dataOverviewSheet[,1] == "chosen_pop_list"),2]
    }

    #####  client name extract #####
    clientName = dataOverviewSheet$data_overview[which(dataOverviewSheet$X1=="company_name")]
    if (length(clientName)==0) {clientName = dataOverviewSheet$data_overview[which(dataOverviewSheet[,1] =="company_name")]}
    if (length(clientName)==0) {clientName = "not found"}

    ##### supply cost extract #####
    if (program == "Diabetes") {
      supplyCost = roiSheet[which(grepl("Diabetes Supplies Cost",roiSheet[,1])),2][1]
      if (supplyCost == "0" || is.na(supplyCost)) {supplyCost = "30"}
    } else if (program == "Hypertension") {
      supplyCost = "0"
    }
  }

  return(list(lvgoPrice = lvgoPrice,roiSheet = roiSheet, nYear = nYear,studyTimePeriodStartDate = studyTimePeriodStartDate, studyTimePeriodLaunchDate = studyTimePeriodLaunchDate, year1 = year1, year0 = year0, minEnrollmentLength = minEnrollmentLength,clientName = clientName,supplyCost = supplyCost))

}
