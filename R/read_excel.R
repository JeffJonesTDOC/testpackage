# This function reads in all relevant excel sheets from the ROI excel file given by file_name.
# The study argument is used to differentiate between sheet name variations depending on the amount of years used in the analysis.


read_excel <- function(study,file_name) {
  tryCatch({
    if (study == "YOY" | study == "1YR" | study=="2YR") {
      roiSheet = openxlsx::read.xlsx(file_name,sheet="ROI")
    } else {roiSheet = openxlsx::read.xlsx(file_name,sheet="ROI_pooled")}
  },error=function(e) {
    print("Error in read_excel when reading ROI sheet.")
    print(e)
    stop()
  })
  tryCatch({
    dataSummarySheet = openxlsx::read.xlsx(file_name,sheet="Data summary")
  },error=function(e) {
    print("Error in read_excel when reading Data summary sheet.")
    print(e)
    stop()
  })
  tryCatch({
    summaryStatsSheet = openxlsx::read.xlsx(file_name,sheet="Summary stats")
  },error=function(e) {
    print("Error in read_excel when reading summary stats sheet.")
    print(e)
    stop()
  })
  tryCatch({
    dataOverviewSheet = openxlsx::read.xlsx(file_name,sheet="data_overview1")
  },error=function(e) {
    print("Error in read_excel when reading data_overview1 sheet.")
    print(e)
    stop()
  })
  tryCatch({
    pharmacyCostsSheet = openxlsx::read.xlsx(file_name,sheet="Pharmacy costs")
  },error=function(e) {
    print("Error in read_excel when reading pharmacy costs sheet.")
    print(e)
    stop()
  })
  tryCatch({
    PDCSheet = openxlsx::read.xlsx(file_name,sheet="PDC")
  },error=function(e) {
    print("Error in read_excel when reading PDC sheet.")
    print(e)
    stop()
  })
   return(list(roiSheet = roiSheet, dataSummarySheet = dataSummarySheet,summaryStatsSheet = summaryStatsSheet,dataOverviewSheet = dataOverviewSheet,pharmacyCostsSheet = pharmacyCostsSheet,PDCSheet = PDCSheet))
}
