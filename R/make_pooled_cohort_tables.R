# This function generates the pooled cohort tables to show combined cohort sizes for multi-year analyses.
# The first table shows the general structure of how cohort sizes are calculated, and the second table shows the actual data.

make_pooled_cohort_tables <- function(nYear,attritionNCount, summaryStatsSheet, studyTimePeriodStartDate) {

  # Generate the first table data frame.
  pooledCohortTable1 = as.data.frame(matrix(nrow=nYear,ncol=2+nYear))
  table1Names = c("Cohorts","Activation Timeframe")
  for (i in 1:nYear) {table1Names = append(table1Names,paste("Year",i))}
  colnames(pooledCohortTable1) = table1Names
  for (i in 1:nYear) {
    pooledCohortTable1$Cohorts[i] = paste("Cohort",i)
    pooledCohortTable1$`Activation Timeframe`[i] = paste(format(lubridate::ymd(as.Date(studyTimePeriodStartDate)+(365*(i-1))),"%m/%Y"),"-",format(lubridate::ymd(as.Date(studyTimePeriodStartDate)+(365*i)-30),"%m/%Y"))
    pooledCohortTable1[i,3:(ncol(pooledCohortTable1)-i+1)] = 'X'
  }

  # Generate the second table data frame.
  pooledCohortTable2 = as.data.frame(matrix(nrow=nYear,ncol=1+nYear))
  table2Names = c("Groups")
  for (i in 1:nYear) {table2Names = append(table2Names,paste("Year",i))}
  colnames(pooledCohortTable2) = table2Names
  nSizeRow = which(summaryStatsSheet[,3] == "All")
  for (i in 1:nYear) {
    pooledCohortTable2$Groups[i] = paste("Cohort",i)
    pooledCohortTable2[i,2:(ncol(pooledCohortTable2)-i+1)] = attritionNCount[i]
  }
  footerRow = c("Pooled Cohort"); for(i in 2:ncol(pooledCohortTable2)){footerRow=append(footerRow,sum(na.omit(as.numeric(pooledCohortTable2[,i]))))}
  pooledCohortTable2 = rbind(pooledCohortTable2,footerRow)

  # Generate the first table flextable.
  pooledCohort_ft1 <- flextable(pooledCohortTable1)
  pooledCohort_ft1 <- add_header_row(pooledCohort_ft1,colwidths = c(2,nYear),values = c(" ","Analysis groupings by Year on Program"),)
  pooledCohort_ft1 <- bg(pooledCohort_ft1, bg = "#66478F", part = "header")
  pooledCohort_ft1 <- bold(pooledCohort_ft1,part="header")
  pooledCohort_ft1 <- color(pooledCohort_ft1,color="white", part="header")
  pooledCohort_ft1 <- align(pooledCohort_ft1,align="center",part="header")
  pooledCohort_ft1 <- width(pooledCohort_ft1,width=1)
  pooledCohort_ft1 <- width(pooledCohort_ft1,j=2,width=1.5)
  pooledCohort_ft1 <- font(pooledCohort_ft1,fontname="Century Gothic",part="all")
  pooledCohort_ft1 <- border_outer(pooledCohort_ft1,border=fp_border(color="black",style="solid",width=2),part="all")
  pooledCohort_ft1 <- border_inner(pooledCohort_ft1,border=fp_border(color="black",style="solid",width=2),part="header")
  pooledCohort_ft1 <- border_inner(pooledCohort_ft1,border=fp_border(color="black",style="solid",width=1),part="body")

  # Generate the second table flextable.
  pooledCohort_ft2 <- flextable(pooledCohortTable2)
  pooledCohort_ft2 <- bg(pooledCohort_ft2, bg = "#55437d", part = "header")
  pooledCohort_ft2 <- bold(pooledCohort_ft2,part="header")
  pooledCohort_ft2 <- color(pooledCohort_ft2,color="white", part="header")
  pooledCohort_ft2 <- bg(pooledCohort_ft2,i=nYear+1, bg="#66478F",part="body")
  pooledCohort_ft2 <- color(pooledCohort_ft2,i=nYear+1,color="white",part="body")
  pooledCohort_ft2 <- bold(pooledCohort_ft2,i=nYear+1,part="body")
  pooledCohort_ft2 <- width(pooledCohort_ft2,j=1,width=1.5)
  pooledCohort_ft2 <- font(pooledCohort_ft2,fontname="Century Gothic",part="all")
  pooledCohort_ft2 <- align(pooledCohort_ft2,align="center",part="all")
  pooledCohort_ft2 <- border_outer(pooledCohort_ft2,border=fp_border(color="black",style="solid",width=2),part="all")
  pooledCohort_ft2 <- border_inner(pooledCohort_ft2,border=fp_border(color="black",style="solid",width=2),part="header")
  pooledCohort_ft2 <- border_inner(pooledCohort_ft2,border=fp_border(color="black",style="solid",width=1),part="body")

  returnList = list(pooledCohortTable1,pooledCohortTable2,pooledCohort_ft1,pooledCohort_ft2)
  names(returnList) = c("table1","table2","flextable1","flextable2")
  return(returnList)
}
