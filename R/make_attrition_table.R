# This function takes various data pieces and generates population attrition tables, one for each cohort. The tables
# are returned as flextable objects.


make_attrition_table <- function(nYear,summaryStatsSheet,minEnrollmentLength,population_conditions,studyTimePeriodStartDate) {

  # Initialize a few arrays that are population in the following for loop.
  attritionNCount = array(dim=nYear)
  activatedCount = array(dim=nYear) # activatedCount will be used in the result overview savings flextable.
  riskScoreArray = array(dim=nYear); ageArray = array(dim=nYear); memCohortArray = array(dim=nYear); nonMemCohortArray = array(dim=nYear)


  for (l in 1:nYear) {

    # Locate the starting row of summaryStatsSheet to extract the attrition table, for both variations of text.
    attritionTableRowStart = which(summaryStatsSheet[,3] == "Enrolled within study period")
    if (length(attritionTableRowStart) == 0) {attritionTableRowStart = which(summaryStatsSheet[,3] == "Ever enrolled members") }

    attritionDF = summaryStatsSheet[attritionTableRowStart:(attritionTableRowStart+16),(3+7*(l-1)):(7+7*(l-1))] # Extract the lth attrition table.

    # Populate a few of the arrays with data pieces.
    riskScoreArray[l] = as.numeric(summaryStatsSheet[which(summaryStatsSheet$X3=="Risk Score"),(4+7*(l-1))])
    ageArray[l] = as.numeric(summaryStatsSheet[which(summaryStatsSheet$X3=="Age"),(4+7*(l-1))])

    # Remove the non-applicable "Enrolled for more than n months" rows
    if (any(grepl("9 months",attritionDF[,1]))) {
      if (minEnrollmentLength == 3) {
        attritionDF = attritionDF[-which(grepl("6 months|9 months",attritionDF[,1])),]
      } else if (minEnrollmentLength == 6) {
        attritionDF = attritionDF[-which(grepl("3 months|9 months",attritionDF[,1])),]
      } else if (minEnrollmentLength == 9) {
        attritionDF = attritionDF[-which(grepl("3 months|6 months",attritionDF[,1])),]
      }
    } else {
      if (minEnrollmentLength == 3) {
        attritionDF = attritionDF[-which(grepl("6 months|9 months",attritionDF[,1])),]
      } else {
        attritionDF = attritionDF[-which(grepl("3 months|9 months",attritionDF[,1])),]
      }
    }

    # Extract the "activated within study period" data piece.
    activatedCount[l] = as.numeric(attritionDF[2,4])

    # Remove the non-applicable population_conditions rows, and replace the last row with "Final Number"
    if (tolower(population_conditions) == "100k annual") {
      attritionDF = attritionDF[-which(grepl("50k|50,000|95th|All|Final",attritionDF[,1])),]
    } else if (tolower(population_conditions) == "50k monthly") {
      attritionDF = attritionDF[-which(grepl("100k|100,000|All|Final",attritionDF[,1])),]
    } else if (tolower(population_conditions) == "capped") {
      attritionDF = attritionDF[-which(grepl("50k|50,000|100k|than 100,000|All|Final",attritionDF[,1])),]
    }
    attritionDF[nrow(attritionDF),1] = "Final Population Sample Size"


    # Remove the "Pre-launch eligible" row and rename the "Post-launch eligible" row to "Pre-launch and Post-launch eligible"
    attritionDF = attritionDF[-which(grepl("Pre-launch eligible",attritionDF[,1])),]
    attritionDF[which(grepl("Post-launch eligible",attritionDF[,1])),1] = "Pre-launch and post-launch eligible"


    # Remove the first row, "Enrolled in <study timeframe>", the new conditions row, and the final row.
    if (length(which(grepl("Enrolled within study period",attritionDF[,1]))) > 0) {
      attritionDF = attritionDF[-c(which(grepl("Enrolled within study period",attritionDF[,1]))),]
    } else if (length(which(grepl("Ever enrolled",attritionDF[,1]))) > 0) {
      attritionDF = attritionDF[-c(which(grepl("Ever enrolled",attritionDF[,1]))),]
    }
    attritionDF = attritionDF[-c(which(grepl("Remove populations with",attritionDF[,1]))),]

    # Flip the outlier removal row from % and # removed to % and # remaining.
    if (population_conditions != "Capped") {
      outlierNMemb =  attritionDF[grepl("with more than",attritionDF[,1]),4]
      outlierNNonMemb = attritionDF[grepl("with more than",attritionDF[,1]),2]
      attritionDF[which(grepl("Enrolled",attritionDF[,1]))+1,2] = as.numeric(attritionDF[grepl("Enrolled",attritionDF[,1]),2]) - as.numeric(outlierNNonMemb)
      attritionDF[which(grepl("Enrolled",attritionDF[,1]))+1,4] = as.numeric(attritionDF[grepl("Enrolled",attritionDF[,1]),4]) - as.numeric(outlierNMemb)
      attritionDF[which(grepl("Enrolled",attritionDF[,1]))+1,3] = as.numeric(attritionDF[grepl("Enrolled",attritionDF[,1]),2]) / as.numeric(attritionDF[which(grepl("Total unique",attritionDF[,1])),2])
      attritionDF[which(grepl("Enrolled",attritionDF[,1]))+1,5] = as.numeric(attritionDF[grepl("Enrolled",attritionDF[,1]),4]) / as.numeric(attritionDF[which(grepl("Total unique",attritionDF[,1])),4])
      for (i in 1:nrow(attritionDF)) {attritionDF[i,1] = str_replace(attritionDF[i,1],"with more than","not with more than")}
    }

    # Add a final "Matched Row" where member sample size = non-member sample size for clarity.
    matchedRow = as.data.frame(t(c("Final Matched Population",0," ",0," ")))
    matchedRow[1,2] = attritionDF[nrow(attritionDF),4]; matchedRow[1,4] = matchedRow[1,2]
    colnames(matchedRow) = colnames(attritionDF)
    attritionDF = rbind(attritionDF,matchedRow)

    # Extract cohort size data pieces.
    memCohortArray[l] = as.numeric(attritionDF[6,4]); nonMemCohortArray[l] = as.numeric(attritionDF[6,2])

    # For YOY, rename the "activated in" row to "Ever activated".
    if (study=="YOY") {attritionDF[grepl("Activated",attritionDF[,1]),1] = "Ever activated prior to launch date."}

    # Reformat some percentages and include study time periods in relevant table rows, for clarity.
    attritionNCount[l] = attritionDF[nrow(attritionDF),4]
    for (i in 1:(nrow(attritionDF)-1)) {
      if (attritionDF[i,1]=="Activated within study period") {
        attritionDF[i,1] = paste("Activated within study period (",
                                 format(ymd(as.Date(studyTimePeriodStartDate)+(365*(l-1))),"%m/%Y"),
                                 "-",
                                 format(ymd(as.Date(studyTimePeriodStartDate)+(365*l)-30),"%m/%Y"),
                                 ")",
                                 sep="")
      }
      if (attritionDF[i,1]=="Enrolled within study period") {
        attritionDF[i,1] = paste("Enrolled within study period (",
                                 format(ymd(as.Date(studyTimePeriodStartDate)+(365*(l-1))),"%m/%Y"),
                                 "-",
                                 format(ymd(as.Date(studyTimePeriodStartDate)+(365*l)-30),"%m/%Y"),
                                 ")",
                                 sep="")
      }
      for(j in 1:ncol(attritionDF)) {
        if ((j == 3|j ==5)&!is.na(attritionDF[i,j])) {attritionDF[i,j] = percent(as.numeric(attritionDF[i,j]),digits = 3)}
      }
    }

    # Replace PwD with PwH for hypertension ROIs.
    if (program=="Hypertension") attritionDF[2,1] = gsub("PwD","PwH",attritionDF[2,1])

    # Generate the flextable for the lth attrition table.
    colnames(attritionDF) = c("Criteria","Count","Percent","Count ","Percent ")
    ft <- flextable::flextable(attritionDF)
    ft <- flextable::add_header_row(ft,colwidths = c(1,2,2),values = c(" ","Non-Member","Member"),)
    ft <- flextable::width(ft,j=1,width=5.2)
    ft <- flextable::width(ft,j=2:5,width=1)
    ft <- flextable::height(ft,height=0.3)
    ft <- flextable::bg(ft,i=1:2,j=1:5, bg = "#66478F", part = "header")
    ft <- flextable::bg(ft,i=1,j=1:5, bg = "#55437d", part = "header")
    ft <- flextable::bg(ft,i=1:nrow(attritionDF),j=1, bg = "#6A696C"); ft <- flextable::bg(ft,i=1,j=1, bg = "#6A696C", part="footer")
    ft <- flextable::bg(ft,i=1:nrow(attritionDF),j=2:5, bg = "#E1DDE5"); ft <-flextable::bg(ft,i=1,j=2:5, bg = "#E1DDE5",part="footer")
    ft <- flextable::fontsize(ft, i = 1:2, size = 16, part="header"); ft <- flextable::bold(ft,i=1:2,part="header")
    ft <- flextable::fontsize(ft,size=12,part="footer"); ft <- flextable::bold(ft,part="footer")
    ft <- flextable::fontsize(ft,size=12, part="body")
    ft <- flextable::font(ft,fontname = "Century Gothic",part="all")
    ft <- flextable::align(ft,align="center",part="header")
    ft <- flextable::bold(ft,i=nrow(attritionDF),j=c(2,4),bold=T)
    ft <- flextable::color(ft,color="white", part="header"); ft <- flextable::color(ft,color="white",j=1, part="all")
    ft <- flextable::border_outer(ft,border=officer::fp_border(color="black",style="solid",width=2),part="all")
    ft <- flextable::border_inner(ft,border=officer::fp_border(color="black",style="solid",width=2),part="header")
    ft <- flextable::border_inner(ft,border=officer::fp_border(color="white",style="solid",width=1),part="body")
    ft <- flextable::border_inner(ft,border=officer::fp_border(color="white",style="solid",width=1),part="footer")
    assign(paste("attrTableY",l,sep=""),ft)
  }

  if (nYear == 1) return(list(riskScoreArray, ageArray, memCohortArray,nonMemCohortArray, attritionNCount, attrTableY1))
  if (nYear == 2) return(list(riskScoreArray, ageArray, memCohortArray,nonMemCohortArray, attritionNCount, attrTableY1, attrTableY2))
  if (nYear == 3) return(list(riskScoreArray, ageArray, memCohortArray,nonMemCohortArray, attritionNCount, attrTableY1, attrTableY2, attrTableY3))
  if (nYear == 4) return(list(riskScoreArray, ageArray, memCohortArray,nonMemCohortArray, attritionNCount, attrTableY1, attrTableY2, attrTableY3, attrTableY4))

}
