# This function's primary purpose is to generate the ROI table used in the
# ROI results slide. However, it also produces and returns several other ROI-related
# tables and arrays used in other slides or for data capture.


make_roi_table <- function(has_rx, nYear, program, attritionNCount,lvgoPrice,PMPM_changes, supplyCost, pooledCohortTable2) {
  # Define finalROITable, which becomes a flextable object, formatted
  # and injected into the powerpoint slide.
  {
    if (has_rx == T) {
      finalROITable = as.data.frame(matrix(nrow=6,ncol=nYear+1))
      finalROITable[,1] = c("Net Medical Costs","Net Rx Costs","Net Diabetes Rx Costs","ROI","Rx-Adjusted ROI","DM Rx-Adjusted ROI")
      finalROITableHeaders <- array(" ")
    } else {
      finalROITable = as.data.frame(matrix(nrow=2,ncol=nYear+1))
      finalROITable[,1] = c("Net Medical Costs","ROI")
      finalROITableHeaders <- array(" ")
    }
    executiveSummaryRoiArray = array() # used later on in the executive summary slide generation
    rxRoiArray = array(); noRxRoiArray = array(); diabetesRxRoiArray = array()
    for (i in 1:nYear) {
      if (nYear > 1) {finalROITableHeaders[i+1] = paste("Year ",i," (N=",as.numeric(pooledCohortTable2[3,i+1]),")",sep="")}
      if (nYear == 1){
        if (study=="YOY") {
          finalROITableHeaders[i+1] = paste0("YoY (N=",attritionNCount[i],")")
        } else {finalROITableHeaders[i+1] = paste("Year ",i," (N=",attritionNCount[i],")",sep="")}}
      roiTableColumnMatch = which(colnames(PMPM_changes) == paste("Year",i,"Change"))
      if (has_rx == T) {
        finalROITable[1,1+i] = paste(PMPM_changes[1,roiTableColumnMatch+1],"\n($",PMPM_changes[1,roiTableColumnMatch]," PMPM medical savings)",sep="")
        finalROITable[2,1+i] = paste(PMPM_changes[2,roiTableColumnMatch+1],"\n($",PMPM_changes[2,roiTableColumnMatch]," PMPM medical savings)",sep="")
        finalROITable[3,1+i] = paste(PMPM_changes[3,roiTableColumnMatch+1],"\n($",PMPM_changes[3,roiTableColumnMatch]," PMPM medical savings)",sep="")
        ROI = as.numeric(PMPM_changes[1,roiTableColumnMatch]) / (as.numeric(lvgoPrice)-as.numeric(supplyCost))
        rxROI = (as.numeric(PMPM_changes[1,roiTableColumnMatch]) + as.numeric(PMPM_changes[2,roiTableColumnMatch])) / (as.numeric(lvgoPrice)-as.numeric(supplyCost))
        diabetes_rxROI = (as.numeric(PMPM_changes[1,roiTableColumnMatch]) + as.numeric(PMPM_changes[3,roiTableColumnMatch])) / (as.numeric(lvgoPrice)-as.numeric(supplyCost))
        if (program == "Diabetes") {
          finalROITable[4,1+i] = paste("$",PMPM_changes[1,roiTableColumnMatch],' ÷ ($',lvgoPrice,"-$",round(as.numeric(supplyCost),0),") = ",round(ROI,1),sep="")
          finalROITable[5,1+i] = paste("($",PMPM_changes[1,roiTableColumnMatch],"+","$",PMPM_changes[2,roiTableColumnMatch],') ÷ ($',lvgoPrice,"-$",round(as.numeric(supplyCost),0),") = ",round(rxROI,1),sep="")
          finalROITable[6,1+i] = paste("($",PMPM_changes[1,roiTableColumnMatch],"+","$",PMPM_changes[3,roiTableColumnMatch],') ÷ ($',lvgoPrice,"-$",round(as.numeric(supplyCost),0),") = ",round(diabetes_rxROI,1),sep="")
        } else if (program == "Hypertension") {
          finalROITable[4,1+i] = paste("$",PMPM_changes[1,roiTableColumnMatch],' ÷ $',lvgoPrice," = ",round(ROI,1),sep="")
          finalROITable[5,1+i] = paste("($",PMPM_changes[1,roiTableColumnMatch],"+","$",PMPM_changes[2,roiTableColumnMatch],') ÷ $',lvgoPrice," = ",round(rxROI,1),sep="")
          finalROITable[6,1+i] = paste("($",PMPM_changes[1,roiTableColumnMatch],"+","$",PMPM_changes[3,roiTableColumnMatch],') ÷ $',lvgoPrice," = ",round(diabetes_rxROI,1),sep="")
        }
      } else {
        finalROITable[1,1+i] = paste(PMPM_changes[1,roiTableColumnMatch+1],"\n($",PMPM_changes[1,roiTableColumnMatch]," PMPM medical savings)",sep="")
        ROI = as.numeric(PMPM_changes[1,roiTableColumnMatch]) / (as.numeric(lvgoPrice)-as.numeric(supplyCost))
        if (program == "Diabetes") {
          finalROITable[2,1+i] = paste("$",PMPM_changes[1,roiTableColumnMatch],' ÷ ($',lvgoPrice,"-$",round(as.numeric(supplyCost),0),") = ",round(ROI,1),sep="")
        } else if (program == "Hypertension") {
          finalROITable[2,1+i] = paste("$",PMPM_changes[1,roiTableColumnMatch],' ÷ $',lvgoPrice," = ",round(ROI,1),sep="")
        }
      }
      if (has_rx) {
        executiveSummaryRoiArray[i] = round(rxROI,1)
        rxRoiArray[i] = round(rxROI,1); noRxRoiArray[i] = round(ROI,1); diabetesRxRoiArray[i] = round(diabetes_rxROI,1)
      } else {
        executiveSummaryRoiArray[i] = round(ROI,1)
      }
    }
    colnames(finalROITable) = finalROITableHeaders
  }

  # Now, turn finalROITable into a flextable
  {
    roiFlextable <- flextable::flextable(finalROITable)
    roiFlextable <- flextable::bold(roiFlextable,j=1,bold=TRUE)
    roiFlextable <- flextable::bold(roiFlextable,bold=TRUE,part="header")
    roiFlextable <- flextable::width(roiFlextable,width=(5/(nYear+1)))
    roiFlextable <- flextable::bg(roiFlextable, bg="#66478F",part="header")
    roiFlextable <- flextable::color(roiFlextable,color="white",part="header")
    roiFlextable <- flextable::border_outer(roiFlextable,border=officer::fp_border(color="black",style="solid",width=1.5))
    roiFlextable <- flextable::border_inner(roiFlextable,border=officer::fp_border(color="black",style="solid",width=1))
    roiFlextable <- flextable::fontsize(roiFlextable,size=9,part="all")
    roiFlextable <- flextable::font(roiFlextable,fontname = "century gothic",part="all")
  }
  returnList = list(roiFlextable,executiveSummaryRoiArray,noRxRoiArray,rxRoiArray,diabetesRxRoiArray,ROI,finalROITable,roiFlextable)
  names(returnList) = c("roiFlextable","executiveSummaryRoiArray","noRxRoiArray","rxRoiArray","diabetesRxRoiArray","ROI","finalROITable","roiFlextable")
  return(returnList)
}
