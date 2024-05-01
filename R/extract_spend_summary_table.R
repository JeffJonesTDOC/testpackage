extract_spend_summary_table <- function(nYear, roiSheet,combine_ip_op,year0,year1,remove_POS,HTN_population) {
  suppressWarnings({
    if (nYear > 2) {
      tableStringMatch = paste("All",nYear,"years pooled together")
    } else if (nYear == 2) {
      tableStringMatch = "Combined Result"
    } else if (nYear == 1) {
      tableStringMatch = "Medical spending per member per month"
    }

    if (nYear == 2 && program=="Hypertension") {
      roi_table_column_index = 15
    } else {roi_table_column_index = 1}
    while (roi_table_column_index < ncol(roiSheet)) {
      roi_table_row_index = which(roiSheet[,roi_table_column_index]==tableStringMatch)
      if (length(roi_table_row_index) > 1) {roi_table_row_index = roi_table_row_index[1];break}
      else if (length(roi_table_row_index) == 1) {break}
      else {roi_table_column_index = roi_table_column_index+1}
    }
    # Find "Total Costs" and "PCP" within the bounds of roi_table_row/column_index to determine the
    # dimensions of the table.
    {
      total_cost_index = roi_table_row_index
      while (total_cost_index<roi_table_row_index+50) {
        totalCostCheck = roiSheet[total_cost_index,roi_table_column_index]
        if (is.na(totalCostCheck)) {
          total_cost_index=total_cost_index+1
        } else if (totalCostCheck == "Total costs") {
          break
        } else {total_cost_index=total_cost_index+1}
      }

      pcp_index = roi_table_row_index
      while (pcp_index<roi_table_row_index+50) {
        pcpCheck = roiSheet[pcp_index,roi_table_column_index]
        if (is.na(pcpCheck)) {
          pcp_index=pcp_index+1
        } else if (pcpCheck == "PCP") {
          break
        } else {pcp_index=pcp_index+1}
      }
    }

    spendSummaryTable = roiSheet[total_cost_index:pcp_index,roi_table_column_index:(roi_table_column_index+(7+5*(nYear-1)))]
    spendSummaryTable[is.na(spendSummaryTable)] <- 0;


    # Remove POS if remove_POS is true.
    if (remove_POS) {
      er_visits_index = which(spendSummaryTable[,1] == 'ER visits')
      office_visits_index = which(spendSummaryTable[,1] == "Office visits")
      spendSummaryTable = spendSummaryTable[c(1:(er_visits_index-1),(office_visits_index+1):nrow(spendSummaryTable)),]
    }

    # Remove Hypoglycemia, Lab, Ambulance, and PCP.
    spendSummaryTable = spendSummaryTable[-c(which(spendSummaryTable[,1] %in% c("Hypoglycemia-related","Lab","Ambulance","PCP"))),]

    # Create a flextable object, using the spendSummaryTable as reference.
    {

      # Define some variables for use in IP and OP combining, if combine_ip_op true.
      if (combine_ip_op && !remove_POS) {
        combine_ip_op_array = array(dim=ncol(spendSummaryTable))
        ip_index = which(spendSummaryTable[,1] == "Inpatient hospital, non-ER visits")
        op_index = which(spendSummaryTable[,1] == "Outpatient hospital, non-ER visits")
      }

      # Rounding and percentage-conversions. For each column of the table,
      # if the median value is <1 then it is assumed to be a percentage column.
      # Otherwise, it is a $$$ spending column.
      for (i in 2:ncol(spendSummaryTable)) {
        if (median(na.omit(as.numeric(spendSummaryTable[which(spendSummaryTable[,i]!=0),i]))) < 1) {
          spendSummaryTable[,i] = percent(as.numeric(spendSummaryTable[,i]))
        } else {
          spendSummaryTable[,i] = round(as.numeric(spendSummaryTable[,i]),0)
          # If combine_ip_op true, then sum together into a new array to be attached to spendSummaryTable.
          # Percent changes and DID are calculated further below.
          if (combine_ip_op && !remove_POS) {
            combine_ip_op_array[i] = sum(spendSummaryTable[ip_index:op_index,i])
          }
        }
      }

      # Calculate new percent changes and did for combined ip and op, then attach to spendSummaryTable
      if (combine_ip_op && !remove_POS) {
        if (study == "YOY" | study == "1YR"){
          combine_ip_op_array[4] = (combine_ip_op_array[3]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[7] = (combine_ip_op_array[6]-combine_ip_op_array[5])/combine_ip_op_array[5]
          combine_ip_op_array[8] = combine_ip_op_array[7]-combine_ip_op_array[4]
        }
        if (study == "2YR") {
          combine_ip_op_array[5] = (combine_ip_op_array[3]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[6] = (combine_ip_op_array[4]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[10] = (combine_ip_op_array[8]-combine_ip_op_array[7])/combine_ip_op_array[7]
          combine_ip_op_array[11] = (combine_ip_op_array[9]-combine_ip_op_array[7])/combine_ip_op_array[7]
          combine_ip_op_array[12] = combine_ip_op_array[10]-combine_ip_op_array[5]
          combine_ip_op_array[13] = combine_ip_op_array[11]-combine_ip_op_array[6]
        }
        if (study == "3YR") {
          combine_ip_op_array[6] = (combine_ip_op_array[3]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[7] = (combine_ip_op_array[4]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[8] = (combine_ip_op_array[5]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[13] = (combine_ip_op_array[10]-combine_ip_op_array[9])/combine_ip_op_array[9]
          combine_ip_op_array[14] = (combine_ip_op_array[11]-combine_ip_op_array[9])/combine_ip_op_array[9]
          combine_ip_op_array[15] = (combine_ip_op_array[12]-combine_ip_op_array[9])/combine_ip_op_array[9]
          combine_ip_op_array[16] = combine_ip_op_array[13]-combine_ip_op_array[6]
          combine_ip_op_array[17] = combine_ip_op_array[14]-combine_ip_op_array[7]
          combine_ip_op_array[18] = combine_ip_op_array[15]-combine_ip_op_array[8]
        }
        if (study == "4YR") {
          combine_ip_op_array[7] = (combine_ip_op_array[3]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[8] = (combine_ip_op_array[4]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[9] = (combine_ip_op_array[5]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[10] = (combine_ip_op_array[6]-combine_ip_op_array[2])/combine_ip_op_array[2]
          combine_ip_op_array[16] = (combine_ip_op_array[12]-combine_ip_op_array[11])/combine_ip_op_array[11]
          combine_ip_op_array[17] = (combine_ip_op_array[13]-combine_ip_op_array[11])/combine_ip_op_array[11]
          combine_ip_op_array[18] = (combine_ip_op_array[14]-combine_ip_op_array[11])/combine_ip_op_array[11]
          combine_ip_op_array[19] = (combine_ip_op_array[15]-combine_ip_op_array[11])/combine_ip_op_array[11]
          combine_ip_op_array[20] = combine_ip_op_array[16]-combine_ip_op_array[7]
          combine_ip_op_array[21] = combine_ip_op_array[17]-combine_ip_op_array[8]
          combine_ip_op_array[22] = combine_ip_op_array[18]-combine_ip_op_array[9]
          combine_ip_op_array[23] = combine_ip_op_array[19]-combine_ip_op_array[10]
        }
        for (i in 2:ncol(spendSummaryTable)) {if (is.character(spendSummaryTable[1,i])) {combine_ip_op_array[i]=percent(as.numeric(combine_ip_op_array[i]))}}
        combine_ip_op_array[1] = "Hospital Visits, IP and OP"

        spendSummaryTable = rbind(spendSummaryTable[1:(which(spendSummaryTable[,1]=="Inpatient hospital, non-ER visits")-1),],
                                  combine_ip_op_array,
                                  spendSummaryTable[(which(spendSummaryTable[,1]=="Outpatient hospital, non-ER visits")+1):nrow(spendSummaryTable),])
      }


      # Define column names of the table, dynamically since it varies by nYear.
      {
        spendSummaryTableColumnNames = array(dim=(nYear*5)+3)
        if (nYear == 1) {
          spendSummaryTableColumnNames = c("PMPM Costs",year0,year1,paste0("% Diff ",year0," vs ",year1),paste0(" ",year0),paste0(" ",year1),paste0(" % Diff ",year0," vs ",year1),"DID %")
        } else {
          spendSummaryTableColumnNames[(1:2)] = c("PMPM Costs","Y0"); spendSummaryTableColumnNames[3+(nYear*2)] = " Y0"
          for (i in 1:nYear) {
            spendSummaryTableColumnNames[2+i] = paste("Y",i,sep="")
            spendSummaryTableColumnNames[2+(nYear)+i] = paste("% Diff Y",i," vs Y0",sep="")
            spendSummaryTableColumnNames[3+(nYear*2)+i] = paste(" Y",i,sep="")
            spendSummaryTableColumnNames[3+(nYear*3)+i] = paste(" % Diff Y",i," vs Y0",sep="")
            spendSummaryTableColumnNames[3+(nYear*4)+i] = paste("DID Y",i," vs Y0",sep="")
          }
        }
        colnames(spendSummaryTable) = spendSummaryTableColumnNames
      }

      # Add DID amount columns to the end of the summary spending table
      {
        startingColumnCount = ncol(spendSummaryTable)
        for (i in 1:nYear) {
          spendSummaryTable[,startingColumnCount+i] = (as.numeric(spendSummaryTable[,3+nYear*2+i])-as.numeric(spendSummaryTable[,3+nYear*2]))-(as.numeric(spendSummaryTable[,2+i])-as.numeric(spendSummaryTable[,2]))
          if (nYear == 1) {
            colnames(spendSummaryTable)[startingColumnCount+i] = paste0("DID $ ",year1," vs ",year0)
          } else {colnames(spendSummaryTable)[startingColumnCount+i] = paste0("DID $ Y",i," vs Y0")}
        }
      }

      spendSummaryFlexTablePrep = spendSummaryTable

      # Add on the "$" to the costs
      for (k in 2:ncol(spendSummaryFlexTablePrep)) {
        if (!is.na(as.numeric(spendSummaryFlexTablePrep[2,k]))) {
          spendSummaryFlexTablePrep[,k] = paste("$",spendSummaryFlexTablePrep[,k],sep="")
          spendSummaryFlexTablePrep[,k] = gsub("\\$-","-$",spendSummaryFlexTablePrep[,k])
        }
      }

      # Replace any NA values with 0
      spendSummaryTable[is.na(spendSummaryTable)] = 0

      # Generate the flextable and specify properties.
      {
        spendSummaryFlexTable <- flextable::flextable(spendSummaryFlexTablePrep)
        if (program == "Hypertension" && HTN_population != "All" && study %in% c("YOY","1YR")) {
          spendSummaryFlexTable <- flextable::add_header_row(spendSummaryFlexTable,values = c(" ","Member","Non-Member"," "), colwidths = c(1,2*nYear+1,2*nYear+1,nYear*2))
        } else {
          spendSummaryFlexTable <- flextable::add_header_row(spendSummaryFlexTable,values = c(" ","Non-member","Member"," "), colwidths = c(1,2*nYear+1,2*nYear+1,nYear*2))
        }
        spendSummaryFlexTable <- flextable::color(spendSummaryFlexTable,color="white", part="header")
        spendSummaryFlexTable <- flextable::color(spendSummaryFlexTable,j=1,color="white",part="body")
        spendSummaryFlexTable <- flextable::bg(spendSummaryFlexTable,bg = "#66478F", part="header")
        spendSummaryFlexTable <- flextable::bg(spendSummaryFlexTable,j=1,bg = "#55437d", part="body")
        for (i1 in 1:nrow(spendSummaryTable)) {
          for (j1 in (ncol(spendSummaryTable)-2*nYear+1):ncol(spendSummaryTable)) {
            if (spendSummaryTable[i1,j1] > 0) {
              spendSummaryFlexTable <- flextable::bg(spendSummaryFlexTable, i=i1, j=j1, bg="#E1DDE5",part="body")
            } else {
              spendSummaryFlexTable <- flextable::bg(spendSummaryFlexTable, i=i1, j=j1, bg="#c6efcd",part="body")
            }
          }
        }
        spendSummaryFlexTable <- flextable::align(spendSummaryFlexTable,align=c("center"),part="all")
        spendSummaryFlexTable <- flextable::width(spendSummaryFlexTable,j=1,width=1.5)
        spendSummaryFlexTable <- flextable::width(spendSummaryFlexTable,j=2:ncol(spendSummaryTable),width=0.5)
        spendSummaryFlexTable <- flextable::height(spendSummaryFlexTable,height = 0.05,part="body")
        spendSummaryFlexTable <- flextable::fontsize(spendSummaryFlexTable,size=8,part="all")
        spendSummaryFlexTable <- flextable::font(spendSummaryFlexTable,fontname="century gothic",part="all")
        spendSummaryFlexTable <- flextable::border_outer(spendSummaryFlexTable,border=officer::fp_border(color="black",style="solid",width=2),part="all")
        spendSummaryFlexTable <- flextable::border_inner(spendSummaryFlexTable,border=officer::fp_border(color="black",style="solid",width=1),part="all")
      }

    }

  })
  returnList = list(spendSummaryTable,spendSummaryFlexTable,spendSummaryTableColumnNames)
  names(returnList) = c("table","flex_table","column_names")
  return(returnList)
}
