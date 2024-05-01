# This function creates a table of pmpm changes and, if has_rx, a pooled rx spending table. Both are used extensively
# in later functions for more flextable generation.

get_pmpm_changes_and_pooled_phar <- function(has_rx, nYear, roiSheet,spend_summary_table, program, pharmacyCostsSheet,spend_summary_table_column_names) {
  if (has_rx) {
    PMPM_changes = as.data.frame(matrix(nrow=3,ncol=nYear*2))
    row.names(PMPM_changes) = c("Net Medical Costs","Net Pharmacy Costs","Net Diabetes Rx Costs")
  } else {
    PMPM_changes = as.data.frame(matrix(nrow=1,ncol=nYear*2))
    row.names(PMPM_changes) = c("Net Medical Costs")
  }
  forIndex = 1
  for (i in 1:nYear) {
    colnames(PMPM_changes)[forIndex]=paste("Year",i,"Change")
    colnames(PMPM_changes)[forIndex+1]=paste("Year",i,"% Change")
    forIndex=forIndex+2
  }

  # Extract the pharmacy costs, if applicable.
  if (has_rx) {
    if (nYear > 1) {
      pharCostsRowIndex = which(roiSheet[,1] == "Pharmacy costs")+3
      pharCostsColumnIndex = which(roiSheet[pharCostsRowIndex-3,] == "Combined Result")
      pooledPharSpendingTable = roiSheet[pharCostsRowIndex:(pharCostsRowIndex+2),pharCostsColumnIndex:(pharCostsColumnIndex+(7+5*(nYear-1)))]

      # Sometimes, the file read-in is not correct, and is offset by 1 row. Check for that and fix below.
      if(!is.na(pooledPharSpendingTable[1,1])) {
        if (pooledPharSpendingTable[1,1] == "Total costs") {
          pharCostsRowIndex = pharCostsRowIndex-1
          pooledPharSpendingTable = roiSheet[pharCostsRowIndex:(pharCostsRowIndex+2),pharCostsColumnIndex:(pharCostsColumnIndex+(7+5*(nYear-1)))]
        }
      }
      for (k in (ncol(pooledPharSpendingTable)-nYear+1):ncol(pooledPharSpendingTable)) {
        pooledPharSpendingTable[1,k] = paste("DID Y",abs(k-ncol(pooledPharSpendingTable)+nYear)," vs. Y0",sep="")
      }
      colnames(pooledPharSpendingTable) = pooledPharSpendingTable[1,]
      colnames(pooledPharSpendingTable)[1] = " "
      if (pooledPharSpendingTable[2,1]=="Total costs") {
        pooledPharSpendingTable = pooledPharSpendingTable[2:3,]
      }
    } else {
      pooledPharSpendingTable = pharmacyCostsSheet[min(which(pharmacyCostsSheet[,2] == "Total costs")):(min(which(pharmacyCostsSheet[,2] == "Total costs"))+1),2:ncol(pharmacyCostsSheet)]
      colnames(pooledPharSpendingTable) = spend_summary_table_column_names
    }

    # Add a space to any duplicate column names
    for (l in 2:ncol(pooledPharSpendingTable)) {
      m = l+1
      while (m < ncol(pooledPharSpendingTable)) {
        if (colnames(pooledPharSpendingTable)[l] == colnames(pooledPharSpendingTable)[m]) {
          colnames(pooledPharSpendingTable)[l] = paste(colnames(pooledPharSpendingTable)[l]," ",sep="")
        }
        m=m+1
      }
    }

    # Add DID amount columns to the end of the pharmacy summary spending table
    {
      startingColumnCount = ncol(pooledPharSpendingTable)
      for (i in 1:nYear) {
        pooledPharSpendingTable[,startingColumnCount+i] = (as.numeric(pooledPharSpendingTable[,3+nYear*2+i])-as.numeric(pooledPharSpendingTable[,3+nYear*2]))-(as.numeric(pooledPharSpendingTable[,2+i])-as.numeric(pooledPharSpendingTable[,2]))
        colnames(pooledPharSpendingTable)[startingColumnCount+i] = colnames(spend_summary_table)[startingColumnCount+i]
      }
    }
  } else { pooledPharSpendingTable = NULL}


  for (i in 1:nYear) {
    # Total Costs
    NetChange1 = as.numeric(spend_summary_table[1,(2+i)])-as.numeric(spend_summary_table[1,2])
    NetChange2 = as.numeric(spend_summary_table[1,(2*nYear)+3+i])-as.numeric(spend_summary_table[1,(2*nYear)+3])
    if (program == "Hypertension" && !(study %in% c("YOY","1YR"))) {
      #if (program == "Hypertension" && "HTN_population" != "All" && study %in% c("YOY","1YR")) {
      PMPM_changes[1,(2*i)-1] = round(NetChange2-NetChange1,0)
    } else {
      PMPM_changes[1,(2*i)-1] = round(NetChange1-NetChange2,0)
    }
    if (typeof(spend_summary_table[1,ncol(spend_summary_table)-2*nYear+i]) == "character") {
      PMPM_changes[1,(2*i)] = spend_summary_table[1,ncol(spend_summary_table)-2*nYear+i]
    } else { PMPM_changes[1,(2*i)] = percent(as.numeric(spend_summary_table[1,ncol(spend_summary_table)-2*nYear+i]))}

    # Total Pharmacy Costs
    if (has_rx == T) {
      if (nYear == 1) { # a 1-year or YOY allows the pharmacy data to be extracted directly from the "Pharmacy costs" Sheet.
        NetPharChangeNonMemb = as.numeric(pharmacyCostsSheet[4,4])-as.numeric(pharmacyCostsSheet[4,3])
        NetPharChangeMemb = as.numeric(pharmacyCostsSheet[4,7])-as.numeric(pharmacyCostsSheet[4,6])
        DiaHTNPharChangeNonMemb = as.numeric(pharmacyCostsSheet[5,4])-as.numeric(pharmacyCostsSheet[5,3])
        DiaHTNPharChangeMemb = as.numeric(pharmacyCostsSheet[5,7])-as.numeric(pharmacyCostsSheet[5,6])
        PMPM_changes[2,1] = round(NetPharChangeNonMemb-NetPharChangeMemb,0)
        PMPM_changes[2,2] = percent(as.numeric(pharmacyCostsSheet[4,9]))
        PMPM_changes[3,1] = round(DiaHTNPharChangeNonMemb-DiaHTNPharChangeMemb,0)
        PMPM_changes[3,2] = percent(as.numeric(pharmacyCostsSheet[5,9]))
      }
      if (nYear != 1) { # Multi-year pooled Pharmacy costs take more effort to extract from the ROI sheet.

        # Extract the net $ PMPM change for total phar costs and diabetic phar costs
        NetPharChangeMemb = as.numeric(pooledPharSpendingTable[1,(2*nYear)+3])-as.numeric(pooledPharSpendingTable[1,(2*nYear)+3+i])
        NetPharChangeNonMemb = as.numeric(pooledPharSpendingTable[1,2])-as.numeric(pooledPharSpendingTable[1,(2+i)])
        PMPM_changes[2,(2*i)-1] = round(NetPharChangeMemb-NetPharChangeNonMemb,0)
        DiaHTNPharChangeMemb = as.numeric(pooledPharSpendingTable[2,(2*nYear)+3])-as.numeric(pooledPharSpendingTable[2,(2*nYear)+3+i])
        DiaHTNPharChangeNonMemb = as.numeric(pooledPharSpendingTable[2,2])-as.numeric(pooledPharSpendingTable[2,(2+i)])
        PMPM_changes[3,(2*i)-1] = round(DiaHTNPharChangeMemb-DiaHTNPharChangeNonMemb,0)

        # Extract the % DID for total phar costs.
        tryCatch({
          PMPM_changes[2,(2*i)] = percent(as.numeric(pooledPharSpendingTable[2,ncol(pooledPharSpendingTable)-2*nYear+i]))
        },error=function(e){
          PMPM_changes[2,(2*i)] = pooledPharSpendingTable[2,ncol(pooledPharSpendingTable)-2*nYear+i]
        })

        # Extract the % DID for diabetic phar costs.
        tryCatch({
          PMPM_changes[3,(2*i)] = percent(as.numeric(pooledPharSpendingTable[2,ncol(pooledPharSpendingTable)-2*nYear+i]))
        },error=function(e){
          PMPM_changes[3,(2*i)] = pooledPharSpendingTable[2,ncol(pooledPharSpendingTable)-2*nYear+i]
        })
      }
    }
  }
  returnList = list(PMPM_changes,pooledPharSpendingTable);
  names(returnList) = c("PMPM_changes","pooled_phar_spending_table")
  return(returnList)
}
