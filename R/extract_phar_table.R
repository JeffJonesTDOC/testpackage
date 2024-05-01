# This function extracts the pharmacy claims spending table from the file. It
# also assigns appropriate column names to the table and calculates the
# difference-in-difference dollar amounts and appends them to the table.

extract_phar_table <- function(has_rx, nYear, roiSheet, pharmacyCostsSheet, spend_summary_table_column_names, spend_summary_table = spend_summary$table) {
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
  } else {pooledPharSpendingTable = NULL}
  return(pooledPharSpendingTable)
}
