# This function extracts the year-over-year PPPM difference in differences,
# for both medical costs and pharmacy costs, if they exist.

extract_pppm_changes <- function(has_rx, nYear, roiSheet,spend_summary_table, program, pharmacyCostsSheet,spend_summary_table_column_names, pooledPharSpendingTable) {
  if (has_rx) {
    pppm_changes = as.data.frame(matrix(nrow=3,ncol=nYear*2))
    row.names(pppm_changes) = c("Net Medical Costs","Net Pharmacy Costs","Net Diabetes Rx Costs")
  } else {
    pppm_changes = as.data.frame(matrix(nrow=1,ncol=nYear*2))
    row.names(pppm_changes) = c("Net Medical Costs")
  }
  forIndex = 1
  for (i in 1:nYear){colnames(pppm_changes)[forIndex]=paste("Year",i,"Change");colnames(pppm_changes)[forIndex+1]=paste("Year",i,"% Change");forIndex=forIndex+2}
  rm(forIndex)

  for (i in 1:nYear) {
    # Total Costs
    NetChangePt1 = as.numeric(spend_summary_table[1,(2+i)])-as.numeric(spend_summary_table[1,2])
    NetChangePt2 = as.numeric(spend_summary_table[1,(2*nYear)+3+i])-as.numeric(spend_summary_table[1,(2*nYear)+3])
    if (program == "Hypertension" && !(study %in% c("YOY","1YR"))) {
      #if (program == "Hypertension" && "HTN_population" != "All" && study %in% c("YOY","1YR")) {
      pppm_changes[1,(2*i)-1] = round(NetChangePt2-NetChangePt1,0)
    } else {
      pppm_changes[1,(2*i)-1] = round(NetChangePt1-NetChangePt2,0)
    }
    if (typeof(spend_summary_table[1,ncol(spend_summary_table)-2*nYear+i]) == "character") {
      pppm_changes[1,(2*i)] = spend_summary_table[1,ncol(spend_summary_table)-2*nYear+i]
    } else { pppm_changes[1,(2*i)] = percent(as.numeric(spend_summary_table[1,ncol(spend_summary_table)-2*nYear+i]))}

    # Total Pharmacy Costs
    if (has_rx == T) {
      if (nYear == 1) { # a 1-year or YOY allows the pharmacy data to be extracted directly from the "Pharmacy costs" Sheet.
        NetPharChangeNonMemb = as.numeric(pharmacyCostsSheet[4,4])-as.numeric(pharmacyCostsSheet[4,3])
        NetPharChangeMemb = as.numeric(pharmacyCostsSheet[4,7])-as.numeric(pharmacyCostsSheet[4,6])
        DiaHTNPharChangeNonMemb = as.numeric(pharmacyCostsSheet[5,4])-as.numeric(pharmacyCostsSheet[5,3])
        DiaHTNPharChangeMemb = as.numeric(pharmacyCostsSheet[5,7])-as.numeric(pharmacyCostsSheet[5,6])
        pppm_changes[2,1] = round(NetPharChangeNonMemb-NetPharChangeMemb,0)
        pppm_changes[2,2] = percent(as.numeric(pharmacyCostsSheet[4,9]))
        pppm_changes[3,1] = round(DiaHTNPharChangeNonMemb-DiaHTNPharChangeMemb,0)
        pppm_changes[3,2] = percent(as.numeric(pharmacyCostsSheet[5,9]))
      }
      if (nYear != 1) { # Multi-year pooled Pharmacy costs take more effort to extract from the ROI sheet.

        # Extract the net $ pppm change for total phar costs and diabetic phar costs
        NetPharChangeMemb = as.numeric(pooledPharSpendingTable[1,(2*nYear)+3])-as.numeric(pooledPharSpendingTable[1,(2*nYear)+3+i])
        NetPharChangeNonMemb = as.numeric(pooledPharSpendingTable[1,2])-as.numeric(pooledPharSpendingTable[1,(2+i)])
        pppm_changes[2,(2*i)-1] = round(NetPharChangeMemb-NetPharChangeNonMemb,0)
        DiaHTNPharChangeMemb = as.numeric(pooledPharSpendingTable[2,(2*nYear)+3])-as.numeric(pooledPharSpendingTable[2,(2*nYear)+3+i])
        DiaHTNPharChangeNonMemb = as.numeric(pooledPharSpendingTable[2,2])-as.numeric(pooledPharSpendingTable[2,(2+i)])
        pppm_changes[3,(2*i)-1] = round(DiaHTNPharChangeMemb-DiaHTNPharChangeNonMemb,0)

        # Extract the % DID for total phar costs.
        tryCatch({
          pppm_changes[2,(2*i)] = percent(as.numeric(pooledPharSpendingTable[2,ncol(pooledPharSpendingTable)-2*nYear+i]))
        },error=function(e){
          pppm_changes[2,(2*i)] = pooledPharSpendingTable[2,ncol(pooledPharSpendingTable)-2*nYear+i]
        })

        # Extract the % DID for diabetic phar costs.
        tryCatch({
          pppm_changes[3,(2*i)] = percent(as.numeric(pooledPharSpendingTable[2,ncol(pooledPharSpendingTable)-2*nYear+i]))
        },error=function(e){
          pppm_changes[3,(2*i)] = pooledPharSpendingTable[2,ncol(pooledPharSpendingTable)-2*nYear+i]
        })
      }
    }
  }
  return(pppm_changes)
}
