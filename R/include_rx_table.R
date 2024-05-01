# This function will modify the spend summary table to include pharmaceutical
# data. It essentially tacks on pharmaceutical data to the bottom of the
# already-existing spend summary table. It does NOT check for existence
# of rx data; it already assumes the data is there.

include_rx_table <- function(pooledPharSpendingTable, spendSummaryTable) {
  for (i in c(1,2)) {
    for (j in 2:ncol(pooledPharSpendingTable)) {
      if (abs(as.numeric(pooledPharSpendingTable[i,j])) < 1) {
        pooledPharSpendingTable[i,j] = percent(as.numeric(pooledPharSpendingTable[i,j]))
      } else {pooledPharSpendingTable[i,j] = round(as.numeric(pooledPharSpendingTable[i,j]),0)}
      pooledPharSpendingTable[i,j] = gsub("\\$-","-$",pooledPharSpendingTable[i,j])
    }
  }
  pooledPharSpendingTable[,1] = c("Total pharmaceutical costs","Diabetes-related pharmaceutical costs")
  names(pooledPharSpendingTable) = names(spendSummaryTable)
  spendSummaryWithPhar = rbind(spendSummaryTable,pooledPharSpendingTable)
  for (k in 2:ncol(spendSummaryWithPhar)) {
    if (!is.na(as.numeric(spendSummaryWithPhar[2,k]))) {
      spendSummaryWithPhar[,k] = paste("$",spendSummaryWithPhar[,k],sep="")
      spendSummaryWithPhar[,k] = gsub("\\$-","-$",spendSummaryWithPhar[,k])
    }
  }
  spendSummaryWithPharFT <- flextable::flextable(spendSummaryWithPhar)
  spendSummaryWithPharFT <- flextable::add_header_row(spendSummaryWithPharFT,values = c(" ","Non-member","Member"," "), colwidths = c(1,2*nYear+1,2*nYear+1,nYear*2))
  spendSummaryWithPharFT <- flextable::bg(spendSummaryWithPharFT,bg="#66478F",part="header")
  spendSummaryWithPharFT <- flextable::color(spendSummaryWithPharFT,color="white", part="header")
  spendSummaryWithPharFT <- flextable::color(spendSummaryWithPharFT,color="white", part="body",j=1)
  for (i1 in 1:nrow(spendSummaryWithPhar)) {
    for (j1 in (ncol(spendSummaryWithPhar)-2*nYear+1):ncol(spendSummaryWithPhar)) {
      if (spendSummaryWithPhar[i1,j1] > 0) {
        spendSummaryWithPharFT <- flextable::bg(spendSummaryWithPharFT, i=i1, j=j1, bg="#E1DDE5",part="body")
      } else {
        spendSummaryWithPharFT <- flextable::bg(spendSummaryWithPharFT, i=i1, j=j1, bg="#c6efcd",part="body")
      }
    }
  }
  spendSummaryWithPharFT <- flextable::bg(spendSummaryWithPharFT,j=1,bg = "#55437d", part="body")
  spendSummaryWithPharFT <- flextable::align(spendSummaryWithPharFT,align=c("center"),part="all")
  spendSummaryWithPharFT <- flextable::width(spendSummaryWithPharFT,j=1,width=1.5)
  spendSummaryWithPharFT <- flextable::width(spendSummaryWithPharFT,j=2:ncol(pooledPharSpendingTable),width=0.5)
  spendSummaryWithPharFT <- flextable::fontsize(spendSummaryWithPharFT,size=8,part="all")
  spendSummaryWithPharFT <- flextable::font(spendSummaryWithPharFT,fontname="century gothic",part="all")
  spendSummaryWithPharFT <- flextable::border_inner(spendSummaryWithPharFT,border=officer::fp_border(color="black",style="solid",width=1),part="all")
  spendSummaryWithPharFT <- flextable::border_outer(spendSummaryWithPharFT,border=officer::fp_border(color="black",style="solid",width=2),part = "body")
  spendSummaryWithPharFT <- flextable::border_outer(spendSummaryWithPharFT,border=officer::fp_border(color="black",style="solid",width=2),part = "all")
  spendSummaryWithPharFT <- flextable::hline(spendSummaryWithPharFT,i=nrow(spendSummaryWithPhar)-2,border=officer::fp_border(color="black",style="solid",width=3))

  return(spendSummaryWithPharFT)
}
