# This function creates the difference-in-difference flextable used in the ROI
# results slide.

make_did_table <- function(nYear, spendSummaryTable,pooledPharSpendingTable,year0, year1) {
  if (nYear == 1) {
    DiDTable = as.data.frame(t(c("Medical",
                                 spendSummaryTable[1,2],spendSummaryTable[1,3],as.numeric(spendSummaryTable[1,2])-as.numeric(spendSummaryTable[1,3]), #Non-member med costs & difference
                                 spendSummaryTable[1,5],spendSummaryTable[1,6],as.numeric(spendSummaryTable[1,5])-as.numeric(spendSummaryTable[1,6]), # Member med costs & difference
                                 (as.numeric(spendSummaryTable[1,5])-as.numeric(spendSummaryTable[1,6]))-(as.numeric(spendSummaryTable[1,2])-as.numeric(spendSummaryTable[1,3]))))) # DiD
    if (has_rx) {
      DiDTable = rbind(DiDTable,t(c("Pharmaceutical",
                                    round(as.numeric(pooledPharSpendingTable[1,2])),round(as.numeric(pooledPharSpendingTable[1,3])),round(as.numeric(pooledPharSpendingTable[1,2])-as.numeric(pooledPharSpendingTable[1,3])),
                                    round(as.numeric(pooledPharSpendingTable[1,5])),round(as.numeric(pooledPharSpendingTable[1,6])),round(as.numeric(pooledPharSpendingTable[1,5])-as.numeric(pooledPharSpendingTable[1,6])),
                                    round((as.numeric(pooledPharSpendingTable[1,5])-as.numeric(pooledPharSpendingTable[1,6]))-(as.numeric(pooledPharSpendingTable[1,2])-as.numeric(pooledPharSpendingTable[1,3]))))))
    }
    for (i in 2:8) {
      for (j in 1:nrow(DiDTable)) DiDTable[j,i] = paste0("$",DiDTable[j,i]); DiDTable[j,i] = gsub("\\$-","-\\$",DiDTable[j,i])
    }
    colnames(DiDTable) = c("Total allowed costs",year0,year1,"Difference",paste0(" ",year0),paste0(" ",year1)," Difference","Savings")
    DiDFT = flextable::flextable(DiDTable)
    DiDFT <- flextable::add_header_row(DiDFT,values = c(" ","Non-member","Member","DID"), colwidths = c(1,3,3,1))
    DiDFT <- flextable::bg(DiDFT,bg="#66478F",i=1,j=2:8,part="header")
    DiDFT <- flextable::bg(DiDFT,bg="#696b71",i=2,j=2:8,part="header")
    DiDFT <- flextable::color(DiDFT,color="white",j=2:8, part="header")
    DiDFT <- flextable::align(DiDFT,align="center",part="all")
    DiDFT <- flextable::bold(DiDFT,j=1,part="all")
    DiDFT <- flextable::bold(DiDFT,part="header")
    DiDFT <- flextable::font(DiDFT,fontname="Century Gothic",part="all")
    DiDFT <- flextable::height(DiDFT,i=2,height=1,part="header",unit="in")
    DiDFT <- flextable::border_remove(DiDFT)
    DiDFT <- flextable::surround(DiDFT,j=2:8,border=officer::fp_border(color="black",style="solid",width=1),part="header")
    DiDFT <- flextable::border_inner(DiDFT,border=officer::fp_border(color="black",style="solid",width=1),part="body")
    DiDFT <- flextable::surround(DiDFT,i=2,j=1,border=officer::fp_border(color="black",style="solid",width=2),part="header")
    DiDFT <- flextable::surround(DiDFT,i=2,j=1,border.right=officer::fp_border(color="black",style="solid",width=1),part="header")
    DiDFT <- flextable::border_outer(DiDFT,border=officer::fp_border(color="black",style="solid",width=2),part="body")
    DiDFT <- flextable::bg(DiDFT,bg="#e6e7e8",i=2,j=1,part="header")
    DiDFT <- flextable::bg(DiDFT,bg="#c6efcd",part="body",j=8)
    DiDFT <- flextable::width(DiDFT,j=1,width=2,unit="in")
    DiDFT <- flextable::fontsize(DiDFT,size=9,part="all")
    DiDFT
  }
  if (nYear != 1) {DiDFT = NULL}

  return(DiDFT)
}
