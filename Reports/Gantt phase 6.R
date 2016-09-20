install.packages("plotrix")

library(plotrix)

labels<-    c("Update OM meta database","Link repositories (M3 v1.0)","Complete SDP","Finalize data for OM","Finalize Trial Spec.","Establish prelim. Perf. Metrc.","Incorporate alternative MPs", "Demonstration MSE 1","Demonstration MSE 2","Report/paper writing")
starts<-   c("2016/7/26",              "2016/8/1",                    "2016/8/1",    "2016/7/26",          "2016/7/26",           "2016/10/1",                     "2016/10/1",                     "2016/8/1",          "2016/12/01",       "2016/12/01")
starts<-as.Date(starts, format = "%Y/%m/%d")
Duration<-c(67,                       31,                             61,           37,                    37,                    60,                              92,                              31,                  65,                 65)
ends<-starts+Duration
priorities<-c(1,                       3,                              3,            1,                     1,                     2,                               4,                              3,                   4,                  4)

gantt.info<-list(labels=labels,starts=as.POSIXct(starts, format=Ymd.format),ends=as.POSIXct(ends, format=Ymd.format),priorities=priorities)
months <- seq(as.Date("2016/07/01", "%Y/%m/%d"), by="month", length.out=8)
monthslab <- format(months, format="%b")

vgridpos<-as.POSIXct(months,format=Ymd.format)
vgridlab<-monthslab

colfunc <- colorRampPalette(c("red", "darkgoldenrod1"))

timeframe <- as.POSIXct(as.Date(c("2016/07/01","2017/03/1")),format=Ymd.format)


gantt.chart(gantt.info, taskcolors=colfunc(4),xlim=timeframe, main="ABT-MSE Technical Assistant Phase 6",
            priority.legend=TRUE,vgridpos=vgridpos,vgridlab=vgridlab,hgrid=TRUE)

abline(v=as.POSIXct(as.Date(c("2016/07/20","2016/09/23","2017/02/12","2017/02/18")),format=Ymd.format),lty=2,lwd=2,col="#25ff0055")
