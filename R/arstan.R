ARSTAN <-
function (){
  n.sink<-sink.number();  while(n.sink>0) {sink(); n.sink=n.sink-1}
  f.apply (FUN=arstan)
}


arstan <-
function (a=NULL){
if(is.null(a)) {
a<-tk_choose.files(multi=FALSE)
if(length(a)!=1) return(tk_messageBox(type="ok", "You should select at least one file!", caption="Problems")->a)
}

file.name<-(getFileName(a))
folder.work<-substr(a, 1,nchar(a)- nchar(basename(a)))
Output<-paste(folder.work,  sep="\\")
outputFolder<-paste(file.name,  sep="\\")
output=paste(file.name, "out", sep=".")
setwd(folder.work)
dir.create(file.name, showWarnings = FALSE)
setwd(file.name)
sink(output, append=FALSE)
Detrender.Options(Stc=stc)
sink()
sink(output, append=TRUE)

cat("Input: ", a, "\nOutput: ", Output, outputFolder, "\\", output, "\n",sep ="")

#rwl<-readRwl(a,n.header=n.lines.header, info=FALSE)      #read the file [i]
rwl<-readRwl(a,n.header=NULL, info=FALSE)      #read the file [i]
TreeIds(rwl, stc=stc)                                                           # Give the number of trees and the no of cores per tree

TrwLessThan(rwl, TRW=0)                                                         # MISSING RINGS
RwlInfo(rwl, print=TRUE)                                                        # Rwl information

if (makeSegPlot){                                                               # Make graph with series length
jpeg(paste(file.name, "SegPlot.jpg", sep="-"), width = 700, height = 700, quality = 85 )
segPlot(rwl, nS=45,  main=file.name)                                           #main -> graph title #
dev.off()}

if (remove.shorter.series){                                                     # Delete series shorter than
rwl = DeleteSeriesShorterThan(rwl, filename=file.name, YEAR=delete.shorter.series)
}

# Plot RAW RWL
jpeg(paste(file.name, "RAW.jpg", sep="-"), width=900, height=700, quality=85)   #graph to JPEG file#
plotRwl(rwl,file.name=file.name, save.csv=T)                                   #main -> graph title #
dev.off()

################################################################################
#  DETRENDING  #################################################################
################################################################################
if (makeFirstDetrending) {
cat("\nDETRENDING\n\nFirst detrending [",first.detrending.method, "]\n", sep="")

detrend1= apply(rwl, 2, RemoveTrend, method=method1, BandwidthPerc=nPerc1, Bandwidth = n1, P=p1)
if (save.detrend1) saveDetrendJPG (rwl, detrend1, folderName = "FirstDetrend", detrend.method=first.detrending.method)########

if (interactive.detrend) {InteractiveDetrending (rwl, detrend1, folderName = "FirstDetrend", method=method1,  n=n1, nPerc=nPerc1, p=p1)
if (.get("DETRENDING_INTERACTIVE_FLAG")) {detrend1=.get("DETRENDING_INTERACTIVE_OUTPUT")}
else {if (save.detrend1) saveDetrendJPG (rwl, detrend1, folderName = "FirstDetrend", detrend.method=first.detrending.method)}
}

if(min(detrend1, na.rm=TRUE)<0) {
cat("\n")
TrwLessThan(detrend1, TRW=0.01) }

rw1=rwl/detrend1
write.rwl(detrend1, fname= paste(file.name, "cv1", sep="."),long.names=TRUE)
write.rwl(rw1, fname= paste(file.name, "in1", sep="."),long.names=TRUE)

#if (save.detrend1) saveDetrendJPG (rwl, detrend1, folderName = "FirstDetrend", detrend.method=first.detrending.method)
RwlInfo(rw1, print=TRUE)

if (makeSecondDetrending) {
cat("\nSecond detrending [",second.detrending.method, "]\n", sep="")
detrend2= apply(rw1, 2, RemoveTrend, method=method2, BandwidthPerc=nPerc2 , Bandwidth = n2, P=p2)

if (save.detrend2) saveDetrendJPG (rw1, detrend2, folderName = "SecondDetrend", detrend.method=second.detrending.method)########

if (interactive.detrend) {InteractiveDetrending (rw1, detrend2, folderName = "SecondDetrend", method=method2,  n=n2, nPerc=nPerc2, p=p1)
if (.get("DETRENDING_INTERACTIVE_FLAG")) {detrend2=.get("DETRENDING_INTERACTIVE_OUTPUT")}
else {if (save.detrend2) saveDetrendJPG (rw1, detrend2, folderName = "SecondDetrend", detrend.method=second.detrending.method)}
#rm(DETRENDING_INTERACTIVE_OUTPUT, envir = .GlobalEnv )
#rm(DETRENDING_INTERACTIVE_FLAG, envir = .GlobalEnv )
#rm(DETRENDING_INTERACTIVE_OUTPUT_CHANGE, envir = .GlobalEnv )
}

if(min(detrend2, na.rm=TRUE)<0) {cat("\n")
 TrwLessThan(detrend2, TRW=0.01)
 }
rw2=rw1/detrend2

        #if (save.detrend2)  saveDetrendJPG (rw1, detrend2, folderName = "SecondDetrend", detrend.method=second.detrending.method)
            write.rwl(detrend2, fname= paste(file.name, "cv2", sep="."),long.names=TRUE)
            write.rwl(rw2, fname= paste(file.name, "in2", sep="."),long.names=TRUE)
            RwlInfo(rw2, print=TRUE) }
        else {
             rw2<-rw1 }
        }
else { rw2<-rwl }

# Save standard chronology
crnStd <- Chron(rw2, prefix = paste(file.name, "STD", sep="-"), biweight = .get("biweightMean"), prewhiten = FALSE,
                  stc=stc)
PrintPlotChrono(crnStd, rwl=rw2, file.name = file.name, crono.type="STD" )

if (run.win.analysis){cat("\nRunning analysis of detrended series\n\n")
runWinRw2<-Run.Win(rw2, winLength=winLength, stc=stc, step = stepWin)
for (i in c(0.75, 0.80, 0.85, 0.90)) {EPS.resume(runWinRw2, EPS=i)}
}

if (make.common.EPS){EPS.common.interval(rw2, stc=stc, out=FALSE)}
if (make.select.period.EPS) { if (!(is.null(first.year.common)||is.null(last.year.common))){
EPS.common.interval(rw2, first.year.common=first.year.common,
                    last.year.common=last.year.common, stc=stc, out=FALSE) } }

if (makeAr){
            ArFunction(rw2, order.max = arMAX)                                  #AUTOREGRESSIVE MODELING
            res = apply(rw2, 2, ar.func, order.max = arMAX)
            crnRes <- Chron(res, prefix = paste(file.name, "RES", sep="-"), biweight = .get("biweightMean"), prewhiten = FALSE, stc=stc)
            PrintPlotChrono(crnRes, rwl=res, file.name = file.name, crono.type="RES" )
            cat("\nResidual series\n")
            RwlInfo(res, print=TRUE)
            write.rwl(res, fname= paste(file.name, "res", sep="."),long.names=TRUE)
  if (run.win.analysis){cat("\nRunning analysis of residual series\n\n")
            runWinRes<-Run.Win(res, winLength=winLength, stc=stc, step = stepWin)
            for (i in c(0.75, 0.80, 0.85, 0.90)) {EPS.resume(runWinRes, EPS=i)}}
          
if (make.common.EPS){  EPS.common.interval(res, stc=stc, out=FALSE)  }
if (make.select.period.EPS) { if (!(is.null(first.year.common)||is.null(last.year.common))) {
              EPS.common.interval(res, first.year.common=first.year.common,
              last.year.common=last.year.common, stc=stc, out=FALSE) }  }
}
cat("[", date(),"]",sep="")
sink()
}

