
ar.func <- function(y, order.max = 10) 
{

            y2 = na.omit(y);

            ar1 = ar(y2, order.max = order.max);

            y2 = ar1$resid + ar1$x.mean;

            y[!is.na(y)] = y2;

            return(y);

}

.assign <-
function(x, value){

      assign(x, value, envir = detrenderEnv())

      }

.get <-
function(x){

      get(x, envir = detrenderEnv())

      }


detrenderEnv <-
function ()

{

    pos <- match("detrenderEnv", search())

    if (is.na(pos)) {

        detrenderEnv <- list()

        attach(detrenderEnv, length(search())-1 )

        rm(detrenderEnv)

        pos <- match("detrenderEnv", search())

    }

    return(pos.to.env(pos))

}

.optionsDefault = function(){

 .assign("currentBANDWIDTH.P", 2/3) 	#currentBANDWIDTH.P <<- nPerc
 .assign("currentBANDWIDTH" , 30 ) 		#currentBANDWIDTH <<- n
 .assign("currentP", 0.5 )			#currentP <<- p

#Tree Mask
.assign("stc", c(5,2,1))

#Segment Plot
.assign("makeSegPlot" , FALSE)

#Delete series shorter than
.assign("remove.shorter.series" , TRUE)
.assign("delete.shorter.series", 100)

#Correlation with the master 
.assign("lowCorr", 0.256)

#Interactive Detrending
.assign("interactive.detrend", TRUE)

#FIRST DETRENDING
.assign("makeFirstDetrending", TRUE)
.assign("method1", "Spline") # "ModNegExp"    #"Spline" #   "Spline"  # "Mean"	  #method = c("Spline", "ModNegExp", "Mean")
.assign("n1" , 100)
.assign("nPerc1", 2/3)
.assign("p1", 0.5)
.assign("first.detrending.method", GetDetrendMethod(method1,n1 ,nPerc1, p1))
.assign("save.detrend1", TRUE)

#SECOND DETRENDING 
.assign("makeSecondDetrending", TRUE)
.assign("method2", "Mean")     # "Spline"    # "Mean"     # "Mean"    #"Mean"   	  #method = c("Spline", "ModNegExp", "Mean")
.assign("n2", 60)
.assign("nPerc2", 2/3)       #1.33
.assign("p2", 0.5)
.assign("second.detrending.method", GetDetrendMethod(method2,n2 ,nPerc2, p2))
.assign("save.detrend2", TRUE)

#AR
.assign("makeAr", TRUE)
.assign("arMAX", 3)

#biweightMean
.assign("biweightMean", TRUE) 

#Run windows analysis
.assign("run.win.analysis", TRUE)
.assign("winLength", 50)
.assign("stepWin", 1)

#EPS.common.interval
.assign("make.common.EPS", TRUE)
.assign("make.select.period.EPS", TRUE)
.assign("first.year.common", 1900) #1900)
.assign("last.year.common", 2000)  #2000)

#Save graph 
.assign("saveCronoJpg", TRUE)
}

detrender()


