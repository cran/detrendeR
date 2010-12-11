TrwLessThan=function(rwl, TRW=0){
colnames(rwl)<-substr(paste(colnames(rwl),"       "),1,8)
 
TRW->value
if (value<0)  return(cat("Tree-ring width should be >=0! "))

if (value==0) cat("\nMISSING RINGS\n", sep="")

min(as.numeric(rownames(rwl)))->first.year

Missing = function(rw, value, first.year) { 
which(rw<=value)->years
years<-years+first.year-1
Years=c(years , rep(NA, length(rw)))
Years=Years[1:length(rw)]
Years
}

t(apply(rwl, 2, Missing, value=value, first.year=first.year))->a
if (all(is.na(a))) {
if (value==0)   cat("There are no missing rings.\n")
else {cat("There is no ring narrower than ", value, ".", sep="")}
} else {
a[,apply(a,2, sum, na.rm=T)>0]->a #->>a1

length(a[!is.na(a)])->number.of.rings
   as.data.frame(na.omit((a)))->a #->>a2
if (number.of.rings==1 & value==0) cat("There is one missing ring.\n", sep="")
if (number.of.rings==1 & value>0) cat("There is one ring narrower than ", value, ".\n", sep="")
if (value==0  & number.of.rings>1) cat("There are ", number.of.rings ," missing rings.\n", sep="")
if (number.of.rings>1 & value>0) cat("There are ", number.of.rings ," ring narrower than ", value, ".\n", sep="")

if (number.of.rings <50) {  
cbind( c("Series  ", rownames(a)), rbind(NA,a)) ->A
WriteMatrix (A, na=" ", col.names=F, row.names=F, ID=F) }
cat("\n")
}
}

#TrwLessThan(rwl, TRW=0.1)