# Climate Indecies Calculation software
# R language with TCL/TK package
# Programmed by Yujun Ouyang,Mar,2004
# rewritten by Yang Feng, July 2004

# modified 2013-11-15
# in exceedance() function, changed denominator for base period from 29 to (ys-1)
# this was a major error
#
# version 1.0, 2004-10-14
# modified, 2006-01-24, 
# change .Internal(cbind) to cbind
# change .Internal(rbind) to rbind
# modified 2007-03-23
# change .Internal(rep(0,n)) to rep(0,n)
# modified, 2007-11-26,
# get rid of .Internal on some functions: min(...), sort(...), round(...)
#            max(...), also get rid of dig=.. part from sort(...) function
#            change decrease=... part to decreasing=... at sort(...)
# modified, 2008-05-05,
# output TMAX mean value and TMIN mean value in nastat() function
# modified, 2008-05-06,
# add a random series on TMAX and TMIN in exceedance rate function
# modified thresholds in TN10p calculation, add an 1e-5 item to avoid 
# computational error like 3.60000 > 3.6000, two functions involved:
# nordaytem1() and exceedance()
# modified, 2008-06-16
# change all sort() to mysort(), deal with different version, also combined
# different levels threshold()


# Part I
# General functions & TCL/TK functions

library(cluster)
require(tcltk)
mysort<- if(getRversion()<='2.4.1') function(x,decreasing){.Internal(sort(x,decreasing=decreasing))} else function(x,decreasing){sort.int(x,decreasing=decreasing)}

fontHeading <- tkfont.create(family="times",size=40,weight="bold",slant="italic")
fontHeading1<-tkfont.create(family="times",size=20,weight="bold")
fontHeading2<-tkfont.create(family="times",size=14,weight="bold")
fontTextLabel <- tkfont.create(family="times",size=12)
fontFixedWidth <- tkfont.create(family="courier",size=12)
# initial value for check box
cbvalue1<-tclVar("1");cbvalue2<-tclVar("1");cbvalue3<-tclVar("1")
cbvalue4<-tclVar("1");cbvalue5<-tclVar("1");cbvalue6<-tclVar("1")
cbvalue7<-tclVar("1");cbvalue8<-tclVar("1");cbvalue9<-tclVar("1")
cbvalue10<-tclVar("1");cbvalue11<-tclVar("1");cbvalue12<-tclVar("1")
cbvalue13<-tclVar("1");cbvalue14<-tclVar("1");cbvalue15<-tclVar("1")
cbvalue16<-tclVar("0");cbvalue17<-tclVar("0");cbvalue18<-tclVar("0")
cbvalue19<-tclVar("0");cbvalue21<-tclVar("1")
#  initial value for parameters
stations<-tclVar(paste(""));stdt<-tclVar(paste("3"));prcpMlev.c<-tclVar(paste("200"))
Entry1<-tclVar(paste("1961"));Entry2<-tclVar(paste("1990"))
#Entry3<-tclVar(paste("5"))
Entry4<-tclVar(paste("0"))
Entry5<-tclVar(paste("0"))
Entry6<-tclVar(paste("25"));Entry7<-tclVar(paste("0"))
Entry8<-tclVar(paste("20"));Entry9<-tclVar(paste("0"))
#Entry10<-tclVar(paste("10"));Entry11<-tclVar(paste("5"))
Entry12<-tclVar(paste("25"))
dayim<-as.integer(c(31,28,31,30,31,30,31,31,30,31,30,31))
crt<-3;flag=F
treshold=5;winsize=5
uu<-25;lu<-20
ul<-0;ll<-0

title1<-"Plot of Ind143";title2<-"Ind143";title3<-"Years"
frc<-function(dd,year,month,item){

              a<-dd[dd$year==year & dd$month==month,item]
              a<-a[a>-99]
              frc<-length(a)/rdim(year,month)
                                   }#end

done<-function(){tkdestroy(start1)}

percentile<-function(n,x,pctile){
	 x1<-x[is.na(x)==F]
	 n1<-length(x1)
         a<-mysort(x1,decreasing=F)
         b<-n1*pctile+0.3333*pctile+0.3333
         bb<-trunc(b)
         percentile<-a[bb]+(b-bb)*(a[bb+1]-a[bb]) }#end

pplotts<-function(var="prcp",type="h",tit=NULL){
  if(var=="dtr"){
  ymax<-max(dd[,"tmax"]-dd[,"tmin"],na.rm=T)
  ymin<-0
  }
  else if(var=="prcp"){
  ymax<-max(dd[,var],na.rm=T)
  ymin<-0
  }
  else{
  ymax<-max(dd[,var],na.rm=T)+1
  ymin<-min(dd[,var],na.rm=T)-1
  }
  if(is.na(ymax)|is.na(ymin)|(ymax==-Inf)|(ymin==-Inf)){
    ymax<-100
    ymin<-(-100)
  }
  op<-par(no.readonly=T)
  par(mfrow=c(4,1))
  par(mar=c(3.1,2.1,2.1,2.1),cex=1)
# par(font.axis=2, font.lab=2, font.main=2, font.sub=2)
  for(i in seq(years,yeare,10)){
    at<-rep(1,10)
#   if(i>yeare)
    for(j in (i+1):min(i+9,yeare+1)){
      if(leapyear(j)) at[j-i+1]<-at[j-i]+366
      else at[j-i+1]<-at[j-i]+365
    }
    if(var=="dtr")
      ttmp<-dd[dd$year>=i&dd$year<=min(i+9,yeare),"tmax"]-dd[dd$year>=i&dd$year<=min(i+9,yeare),"tmin"]
    else ttmp<-dd[dd$year>=i&dd$year<=min(i+9,yeare),var]
    plot(1:length(ttmp),ttmp,type=type,col="blue",xlab="",ylab="",xaxt="n",xlim=c(1,3660),ylim=c(ymin,ymax))
    abline(h=0)
    tt<-seq(1,length(ttmp))
    tt<-tt[is.na(ttmp)==T]
    axis(side=1,at=at,labels=c(i:(i+9)))
    for(k in 1:10) abline(v=at[k],col="yellow")
    lines(tt,rep(0,length(tt)),type="p",col="red")
    if(var=="prcp") cvar<-paste(var,"(mm)") else cvar<-var
    title(paste("Station: ",tit,", ",i,"~",min(i+9,yeare),",  ",cvar,sep=""))
  }
  par(op)
}

ind143gsl<-function(){
  if (latitude<0) south=T else south=F
  if (latitude<0) eyear=yeare-1 else eyear=yeare
  threshold<-5
  a<-eyear-years+1
  b<-rep(0,a)
  b<-cbind(b,b)
  colnames(b)<-c("year","gsl")
  i=1
  for (year in years:eyear) {
    b[i,"year"]<-year
    if(south){
      gslstart<-dd[dd$year==year&dd$month>6,]
      gslstart<-(gslstart[,"tmax"]+gslstart[,"tmin"])/2
      gslend<-dd[dd$year==(year+1)&dd$month<7,]
      gslend<-(gslend[,"tmax"]+gslend[,"tmin"])/2
    }
    else{
      gslstart<-dd[dd$year==year&dd$month<7,]
      gslstart<-(gslstart[,"tmax"]+gslstart[,"tmin"])/2
      gslend<-dd[dd$year==year&dd$month>6,]
      gslend<-(gslend[,"tmax"]+gslend[,"tmin"])/2
    }
    beginday<-0
    count=0
    for(step in 1:length(gslstart)){
      if(is.na(gslstart[step])==F){
        if(gslstart[step]>threshold) count<-count+1
        else count<-0
      }
      else count<-0
      if(count>5){
        beginday<-step-5
        break
      }
    }

#    if(beginday==0){
#      b[i,"gsl"]<-0
#      break
#    }

    endday<-0
    count<-0
    for(step in 1:length(gslend)){
      if(is.na(gslend[step])==F){
        if(gslend[step]<threshold) count<-count+1
        else count<-0
      }
      else count<-0
      if(count>5){
         endday<-step-5
         break
      }
    }
      
    if(sum(is.na(gslstart))+sum(is.na(gslend))>15)  b[i,"gsl"]<-NA
    else{
      if(beginday==0)
        b[i,"gsl"]<-0
      else {
        if(endday==0)  b[i,"gsl"]<-length(gslend)+length(gslstart)-beginday
        else b[i,"gsl"]<-endday+length(gslstart)-beginday
      }
    }
        
    i=i+1
  } 
  b<-as.data.frame(b)
  nam1<-paste(outinddir,paste(ofilename,"_GSL.csv",sep=""),sep="/")
  write.table(b,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
  
  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(b[,"gsl"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
    fit1<-lsfit(b[,1],b[,"gsl"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"gsl",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)
  
  nam2<-paste(outjpgdir,paste(ofilename,"_GSL.jpg",sep=""),sep="/")
  jpeg(file=nam2,width=1024,height=768)
  plotx(b[,1],b[,"gsl"],main=paste("GSL",ofilename,sep="   "),xlab="Year",ylab="GSL")
  dev.off()
}

dataext<-function(dd,year,month,day,item){
dataext<-dd[dd$year==year & dd$month==month & dd$day==day,item]
}

leapyear<-function(year){
    remainder400 <-trunc(year-400*trunc(year/400))
    remainder100 <-trunc(year-100*trunc(year/100))
    remainder4 <-trunc(year-4*trunc(year/4))
     if (remainder400 == 0) leapyear = T
     else{
       if(remainder100 == 0) leapyear = F
       else{
         if(remainder4 == 0) leapyear = T
	 else leapyear = F
	 }
       }
}

rdim<-function(year,month) {
   a<-leapyear(year) 
   if (month==1) rdim<-31
   else if (month==3) rdim<-31
   else if (month==4) rdim<-30
   else if (month==5) rdim<-31
   else if (month==6) rdim<-30
   else if (month==7) rdim<-31
   else if (month==8) rdim<-31
   else if (month==9) rdim<-30
   else if (month==10) rdim<-31
   else if (month==11) rdim<-30
   else if (month==12) rdim<-31
   else if (a==T & month==2) rdim<-29
   else rdim<-28  
       }


qcontrol<-function(){
  tkmessageBox(message=paste("Data QC(",ofilename,") may take a few minutes, click OK to continue.",sep=""))
# output records of problematic like prcp <0 and NA
ddu<-duplicated(dd[,c("year","month","day")])
if(sum(ddu)>0){
  nam1<-paste(outlogdir,paste(ofilename,"dupliQC.csv",sep=""),sep="/")
  msg=paste("Date duplicated found in original data file, please check:",nam1,sep=" ")
  tkmessageBox(message=msg)
  ddu2<-dd[duplicated(dd[,c("year","month","day")])==T,c("year", "month", "day")]
  nam1<-paste(outlogdir,paste(ofilename,"dupliQC.csv",sep=""),sep="/")
  write.table(ddu2,file=nam1,append=F,quote=F,sep=", ",row.names=F)
  tkdestroy(start1)
  stop(paste("QC stopped due to duplicated date, please check ",nam1,sep=""))
}
  
mid<-dd[is.na(dd$prcp)==F,]
mid<-mid[mid$prcp<0|mid$prcp>prcpMaxlev,]
#dd[is.na(dd$prcp)==F & dd$prcp<0,"prcp"]<-NA
nam1<-paste(outlogdir,paste(ofilename,"_prcpQC.csv",sep=""),sep="/")
write.table(mid,file=nam1,append=F,quote=F,sep=", ",row.names=F)
if (dim(mid)[1]>0) tkmessageBox(message=paste("Errors in prcp, please check the log file",nam1,sep=" "))
# output plots for PRCP
nam1<-paste(outlogdir,paste(ofilename,"_prcpPLOT.pdf",sep=""),sep="/")
pdf(file=nam1)
ttmp<-dd[dd$prcp>=1,"prcp"]
ttmp<-ttmp[is.na(ttmp)==F]
if(length(ttmp)>30){
  hist(ttmp,main=paste("Histogram for Station:",ofilename," of PRCP>=1mm",sep=""),breaks=c(seq(0,20,2),max(30,ttmp)),xlab="",col="green",freq=F)
  lines(density(ttmp,bw=0.2,from=1),col="red")
}
  pplotts(var="prcp",tit=ofilename)
  dev.off()
  nam1<-paste(outlogdir,paste(ofilename,"_tmaxPLOT.pdf",sep=""),sep="/")
  pdf(file=nam1)
  pplotts(var="tmax",type="l",tit=ofilename)
  dev.off()
  nam1<-paste(outlogdir,paste(ofilename,"_tminPLOT.pdf",sep=""),sep="/")
pdf(file=nam1)
pplotts(var="tmin",type="l",tit=ofilename)
dev.off()
nam1<-paste(outlogdir,paste(ofilename,"_dtrPLOT.pdf",sep=""),sep="/")
pdf(file=nam1)
pplotts(var="dtr",type="l",tit=ofilename)
dev.off()

#par(mfrow=c(1,1))
# output problematic temperature like tmax < tmin
mm<-dd[,"tmax"]-dd[,"tmin"]
dd<-cbind(dd,mm)
dimnames(dd)[[2]][7]<-"dtr"
   #output "log" file review
   temiss<-dd
   temiss<-temiss[is.na(temiss[,"tmax"])==F&is.na(temiss[,"tmin"])==F,]
#  temiss<-temiss[is.na(temiss[,6])==F,]
   temiss<-temiss[temiss[,7]<=0|temiss[,5]<=(-70)|temiss[,5]>=70|temiss[,6]<=(-70)|temiss[,6]>=70,]
   dimnames(temiss)[[2]][7]<-"tmax-tmin"
   nam1<-paste(outlogdir,paste(ofilename,"_tempQC.csv",sep=""),sep="/")
   write.table(temiss,file=nam1,append=F,quote=F,sep=", ",row.names=F)
if (dim(temiss)[1]>0) {
  tkmessageBox(message=paste("Errors in temperature, please check the log file",nam1,sep=" "))
   # records with abs(tmax)>=70, abs(tmin)>=70 set to NA
   dd[is.na(dd[,5])==F & abs(dd[,5])>=70,5]<-NA
   dd[is.na(dd[,6])==F & abs(dd[,6])>=70,6]<-NA
   # records with tmax < tmin are set to NA
    dd[is.na(dd[,5])==F & is.na(dd[,6])==F & dd[,"dtr"]<0,c("tmax","tmin")]<-NA
#   dd[is.na(dd[,5])==F & dd[,"mm"]<0,"tmin"]<-NA
}
#  dd<-dd[,-7]

# output problematic temperature which is out of 3 standard diviation (temp only)
ys<-yeare-years+1

tmaxm<-matrix(0,ys,365)
tminm<-matrix(0,ys,365)
tdtrm<-matrix(0,ys,365)

tmaxstd<-rep(0,365)
tminstd<-rep(0,365)
tdtrstd<-rep(0,365)

tmaxmean<-rep(0,365)
tminmean<-rep(0,365)
tdtrmean<-rep(0,365)

for(i in 1:ys)
  tmaxm[i,]<-dd[dd[,"year"]==(i+years-1)&(dd[,"month"]*100+dd[,"day"]!=229),"tmax"]
for(i in 1:365){
  tmaxstd[i]<-sqrt(var(tmaxm[,i],na.rm=T))
  tmaxmean[i]<-mean(tmaxm[,i],na.rm=T)
}

for(i in 1:ys)
  tminm[i,]<-dd[dd[,"year"]==(i+years-1)&(dd[,"month"]*100+dd[,"day"]!=229),"tmin"]
for(i in 1:365){
  tminstd[i]<-sqrt(var(tminm[,i],na.rm=T))
  tminmean[i]<-mean(tminm[,i],na.rm=T)
}

for(i in 1:ys)
  tdtrm[i,]<-dd[dd[,"year"]==(i+years-1)&(dd[,"month"]*100+dd[,"day"]!=229),"dtr"]
for(i in 1:365){
  tdtrstd[i]<-sqrt(var(tdtrm[,i],na.rm=T))
  tdtrmean[i]<-mean(tdtrm[,i],na.rm=T)
}

tmaxstdleap<-rep(0,366)
tmaxstdleap[1:59]<-tmaxstd[1:59]
tmaxstdleap[60]<-tmaxstd[59]
tmaxstdleap[61:366]<-tmaxstd[60:365]

tmaxmeanleap<-rep(0,366)
tmaxmeanleap[1:59]<-tmaxmean[1:59]
tmaxmeanleap[60]<-tmaxmean[59]
tmaxmeanleap[61:366]<-tmaxmean[60:365]

tminstdleap<-rep(0,366)
tminstdleap[1:59]<-tminstd[1:59]
tminstdleap[60]<-tminstd[59]
tminstdleap[61:366]<-tminstd[60:365]

tminmeanleap<-rep(0,366)
tminmeanleap[1:59]<-tminmean[1:59]
tminmeanleap[60]<-tminmean[59]
tminmeanleap[61:366]<-tminmean[60:365]

tdtrstdleap<-rep(0,366)
tdtrstdleap[1:59]<-tdtrstd[1:59]
tdtrstdleap[60]<-tdtrstd[59]
tdtrstdleap[61:366]<-tdtrstd[60:365]

tdtrmeanleap<-rep(0,366)
tdtrmeanleap[1:59]<-tdtrmean[1:59]
tdtrmeanleap[60]<-tdtrmean[59]
tdtrmeanleap[61:366]<-tdtrmean[60:365]

tmp<-matrix(0,dim(dd)[1],6)
dimnames(tmp)<-list(NULL,c("tmaxlow","tmaxup","tminlow","tminup","dtrlow","dtrup"))

idx<-0
for(i in years:yeare){
  if(leapyear(i)==T){
    tmp[(idx+1):(idx+366),1]<-tmaxmeanleap-crt*tmaxstdleap
    tmp[(idx+1):(idx+366),2]<-tmaxmeanleap+crt*tmaxstdleap
    tmp[(idx+1):(idx+366),3]<-tminmeanleap-crt*tminstdleap
    tmp[(idx+1):(idx+366),4]<-tminmeanleap+crt*tminstdleap
    tmp[(idx+1):(idx+366),5]<-tdtrmeanleap-crt*tdtrstdleap
    tmp[(idx+1):(idx+366),6]<-tdtrmeanleap+crt*tdtrstdleap
    idx<-idx+366
  }
  else{
    tmp[(idx+1):(idx+365),1]<-tmaxmean-crt*tmaxstd
    tmp[(idx+1):(idx+365),2]<-tmaxmean+crt*tmaxstd
    tmp[(idx+1):(idx+365),3]<-tminmean-crt*tminstd
    tmp[(idx+1):(idx+365),4]<-tminmean+crt*tminstd
    tmp[(idx+1):(idx+365),5]<-tdtrmean-crt*tdtrstd
    tmp[(idx+1):(idx+365),6]<-tdtrmean+crt*tdtrstd
    idx<-idx+365
  }
}

odata<-cbind(dd,tmp)

odata<-odata[is.na(odata[,"tmax"])==F,]
odata<-odata[is.na(odata[,"tmin"])==F,]
odata<-odata[is.na(odata[,"dtr"])==F,]
o1data<-odata[odata[,5]<odata[,8]|odata[,5]>odata[,9]|odata[,6]<odata[,10]|odata[,6]>odata[,11]|odata[,7]<odata[,12]|odata[,7]>odata[,13],]
#o2data<-odata[odata[,5]>odata[,8],]
#o3data<-odata[odata[,6]<odata[,9],]
#o4data<-odata[odata[,6]>odata[,10],]

#write.table(errstdo,file=nam1,append=F,quote=F,sep=",",row.names=F)
if (dim(o1data)[1] > 0){
  nam1<-paste(outlogdir,paste(ofilename,"_tepstdQC.csv",sep=""),sep="/")
  tkmessageBox(message=paste("Outliers found, please check the log file: ",nam1,sep=""))
  ofile<-cbind(o1data[,c(1,2,3,8,5,9,10,6,11,12,7,13)])
  write.table(round(ofile,digit=2),file=nam1,append=F,quote=F,sep=",",row.names=F)
}

dd<-dd[,c("year","month","day","prcp","tmax","tmin")];assign("dd",dd,envir=.GlobalEnv)

namcal<-paste(nama,"indcal.csv",sep="")
assign("namcal",namcal,envir=.GlobalEnv)
write.table(dd,file=namcal,append=F,quote=F,sep=",",row.names=F,na="-99.9")

tkmessageBox(message=paste("If you have checked data(", namcal,"), click OK to continue.",sep=""))
}# end of qcontrol()

nastat<-function(){
   dd <- read.table(namcal,header=T,sep=",",na.strings="-99.9",colClasses=rep("real",6))
   assign("dd",dd,envir=.GlobalEnv)
# NA statistics
nast<-rep(0,12)
nast<-array(nast,c(1,12,12,(yeare-years+1)))
dimnames(nast)<-list(NULL,c("ynapr","ynatma","ynatmi","napr","natma","natmi","mnapr>3","mnatma>3","mnatmi>3","ynapr>15","ynatma>15","ynatmi>15"),NULL,NULL)
ys<-yeare-years+1                    
year=years
aa1<-matrix(NA,12*ys,4)
dimnames(aa1)<-list(NULL,c("year","month","tmaxm","tminm"))
aa1[,"year"]<-years:yeare
aa1[,"year"]<-mysort(aa1[,"year"],decreasing=F)
aa1[,"month"]<-1:12
aa2<-matrix(NA,ys,3)
dimnames(aa2)<-list(NULL,c("year","tmaxm","tminm"))
aa2[,"year"]<-years:yeare
for (i in 1:(yeare-years+1)){
  month<-1;midvalue1<-dd[dd$year==year,]
  aa2[i,"tmaxm"]<-mean(midvalue1[,"tmax"],na.rm=T)
  aa2[i,"tminm"]<-mean(midvalue1[,"tmin"],na.rm=T)
  for (j in 1:12){
    midvalue2<-midvalue1[midvalue1$month==month,]
    aa1[(i-1)*12+j,"tmaxm"]<-mean(midvalue2[,"tmax"],na.rm=T)
    aa1[(i-1)*12+j,"tminm"]<-mean(midvalue2[,"tmin"],na.rm=T)
    nast[1,"ynapr",j,i]<-dim(midvalue1[is.na(midvalue1$prcp),])[1]
    if (nast[1,"ynapr",j,i]>15) nast[1,"ynapr>15",j,i]<-NA
    nast[1,"ynatma",j,i]<-dim(midvalue1[is.na(midvalue1$tmax),])[1]
    if (nast[1,"ynatma",j,i]>15) nast[1,"ynatma>15",j,i]<-NA
    nast[1,"ynatmi",j,i]<-dim(midvalue1[is.na(midvalue1$tmin),])[1]
    if (nast[1,"ynatmi",j,i]>15) nast[1,"ynatmi>15",j,i]<-NA
    nast[1,"napr",j,i]<-dim(midvalue2[is.na(midvalue2$prcp),])[1]
    if (nast[1,"napr",j,i]>3) nast[1,"mnapr>3",j,i]<-NA
    nast[1,"natma",j,i]<-dim(midvalue2[is.na(midvalue2$tmax),])[1]
    if (nast[1,"natma",j,i]>3) nast[1,"mnatma>3",j,i]<-NA
    nast[1,"natmi",j,i]<-dim(midvalue2[is.na(midvalue2$tmin),])[1]
    if (nast[1,"natmi",j,i]>3) nast[1,"mnatmi>3",j,i]<-NA
    month=month+1         }
    year=year+1           }
    nasto<-t(nast[,,,1])
    for ( i in 2:(yeare-years+1)){
      nasto<-rbind(nasto,t(nast[,,,i])) }
    nastout<-matrix(0,(yeare-years+1)*12,2)
    dimnames(nastout)<-list(NULL,c("year","month"))                
    nastout<-as.data.frame(nastout)
    nastout[,"year"]<-years:yeare
    nastout[,"year"]<-mysort(nastout[,"year"],decreasing=F)
    nastout[,"month"]<-1:12
    
    nastout<-cbind(nastout,nasto);assign("nastout",nastout,envir=.GlobalEnv)
    nastatistic<-nastout[,1:8]
   
    nacor<-nastout[,-(3:8)]
    ynacor<-matrix(0,ys,4)
    dimnames(ynacor)<-list(NULL,c("year","ynapr>15","ynatma>15","ynatmi>15"))
    ynacor[,"year"]<-years:yeare
    ynacor<-as.data.frame(ynacor)
    for (year in years:yeare){
    ynacor[ynacor$year==year,"ynapr>15"]<-nacor[nacor$year==year & nacor$month==1,"ynapr>15"]
    ynacor[ynacor$year==year,"ynatma>15"]<-nacor[nacor$year==year & nacor$month==1,"ynatma>15"]
    ynacor[ynacor$year==year,"ynatmi>15"]<-nacor[nacor$year==year & nacor$month==1,"ynatmi>15"]}
    nacor<-nacor[,1:5]
    assign("nacor",nacor,envir=.GlobalEnv)
    assign("ynacor",ynacor,envir=.GlobalEnv)
    if(sum(is.na(ynacor[,"ynapr>15"])==F)==0) prallna<-1
    else prallna<-0
    if(sum(is.na(ynacor[,"ynatma>15"])==F)==0) txallna<-1
    else txallna<-0
    if(sum(is.na(ynacor[,"ynatmi>15"])==F)==0) tnallna<-1
    else tnallna<-0
    assign("prallna",prallna,envir=.GlobalEnv)
    assign("txallna",txallna,envir=.GlobalEnv)
    assign("tnallna",tnallna,envir=.GlobalEnv)
    assign("nastatistic",nastatistic,envir=.GlobalEnv)
    nam1<-paste(outlogdir,paste(ofilename,"_nastatistic.csv",sep=""),sep="/")
    cat(file=nam1,"TITLE,YEAR,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC,ANN\n")
    for(year in years:yeare)
      for(i in 1:3) {
        if(i==1) tit<-"PRCP"
	if(i==2) tit<-"TMAX"
	if(i==3) tit<-"TMIN"
	line<-paste(tit,year,sep=",")
	for(mon in 1:12)
	line<-paste(line,nastatistic[nastatistic$year==year&nastatistic$month==mon,i+5],sep=",")
	line<-paste(line,nastatistic[nastatistic$year==year&nastatistic$month==1,i+2],sep=",")
	cat(file=nam1,line,fill=100,append=T)
      }
#   write.table(nastatistic,file=nam1,append=F,quote=F,sep=", ",row.names=F)
    aa1[,"tmaxm"]<-aa1[,"tmaxm"]+nacor[,"mnatma>3"]
    aa1[,"tminm"]<-aa1[,"tminm"]+nacor[,"mnatmi>3"]
    aa2[,"tmaxm"]<-aa2[,"tmaxm"]+ynacor[,"ynatma>15"]
    aa2[,"tminm"]<-aa2[,"tminm"]+ynacor[,"ynatmi>15"]
    ofile1<-paste(outinddir,paste(ofilename,"_TMAXmean.csv",sep=""),sep="/")
    odata<-matrix(0,ys,14)
    dimnames(odata)<-list(NULL,c("year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","annual"))
    odata[,1]<-years:yeare
    odata[,14]<-aa2[,"tmaxm"]
    for(i in 1:ys) odata[i,2:13]<-aa1[((i-1)*12+1):(i*12),"tmaxm"]
    write.table(round(odata,2),file=ofile1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
    odata[,14]<-aa2[,"tminm"]
    for(i in 1:ys) odata[i,2:13]<-aa1[((i-1)*12+1):(i*12),"tminm"]
    ofile1<-paste(outinddir,paste(ofilename,"_TMINmean.csv",sep=""),sep="/")
    write.table(round(odata,2),file=ofile1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
    if(txallna!=1){
    nam<-paste(outjpgdir,paste(ofilename,"_TMAXmean.jpg",sep=""),sep="/")
    jpeg(nam,width=1024,height=768)
    plotx(years:yeare,aa2[,"tmaxm"], main=paste("TMAXmean",ofilename,sep="   "),ylab="TMAXmean",xlab="Year")
    dev.off()
    }
    if(tnallna!=1){
    nam<-paste(outjpgdir,paste(ofilename,"_TMINmean.jpg",sep=""),sep="/")
    jpeg(nam,width=1024,height=768)
    plotx(years:yeare,aa2[,"tminm"], main=paste("TMINmean",ofilename,sep="   "),ylab="TMINmean",xlab="Year")
    dev.off()
    }
    assign("aa2",aa2,envir=.GlobalEnv)

    parameter()
 }#end of nastat()


getfile<-function() {
     name <- tclvalue(tkgetOpenFile(filetypes="{{TEXT Files} {.txt}} {{All files} *}"))
     if (name=="") return();
     dd <- read.table(name,header=F,col.names=c("year","month","day","prcp","tmax","tmin"),colClasses=rep("real",6))
     nama<-substr(name,start=1,stop=(nchar(name)-4))
     outdirtmp<-strsplit(name,"/")[[1]]
     if(length(outdirtmp)<=2){
       outinddir<-paste(strsplit(name,":")[[1]][1],"indices",sep=":/")
       outlogdir<-paste(strsplit(name,":")[[1]][1],"log",sep=":/")
       outjpgdir<-paste(strsplit(name,":")[[1]][1],"plots",sep=":/")
       outtrddir<-paste(strsplit(name,":")[[1]][1],"trend",sep=":/")
     }
     else{
       outdir<-outdirtmp[1]
       for(i in 2:(length(outdirtmp)-1))
         outdir<-paste(outdir,outdirtmp[i],sep="/")
         outinddir<-paste(outdir,"indices",sep="/")
         outlogdir<-paste(outdir,"log",sep="/")
         outjpgdir<-paste(outdir,"plots",sep="/")
         outtrddir<-paste(outdir,"trend",sep="/")
     }
     ofilename<-substr(outdirtmp[length(outdirtmp)],start=1,stop=(nchar(outdirtmp[length(outdirtmp)])-4))
     if(!file.exists(outinddir)) dir.create(outinddir)
     if(!file.exists(outlogdir)) dir.create(outlogdir)
     if(!file.exists(outjpgdir)) dir.create(outjpgdir)
     if(!file.exists(outtrddir)) dir.create(outtrddir)
     
#     dimnames(dd)<-list(NULL,c("year","month","day","prcp","tmax","tmin"))
     assign("nama",nama,envir=.GlobalEnv)
     assign("outinddir",outinddir,envir=.GlobalEnv)
     assign("outlogdir",outlogdir,envir=.GlobalEnv)
     assign("outjpgdir",outjpgdir,envir=.GlobalEnv)
     assign("outtrddir",outtrddir,envir=.GlobalEnv)
     assign("ofilename",ofilename,envir=.GlobalEnv)
    # dd<-dd[dd$tmax!=-99.9,]
    # dd$year<-dd$year+40 # just for the test data
    # replace missing value with NA
     dd[dd$prcp<=(-99.),"prcp"]<-NA
     dd[dd$tmax<=(-99.),"tmax"]<-NA
     dd[dd$tmin<=(-99.),"tmin"]<-NA
    # replace missing records
     ddd<-matrix(NA,365,6)
     dddl<-matrix(NA,366,6)
     dimnames(ddd)<-list(NULL,c("year","month","day","prcp","tmax","tmin"))
     dimnames(dddl)<-list(NULL,c("year","month","day","prcp","tmax","tmin"))
     ddd[1:31,"month"]<-1;ddd[1:31,"day"]<-c(1:31);ddd[32:59,"month"]<-2;ddd[32:59,"day"]<-c(1:28)
     ddd[60:90,"month"]<-3;ddd[60:90,"day"]<-c(1:31);ddd[91:120,"month"]<-4;ddd[91:120,"day"]<-c(1:30)
     ddd[121:151,"month"]<-5;ddd[121:151,"day"]<-c(1:31);ddd[152:181,"month"]<-6;ddd[152:181,"day"]<-c(1:30)
     ddd[182:212,"month"]<-7;ddd[182:212,"day"]<-c(1:31);ddd[213:243,"month"]<-8;ddd[213:243,"day"]<-c(1:31)
     ddd[244:273,"month"]<-9;ddd[244:273,"day"]<-c(1:30);ddd[274:304,"month"]<-10;ddd[274:304,"day"]<-c(1:31)
     ddd[305:334,"month"]<-11;ddd[305:334,"day"]<-c(1:30);ddd[335:365,"month"]<-12;ddd[335:365,"day"]<-c(1:31)

     dddl[1:31,"month"]<-1;dddl[1:31,"day"]<-c(1:31);dddl[32:60,"month"]<-2;dddl[32:60,"day"]<-c(1:29)
     dddl[61:91,"month"]<-3;dddl[61:91,"day"]<-c(1:31);dddl[92:121,"month"]<-4;dddl[92:121,"day"]<-c(1:30)
     dddl[122:152,"month"]<-5;dddl[122:152,"day"]<-c(1:31);dddl[153:182,"month"]<-6;dddl[153:182,"day"]<-c(1:30)
     dddl[183:213,"month"]<-7;dddl[183:213,"day"]<-c(1:31);dddl[214:244,"month"]<-8;dddl[214:244,"day"]<-c(1:31)
     dddl[245:274,"month"]<-9;dddl[245:274,"day"]<-c(1:30);dddl[275:305,"month"]<-10;dddl[275:305,"day"]<-c(1:31)
     dddl[306:335,"month"]<-11;dddl[306:335,"day"]<-c(1:30);dddl[336:366,"month"]<-12;dddl[336:366,"day"]<-c(1:31)

     years<-dd[1,1];yeare<-dd[dim(dd)[1],1]
     if (leapyear(years)) dddd<-dddl else dddd<-ddd
     dddd[,"year"]<-years
     for (year in years:yeare){                  # year loop start
     if (leapyear(year)) dddd1<-dddl else dddd1<-ddd
     dddd1[,"year"]<-year
     if (year!=years) dddd<-rbind(dddd,dddd1) }# year loop end

     dddd<-as.data.frame(dddd)
     dddd2<-merge(dddd,dd,by=c("year","month","day"),all.x=T)
     dddd2<-dddd2[,-(4:6)]
     dimnames(dddd2)[[2]]<-c("year","month","day","prcp","tmax","tmin")
     tmporder<-dddd2[,"year"]*10000+dddd2[,"month"]*100+dddd2[,"day"]
     dd<-dddd2[order(tmporder),]
    
     assign("years",years,envir=.GlobalEnv)
     assign("yeare",yeare,envir=.GlobalEnv)
     assign("dd",dd,envir=.GlobalEnv)

     tkmessageBox(message=paste("Data(",ofilename,") loaded, click OK to continue.",sep=""))
     
# enter station name and the times of stadard deviation
     infor1<-tktoplevel()
     tkfocus(infor1)
     tkgrab.set(infor1)
     tkwm.title(infor1,"Set Parameters for Data QC")

     textEntry1<-stations;textEntry2<-stdt;textEntry3<-prcpMlev.c
         
     textEntryWidget1<-tkentry(infor1,width=30,textvariable=textEntry1)
     textEntryWidget2<-tkentry(infor1,width=30,textvariable=textEntry2)
     textEntryWidget3<-tkentry(infor1,width=30,textvariable=textEntry3)
         
#     tkgrid(tklabel(infor1,text="!!Enter parameters please",font=fontHeading1))
     tkgrid(tklabel(infor1,text="                  Station name or code:"),textEntryWidget1)
     tkgrid(tklabel(infor1,text="Criteria(number of Stand Dev. for temp):"),textEntryWidget2)
     tkgrid(tklabel(infor1,text="Criteria(upper limit for PRCP in mm):"),textEntryWidget3)
          
     ok1<-function(){
       station<-as.character(tclvalue(textEntry1));assign("station",station,envir=.GlobalEnv)
       crt<-as.numeric(tclvalue(textEntry2));assign("crt",crt,envir=.GlobalEnv)
       prcpMaxlev<-as.numeric(tclvalue(textEntry3));assign("prcpMaxlev",prcpMaxlev,envir=.GlobalEnv)
       tkgrab.release(infor1)
       tkdestroy(infor1)
       stations<-textEntry1;assign("stations",stations,envir=.GlobalEnv)
       stdt<-textEntry2;assign("stdt",stdt,envir=.GlobalEnv)
       qcontrol();tkfocus(start1)
       }# end of ok

     cancel1<-function(){
     tkmessageBox(message="You have to enter these parameters before you can move on.")
     tkfocus(infor1)}# end of cancel1
     
     ok1.but<-    tkbutton(infor1,text="    OK    ",command=ok1)
     cancel1.but<-tkbutton(infor1,text="  CANCEL  ",command=cancel1)
     tkgrid(ok1.but,cancel1.but)
        }# end of getfile

parameter<-function(){
     infor<-tktoplevel()
     tkfocus(infor)
     tkgrab.set(infor)
     tkwm.title(infor,"Set Parameter Values")

     textEntry1<-Entry1
     textEntry2<-Entry2
#     textEntry3<-Entry3
     textEntry4<-Entry4
     textEntry5<-Entry5
     textEntry6<-Entry6;textEntry7<-Entry7
     textEntry8<-Entry8;textEntry9<-Entry9
#     textEntry10<-Entry10;textEntry11<-Entry11
     textEntry12<-Entry12
     
     textEntryWidget1<-tkentry(infor,width=20,textvariable=textEntry1)
     textEntryWidget2<-tkentry(infor,width=20,textvariable=textEntry2)
#     textEntryWidget3<-tkentry(infor,width=20,textvariable=textEntry3)
     textEntryWidget4<-tkentry(infor,width=20,textvariable=textEntry4)
     textEntryWidget5<-tkentry(infor,width=20,textvariable=textEntry5)
     textEntryWidget6<-tkentry(infor,width=20,textvariable=textEntry6)
     textEntryWidget7<-tkentry(infor,width=20,textvariable=textEntry7)
     textEntryWidget8<-tkentry(infor,width=20,textvariable=textEntry8)
     textEntryWidget9<-tkentry(infor,width=20,textvariable=textEntry9)
#     textEntryWidget10<-tkentry(infor,width=20,textvariable=textEntry10)
#     textEntryWidget11<-tkentry(infor,width=20,textvariable=textEntry11)
     textEntryWidget12<-tkentry(infor,width=20,textvariable=textEntry12)
     
     tkgrid(tklabel(infor,text="User defined parameters for Indices Calculation",font=fontHeading1))
     tkgrid(tklabel(infor,text="First year of base period"),textEntryWidget1)
     tkgrid(tklabel(infor,text="Last year of base period"),textEntryWidget2)
     tkgrid(tklabel(infor,text="Latitude of this station location"),textEntryWidget4)
     tkgrid(tklabel(infor,text="Longitude of this station location"),textEntryWidget5)
     tkgrid(tklabel(infor,text="User defined upper threshold of daily maximum temperature"),textEntryWidget6)
     tkgrid(tklabel(infor,text="User defined lower threshold of daily maximum temperature"),textEntryWidget7)
     tkgrid(tklabel(infor,text="User defined upper threshold of daily minimum temperature"),textEntryWidget8)
     tkgrid(tklabel(infor,text="User defined lower threshold of daily minimum temperature"),textEntryWidget9)
     tkgrid(tklabel(infor,text="User defined daily precipitation threshold"),textEntryWidget12)
     
     ok1<-function(){
#       tkmessageBox(message="This process may take 2 mins to initialize the data. Please wait until the window disapear!")
       startyear<-as.numeric(tclvalue(textEntry1));assign("startyear",startyear,envir=.GlobalEnv)
       endyear<-as.numeric(tclvalue(textEntry2));assign("endyear",endyear,envir=.GlobalEnv)
       if(startyear<years|endyear>yeare){
         if(startyear<years) msg<-paste("Input base period start:", startyear," less then start year of data:", years, sep=" ")
         else msg<-paste("Input base period end:", endyear," greater then end year of data:", yeare, sep=" ")
         tkmessageBox(message=msg)
         tkfocus(infor)
         return()
       }
#       winsize<-as.numeric(tclvalue(textEntry3));assign("winsize",winsize,envir=.GlobalEnv)
       latitude<-as.numeric(tclvalue(textEntry4));assign("latitude",latitude,envir=.GlobalEnv)
       longitude<-as.numeric(tclvalue(textEntry5));assign("longitude",longitude,envir=.GlobalEnv)
#       threshold<-as.numeric(tclvalue(textEntry5));assign("threshold",threshold,envir=.GlobalEnv)
       uuu<-as.numeric(tclvalue(textEntry6));assign("uuu",uuu,envir=.GlobalEnv)
       ulu<-as.numeric(tclvalue(textEntry7));assign("uul",ulu,envir=.GlobalEnv)
       uul<-as.numeric(tclvalue(textEntry8));assign("ulu",uul,envir=.GlobalEnv)
       ull<-as.numeric(tclvalue(textEntry9));assign("ull",ull,envir=.GlobalEnv)
#       up<-as.numeric(tclvalue(textEntry10));assign("up",up,envir=.GlobalEnv)
#       lp<-as.numeric(tclvalue(textEntry11));assign("lp",lp,envir=.GlobalEnv)
       nn<-as.numeric(tclvalue(textEntry12));assign("nn",nn,envir=.GlobalEnv)
       startpoint<-startyear-1;assign("startpoint",startpoint,envir=.GlobalEnv)
       endpoint<-endyear+1;assign("endpoint",endpoint,envir=.GlobalEnv)
       nordaytem1()
       tkgrab.release(infor)
       tkdestroy(infor)
       Entry1<-textEntry1;assign("Entry1",Entry1,envir=.GlobalEnv)
       Entry2<-textEntry2;assign("Entry2",Entry2,envir=.GlobalEnv)
#       Entry3<-textEntry3;assign("Entry3",Entry3,envir=.GlobalEnv)
       Entry4<-textEntry4;assign("Entry4",Entry4,envir=.GlobalEnv)
       Entry5<-textEntry5;assign("Entry5",Entry5,envir=.GlobalEnv)
       Entry6<-textEntry6;assign("Entry6",Entry6,envir=.GlobalEnv)
       Entry7<-textEntry7;assign("Entry7",Entry7,envir=.GlobalEnv)
       Entry8<-textEntry8;assign("Entry8",Entry8,envir=.GlobalEnv)
       Entry9<-textEntry9;assign("Entry9",Entry9,envir=.GlobalEnv)
#       Entry10<-textEntry10;assign("Entry10",Entry10,envir=.GlobalEnv)
#       Entry11<-textEntry11;assign("Entry11",Entry11,envir=.GlobalEnv)
       Entry12<-textEntry12;assign("Entry12",Entry12,envir=.GlobalEnv)
       main1()
     }

     cancel1<-function(){
#       tkmessageBox(message="Please enter these parameters before you can move forward!!")
#       tkfocus(infor)
       tkdestroy(infor)
#       tkdestroy(main)
#       tkfocus(start1)
       return()
     }
     
     ok1.but<-    tkbutton(infor,text="    OK    ",command=ok1)
     cancel1.but<-tkbutton(infor,text="  CANCEL  ",command=cancel1)
     tkgrid(ok1.but,cancel1.but)}

# End of Part I (general functions & TCL/TK functions

# Part II
# Functions of calculating climate indecies
hwfi<-function(){
  if (flag==T) return()
  hwfi<-matrix(0,(yeare-years+1),2)
  dimnames(hwfi)[[2]]<-c("year","wsdi")
  hwfi[,"year"]<-years:yeare
  for (year in years:yeare) {
    if(leapyear(year)){
      aa<-rep(0,366)
      aa[1:59]<-aas[,"pcmax90"][1:59]
      aa[60]<-aa[59]
      aa[61:366]<-aas[,"pcmax90"][60:365]
    }
    else aa<-aas[,"pcmax90"]
    bb<-dd[dd$year==year,"tmax"]
    if(length(aa)!=length(bb)) stop("ERROR in WSDI, check data!")
    midval<-bb-aa
    ylen<-length(aa)
    ycnt<-0
    icnt<-0
    for(i in 1:ylen){
      if(is.na(midval[i])==F&midval[i]>0)
        icnt<-icnt+1
      else{
        if(icnt>=6) ycnt<-ycnt+icnt
	icnt<-0
	}
      if(i==ylen&icnt>=6) ycnt<-ycnt+icnt
    }
    hwfi[year-years+1,2]<-ycnt
  }
  hwfi<-as.data.frame(hwfi)
  hwfi[,"wsdi"]<-hwfi[,"wsdi"]+ynacor[,"ynatma>15"]  
  nam1<-paste(outinddir,paste(ofilename,"_WSDI.csv",sep=""),sep="/")
  write.table(hwfi,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
  
  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(hwfi[,"wsdi"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
      fit1<-lsfit(hwfi[,"year"],hwfi[,"wsdi"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"wsdi",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

  nam2<-paste(outjpgdir,paste(ofilename,"_WSDI.jpg",sep=""),sep="/")
  jpeg(nam2,width=1024,height=768)
  plotx(hwfi[,1],hwfi[,2], main=paste("WSDI",ofilename,sep="   "),ylab="WSDI",xlab="Year")
  dev.off()
} # end of hwfi function

cwdi<-function(){
  if (flag==T) return()
  cwdi<-matrix(0,(yeare-years+1),2)
  dimnames(cwdi)[[2]]<-c("year","csdi")
  cwdi[,"year"]<-years:yeare
  for (year in years:yeare) {
    if(leapyear(year)){
      aa<-rep(0,366)
      aa[1:59]<-aas[,"pcmin10"][1:59]
      aa[60]<-aa[59]
      aa[61:366]<-aas[,"pcmin10"][60:365]
    }
    else aa<-aas[,"pcmin10"]
    bb<-dd[dd$year==year,"tmin"]
    if(length(aa)!=length(bb)) stop("ERROR in CWDI, check data!")
    midval<-aa-bb
    ylen<-length(aa)
    ycnt<-0
    icnt<-0
    for(i in 1:ylen){
      if(is.na(midval[i])==F&midval[i]>0)
        icnt<-icnt+1
      else{
        if(icnt>=6) ycnt<-ycnt+icnt
	icnt<-0
      }
    if(i==ylen&icnt>=6) ycnt<-ycnt+icnt
  }
  cwdi[year-years+1,2]<-ycnt
}
  cwdi<-as.data.frame(cwdi)
  cwdi[,"csdi"]<-cwdi[,"csdi"]+ynacor[,"ynatmi>15"]  
  nam1<-paste(outinddir,paste(ofilename,"_CSDI.csv",sep=""),sep="/")
  write.table(cwdi,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(cwdi[,"csdi"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
      fit1<-lsfit(cwdi[,"year"],cwdi[,"csdi"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"csdi",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

  nam2<-paste(outjpgdir,paste(ofilename,"_CSDI.jpg",sep=""),sep="/")
  jpeg(nam2,width=1024,height=768)
  plotx(cwdi[,1],cwdi[,2],main=paste("CSDI",ofilename,sep="   "),ylab="CSDI",xlab="Year")
  dev.off()
} # end of cwdi function

r95ptot<-function(){
  prcptmp<-dd[dd$year>=startyear&dd$year<=endyear&dd$prcp>=1,"prcp"]
  prcptmp<-prcptmp[is.na(prcptmp)==F]
  len<-length(prcptmp)
  prcp95<-percentile(len,prcptmp,0.95)
  prcp99<-percentile(len,prcptmp,0.99)

  ys<-yeare-years+1
  
  dp<-matrix(0,ys,4)
  dimnames(dp)<-list(NULL,c("year","r95p","r99p","prcptot"))
  dp[,"year"]<-years:yeare
  for(i in years:yeare){
    dp[(i-years+1),"r95p"]<-sum(dd[dd$year==i&dd$prcp>prcp95,"prcp"],na.rm=T)
    dp[(i-years+1),"r99p"]<-sum(dd[dd$year==i&dd$prcp>prcp99,"prcp"],na.rm=T)
    dp[(i-years+1),"prcptot"]<-sum(dd[dd$year==i&dd$prcp>=1,"prcp"],na.rm=T)
  }
  dp[,"r95p"]<-round(dp[,"r95p"],1)+ynacor[,"ynapr>15"]
  dp[,"r99p"]<-round(dp[,"r99p"],1)+ynacor[,"ynapr>15"]
  dp[,"prcptot"]<-round(dp[,"prcptot"],1)+ynacor[,"ynapr>15"]
  dp<-as.data.frame(dp)
  nam1<-paste(outinddir,paste(ofilename,"_R95p.csv",sep=""),sep="/")
  nam2<-paste(outinddir,paste(ofilename,"_R99p.csv",sep=""),sep="/")
  nam3<-paste(outinddir,paste(ofilename,"_PRCPTOT.csv",sep=""),sep="/")
  write.table(dp[,c("year","r95p")],file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
  write.table(dp[,c("year","r99p")],file=nam2,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
  write.table(dp[,c("year","prcptot")],file=nam3,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  for(i in c("r95p","r99p","prcptot")){
  if(sum(is.na(dp[,i]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
    fit1<-lsfit(dp[,"year"],dp[,i])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,i,years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)
}

  nam4<-paste(outjpgdir,paste(ofilename,"_R95p.jpg",sep=""),sep="/")
  jpeg(nam4,width=1024,height=768)
  plotx(dp[,1],dp[,"r95p"],main=paste("R95p",ofilename,sep="   "),xlab="Year",ylab="R95p")
  dev.off()
  nam5<-paste(outjpgdir,paste(ofilename,"_R99p.jpg",sep=""),sep="/")
  jpeg(nam5,width=1024,height=768)
  plotx(dp[,1],dp[,"r99p"],main=paste("R99p",ofilename,sep="   "),xlab="Year",ylab="R99p")
  dev.off()
  nam6<-paste(outjpgdir,paste(ofilename,"_PRCPTOT.jpg",sep=""),sep="/")
  jpeg(nam6,width=1024,height=768)
  plotx(dp[,1],dp[,"prcptot"],main=paste("PRCPTOT",ofilename,sep="   "),xlab="Year",ylab="PRCPTOT")
  dev.off()
} # end of function r95ptot

daysprcp20<-function(){
  ys<-yeare-years+1
R20<-rep(0,ys)
yearss<-c(years:yeare)
target<-as.data.frame(cbind(yearss,R20))
for (year in years:yeare){
  mid<-dd[dd$year==year,"prcp"]
  mid<-mid[is.na(mid)==F]
target[target$yearss==year,"R20"]<-length(mid[mid>=20])}# end for
dimnames(target)[[2]][1]<-"year"
 target[,"R20"]<-target[,"R20"]+ynacor[,"ynapr>15"]
nam1<-paste(outinddir,paste(ofilename,"_R20mm.csv",sep=""),sep="/")
write.table(target,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(target[,"R20"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
    fit1<-lsfit(target[,"year"],target[,"R20"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"r20mm",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

nam2<-paste(outjpgdir,paste(ofilename,"_R20mm.jpg",sep=""),sep="/")
jpeg(nam2,width=1024,height=768)
plotx(target[,1],target[,2],main=paste("R20mm",ofilename,sep="   "),xlab="Year",ylab="R20mm")
dev.off()
}

daysprcpn<-function(){
   ys<-yeare-years+1
Rnn<-rep(0,ys)
yearss<-c(years:yeare)
target<-as.data.frame(cbind(yearss,Rnn))
for (year in years:yeare){
  mid<-dd[dd$year==year,"prcp"]
  mid<-mid[is.na(mid)==F]
  target[target$yearss==year,"Rnn"]<-length(mid[mid>=nn])
}
  dimnames(target)[[2]][1]<-"year"
  target[,"Rnn"]<-target[,"Rnn"]+ynacor[,"ynapr>15"]

   nam1<-paste(outinddir,paste(ofilename,"_R",as.character(nn),"mm.csv",sep=""),sep="/")
   write.table(target,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(target[,"Rnn"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
      fit1<-lsfit(target[,1],target[,"Rnn"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,paste("R",as.character(nn),"mm",sep=""),years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

   nam2<-paste(outjpgdir,paste(ofilename,"_R",as.character(nn),"mm.jpg",sep=""),sep="/")
   jpeg(nam2,width=1024,height=768)
   plotx(target[,1],target[,2],main=paste("R",as.character(nn),"mm",ofilename,sep="   "),xlab="Year",ylab="Rnnmm")
   dev.off()
 }

nordaytem1<-function(){  # initialize data
# normal temp

daynorm<-dd[dd$year>=startyear,]
daynorm<-daynorm[daynorm$year<=endyear,] # initialize daynorm matrix
daynor<-daynorm              # create target matrix
nn<-dd[dd$year==startpoint,]
nn<-nn[nn$month==12,]
nn<-nn[nn$day>(31-round(winsize/2)),]
daynorm<-rbind(nn,daynorm)
nn<-dd[dd$year==endpoint,]
nn<-nn[nn$month==1,]
nn<-nn[nn$day<=round(winsize/2),]
daynorm<-rbind(daynorm,nn)

daynorm1<-daynorm[,-4]
daynorm1[daynorm1$month==2 & daynorm1$day==29,]<--99
daynorm1<-daynorm1[daynorm1$year!=-99,]
dayt<-daynorm1[,c("tmax","tmin")]

ddtem<-dd[,-4]
ddtem[ddtem$month==2 & ddtem$day==29,]<--99
ddtem<-ddtem[ddtem$year!=-99,]
assign("ddtem",ddtem,envir=.GlobalEnv)

a<-matrix(-99,5,5)
dimnames(a)[[2]]<-c("year","month","day","tmax","tmin")
ddtemt<-rbind(a,ddtem);assign("ddtemt",ddtemt,envir=.GlobalEnv)

ys<-endyear-startyear+1
window<-matrix(0,winsize,2)
windows<-array(window,c(winsize,2,366,ys))
dimnames(windows)<-list(NULL,c("tmax","tmin"),NULL,NULL)

i=winsize-round(winsize/2,digits=0)
i1=round(winsize/2,digits=0)
daynormm<-daynorm[,c("tmax","tmin")]
daynormm<-as.matrix(daynormm)
year<-startyear

for (k in 1:ys){
   if (leapyear(year)==T) jj<-366 else {jj<-365; windows[,,366,k]<--99 }
   year<-year+1
for (j in 1:jj){
    windows[,,j,k]<-daynormm[(i-i1):(i+i1),]
    i=i+1 }}

mwindows<-colMeans(windows,na.rm=T)
tmax<-mwindows["tmax",,];tmax<-tmax[tmax!=-99]
tmin<-mwindows["tmin",,];tmin<-tmin[tmin!=-99]
daynor[,"tmax"]<-tmax;daynor[,"tmin"]<-tmin

a<-rep(0,nrow(daynor))
a<-(daynor[,"tmax"]+daynor[,"tmin"])/2
daytemave<-a    
daynor<-cbind(daynor,daytemave)

# output the result to globe enviroment
assign("daynor",daynor,envir=.GlobalEnv)
assign("daynorm",daynorm,envir=.GlobalEnv)
assign("daynorm1",daynorm1,envir=.GlobalEnv)
assign("dayt",dayt,envir=.GlobalEnv)

# prcp percentile 95% and 99%
  prcpnorm<-dd[dd$year>=startyear,]
  prcpnorm<-prcpnorm[prcpnorm$year<=endyear,] # initialize prcpnorm matrix
  nnp<-dd[dd$year==startpoint,]
  nnp<-nnp[nnp$month==12,]
  nnp<-nnp[nnp$day>29,]
  prcpnorm<-rbind(nnp,prcpnorm)
  nnp<-dd[dd$year==endpoint,]
  nnp<-nnp[nnp$month==1,]
  nnp<-nnp[nnp$day<=2,]
  prcpnorm<-rbind(prcpnorm,nnp)
  prcpnorm<-prcpnorm[,1:4]
# remove Feb 29
  prcpnorm[prcpnorm$month==2 & prcpnorm$day==29,]<--99
  prcpnorm<-prcpnorm[prcpnorm$year!=-99,]
  assign("prcpnorm",prcpnorm,envir=.GlobalEnv)

  ys<-endyear-startyear+1 
 
  aasp<-matrix(NA,365,3)
  dimnames(aasp)<-list(NULL,c("day","prcp95","prcp99"))
  aasp[,"day"]<-1:365

  msp<-5*ys
  prcpnorm<-as.matrix(prcpnorm)
  pwindow<-matrix(0,5,1)
  pwindows<-array(pwindow,c(5,1,365,ys)) #array used to store all windows
  ip=3
  ip1=2
  for (k in 1:ys){
  for (j in 1:365){
   
     pwindows[,,j,k]<-prcpnorm[(ip-ip1):(ip+ip1),"prcp"]
      ip=ip+1}}

  prcpwin<-matrix(0,msp,2)
  prcpwin[,2]<-1:ys
  prcpwin[,2]<-mysort(prcpwin[,2],decreasing=F)

  prcpwins<-array(prcpwin,c(msp,2,365)) 

for (j in 1:365){
for (i in 1:ys){
    prcpwins[prcpwins[,2,j]==i,1,j]<-pwindows[,,j,i]}}
#assign("exwins",exwins,envir=.GlobalEnv)

  for (i in 1:365){
  assp<-prcpwins[,,i]
  if(sum(is.na(assp[,1])==F)>=1)
  aasp[i,"prcp95"]<-percentile(msp,assp[,1],0.95)
  else aasp[i,"prcp95"]<-NA
  if(sum(is.na(assp[,1])==F)>=1)
  aasp[i,"prcp99"]<-percentile(msp,assp[,1],0.99)
  else aasp[i,"prcp99"]<-NA
   }
assign("aasp",aasp,envir=.GlobalEnv)

aas<-matrix(NA,365,5)
dimnames(aas)<-list(NULL,c("day","pcmax10","pcmax90","pcmin10","pcmin90"))
aas[,"day"]<-1:365

ms<-winsize*ys
dayt<-as.matrix(dayt)
window<-matrix(0,winsize,2)
windows<-array(window,c(winsize,2,365,ys)) #array used to store all windows
i=winsize-round(winsize/2,digits=0)
i1=round(winsize/2,digits=0)
for (k in 1:ys){
for (j in 1:365){
   
      windows[,,j,k]<-dayt[(i-i1):(i+i1),]
      i=i+1}}

exwin<-matrix(0,ms,3)
exwin[,3]<-1:ys
exwin[,3]<-mysort(exwin[,3],decreasing=F)
assign("exwin",exwin,envir=.GlobalEnv)
#indd<-exwin[exwin[,3]!=ys,3]
exwins<-array(exwin,c(ms,3,365)) # array for bootstrap

for (j in 1:365){
for (i in 1:ys){
    exwins[exwins[,3,j]==i,1:2,j]<-windows[,,j,i]}}
assign("exwins",exwins,envir=.GlobalEnv)

for ( i in 1:365){
  ass<-exwins[,,i]
  ass1<-ass[,1]
  ass2<-ass[,2]
  kgb1<-length(ass1[is.na(ass1)])
  kgb2<-length(ass2[is.na(ass2)])
  if (kgb1>37.5 | kgb2>37.5) {flag=T;break}}#150*0.25=37.5

assign("flag",flag,envir=.GlobalEnv)
if (flag==T) tkmessageBox(message="More than 25% data missing, Exceedance rate, HWDI,CWDI will not be calculated!!")
if (flag==T) return()
for (i in 1:365){
  ass<-exwins[,,i]
# if(i == 363) {
# ttmp<-ms
# assign("ttmp",ttmp,envir=.GlobalEnv)
# }
  itmp<-percentile(ms,ass[,1],c(0.1,0.9))
  aas[i,"pcmax10"]<-itmp[1]-1e-5
  aas[i,"pcmax90"]<-itmp[2]+1e-5
  itmp<-percentile(ms,ass[,2],c(0.1,0.9))
  aas[i,"pcmin10"]<-itmp[1]-1e-5
  aas[i,"pcmin90"]<-itmp[2]+1e-5  }

assign("aas",aas,envir=.GlobalEnv)# matrix to store 10 and 90 percentile
# exceedance rate before 1961 and after 2000
before<-dd[dd$year<startyear,]
after<-dd[dd$year>endyear,]

# dataframe store the before monthly exceedance rate
ys1<-startyear-years;ys2<-yeare-endyear
bmonex<-matrix(NA,ys1*12,6)
dimnames(bmonex)<-list(NULL,c("year","month","tx10p","tx90p","tn10p","tn90p"))
bmonex[,"month"]<-rep(1:12,ys1)
bmonex[,"year"]<-years:(startyear-1)
bmonex[,"year"]<-mysort(bmonex[,"year"],decreasing=F)
bmonex<-as.data.frame(bmonex)

# dataframe store the after monthly exceedance rate
amonex<-matrix(NA,ys2*12,6)
dimnames(amonex)<-list(NULL,c("year","month","tx10p","tx90p","tn10p","tn90p"))
amonex[,"month"]<-rep(1:12,ys2)
amonex[,"year"]<-(endyear+1):yeare
amonex[,"year"]<-mysort(amonex[,"year"],decreasing=F)
amonex<-as.data.frame(amonex)

# dataframe store yearly exceedance rate (before and after)
yearex<-c(years:(startyear-1));txg10p<-rep(0,length(yearex))
txg90p<-rep(0,length(yearex));tng10p<-rep(0,length(yearex))
tng90p<-rep(0,length(yearex))
bd<-as.data.frame(cbind(yearex,txg10p,txg90p,tng10p,tng90p))
colnames(bd)[1]<-"year"

yearex<-c((endyear+1):yeare);txg10p<-rep(0,length(yearex))
txg90p<-rep(0,length(yearex));tng10p<-rep(0,length(yearex))
tng90p<-rep(0,length(yearex))
ad<-as.data.frame(cbind(yearex,txg10p,txg90p,tng10p,tng90p))
colnames(ad)[1]<-"year"

year=years;jjj6=1
for (i in 1:ys1){
   midvalue<-ddtem[ddtem$year==year,]
   exmax10<-midvalue[,4]-aas[,2]
   exmax10m1<-exmax10[1:31];exmax10m2<-exmax10[32:59];exmax10m3<-exmax10[60:90]
   exmax10m4<-exmax10[91:120];exmax10m5<-exmax10[121:151];exmax10m6<-exmax10[152:181]
   exmax10m7<-exmax10[182:212];exmax10m8<-exmax10[213:243];exmax10m9<-exmax10[244:273]
   exmax10m10<-exmax10[274:304];exmax10m11<-exmax10[305:334];exmax10m12<-exmax10[335:365]
   
   exmax90<-midvalue[,4]-aas[,3]
   exmax90m1<-exmax90[1:31];exmax90m2<-exmax90[32:59];exmax90m3<-exmax90[60:90]
   exmax90m4<-exmax90[91:120];exmax90m5<-exmax90[121:151];exmax90m6<-exmax90[152:181]
   exmax90m7<-exmax90[182:212];exmax90m8<-exmax90[213:243];exmax90m9<-exmax90[244:273]
   exmax90m10<-exmax90[274:304];exmax90m11<-exmax90[305:334];exmax90m12<-exmax90[335:365]

   exmin10<-midvalue[,5]-aas[,4]
   exmin10m1<-exmin10[1:31];exmin10m2<-exmin10[32:59];exmin10m3<-exmin10[60:90]
   exmin10m4<-exmin10[91:120];exmin10m5<-exmin10[121:151];exmin10m6<-exmin10[152:181]
   exmin10m7<-exmin10[182:212];exmin10m8<-exmin10[213:243];exmin10m9<-exmin10[244:273]
   exmin10m10<-exmin10[274:304];exmin10m11<-exmin10[305:334];exmin10m12<-exmin10[335:365]

   exmin90<-midvalue[,5]-aas[,5]
   exmin90m1<-exmin90[1:31];exmin90m2<-exmin90[32:59];exmin90m3<-exmin90[60:90]
   exmin90m4<-exmin90[91:120];exmin90m5<-exmin90[121:151];exmin90m6<-exmin90[152:181]
   exmin90m7<-exmin90[182:212];exmin90m8<-exmin90[213:243];exmin90m9<-exmin90[244:273]
   exmin90m10<-exmin90[274:304];exmin90m11<-exmin90[305:334];exmin90m12<-exmin90[335:365]

   bd[i,"txg10p"]<-length(exmax10[exmax10<0&is.na(exmax10)==F])
   bd[i,"txg90p"]<-length(exmax90[exmax90>0&is.na(exmax90)==F])
   bd[i,"tng10p"]<-length(exmin10[exmin10<0&is.na(exmin10)==F])
   bd[i,"tng90p"]<-length(exmin90[exmin90>0&is.na(exmin90)==F])
   
  bmonex[jjj6,"tx10p"]<-length(exmax10m1[exmax10m1<0&is.na(exmax10m1)==F])
  bmonex[jjj6,"tx90p"]<-length(exmax90m1[exmax90m1>0&is.na(exmax90m1)==F])
  bmonex[jjj6,"tn10p"]<-length(exmin10m1[exmin10m1<0&is.na(exmin10m1)==F])
  bmonex[jjj6,"tn90p"]<-length(exmin90m1[exmin90m1>0&is.na(exmin90m1)==F])

  bmonex[jjj6+1,"tx10p"]<-length(exmax10m2[exmax10m2<0&is.na(exmax10m2)==F])
  bmonex[jjj6+1,"tx90p"]<-length(exmax90m2[exmax90m2>0&is.na(exmax90m2)==F])
  bmonex[jjj6+1,"tn10p"]<-length(exmin10m2[exmin10m2<0&is.na(exmin10m2)==F])
  bmonex[jjj6+1,"tn90p"]<-length(exmin90m2[exmin90m2>0&is.na(exmin90m2)==F])   

  bmonex[jjj6+2,"tx10p"]<-length(exmax10m3[exmax10m3<0&is.na(exmax10m3)==F])
  bmonex[jjj6+2,"tx90p"]<-length(exmax90m3[exmax90m3>0&is.na(exmax90m3)==F])
  bmonex[jjj6+2,"tn10p"]<-length(exmin10m3[exmin10m3<0&is.na(exmin10m3)==F])
  bmonex[jjj6+2,"tn90p"]<-length(exmin90m3[exmin90m3>0&is.na(exmin90m3)==F])

  bmonex[jjj6+3,"tx10p"]<-length(exmax10m4[exmax10m4<0&is.na(exmax10m4)==F])
  bmonex[jjj6+3,"tx90p"]<-length(exmax90m4[exmax90m4>0&is.na(exmax90m4)==F])
  bmonex[jjj6+3,"tn10p"]<-length(exmin10m4[exmin10m4<0&is.na(exmin10m4)==F])
  bmonex[jjj6+3,"tn90p"]<-length(exmin90m4[exmin90m4>0&is.na(exmin90m4)==F])

  bmonex[jjj6+4,"tx10p"]<-length(exmax10m5[exmax10m5<0&is.na(exmax10m5)==F])
  bmonex[jjj6+4,"tx90p"]<-length(exmax90m5[exmax90m5>0&is.na(exmax90m5)==F])
  bmonex[jjj6+4,"tn10p"]<-length(exmin10m5[exmin10m5<0&is.na(exmin10m5)==F])
  bmonex[jjj6+4,"tn90p"]<-length(exmin90m5[exmin90m5>0&is.na(exmin90m5)==F])

  bmonex[jjj6+5,"tx10p"]<-length(exmax10m6[exmax10m6<0&is.na(exmax10m6)==F])
  bmonex[jjj6+5,"tx90p"]<-length(exmax90m6[exmax90m6>0&is.na(exmax90m6)==F])
  bmonex[jjj6+5,"tn10p"]<-length(exmin10m6[exmin10m6<0&is.na(exmin10m6)==F])
  bmonex[jjj6+5,"tn90p"]<-length(exmin90m6[exmin90m6>0&is.na(exmin90m6)==F])

  bmonex[jjj6+6,"tx10p"]<-length(exmax10m7[exmax10m7<0&is.na(exmax10m7)==F])
  bmonex[jjj6+6,"tx90p"]<-length(exmax90m7[exmax90m7>0&is.na(exmax90m7)==F])
  bmonex[jjj6+6,"tn10p"]<-length(exmin10m7[exmin10m7<0&is.na(exmin10m7)==F])
  bmonex[jjj6+6,"tn90p"]<-length(exmin90m7[exmin90m7>0&is.na(exmin90m7)==F])

  bmonex[jjj6+7,"tx10p"]<-length(exmax10m8[exmax10m8<0&is.na(exmax10m8)==F])
  bmonex[jjj6+7,"tx90p"]<-length(exmax90m8[exmax90m8>0&is.na(exmax90m8)==F])
  bmonex[jjj6+7,"tn10p"]<-length(exmin10m8[exmin10m8<0&is.na(exmin10m8)==F])
  bmonex[jjj6+7,"tn90p"]<-length(exmin90m8[exmin90m8>0&is.na(exmin90m8)==F])

  bmonex[jjj6+8,"tx10p"]<-length(exmax10m9[exmax10m9<0&is.na(exmax10m9)==F])
  bmonex[jjj6+8,"tx90p"]<-length(exmax90m9[exmax90m9>0&is.na(exmax90m9)==F])
  bmonex[jjj6+8,"tn10p"]<-length(exmin10m9[exmin10m9<0&is.na(exmin10m9)==F])
  bmonex[jjj6+8,"tn90p"]<-length(exmin90m9[exmin90m9>0&is.na(exmin90m9)==F])

  bmonex[jjj6+9,"tx10p"]<-length(exmax10m10[exmax10m10<0&is.na(exmax10m10)==F])
  bmonex[jjj6+9,"tx90p"]<-length(exmax90m10[exmax90m10>0&is.na(exmax90m10)==F])
  bmonex[jjj6+9,"tn10p"]<-length(exmin10m10[exmin10m10<0&is.na(exmin10m10)==F])
  bmonex[jjj6+9,"tn90p"]<-length(exmin90m10[exmin90m10>0&is.na(exmin90m10)==F])

  bmonex[jjj6+10,"tx10p"]<-length(exmax10m11[exmax10m11<0&is.na(exmax10m11)==F])
  bmonex[jjj6+10,"tx90p"]<-length(exmax90m11[exmax90m11>0&is.na(exmax90m11)==F])
  bmonex[jjj6+10,"tn10p"]<-length(exmin10m11[exmin10m11<0&is.na(exmin10m11)==F])
  bmonex[jjj6+10,"tn90p"]<-length(exmin90m11[exmin90m11>0&is.na(exmin90m11)==F])

  bmonex[jjj6+11,"tx10p"]<-length(exmax10m12[exmax10m12<0&is.na(exmax10m12)==F])
  bmonex[jjj6+11,"tx90p"]<-length(exmax90m12[exmax90m12>0&is.na(exmax90m12)==F])
  bmonex[jjj6+11,"tn10p"]<-length(exmin10m12[exmin10m12<0&is.na(exmin10m12)==F])
  bmonex[jjj6+11,"tn90p"]<-length(exmin90m12[exmin90m12>0&is.na(exmin90m12)==F])

   if(leapyear(year)){
     if(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"]>aas[59,"pcmax90"]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"])==F&is.na(aas[59,"pcmax90"])==F)
       bmonex[jjj6+1,"tx90p"]<-bmonex[jjj6+1,"tx90p"]+1
     if(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"]<aas[59,"pcmax10"]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"])==F&is.na(aas[59,"pcmax10"])==F)
       bmonex[jjj6+1,"tx10p"]<-bmonex[jjj6+1,"tx10p"]+1
     if(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"]>aas[59,"pcmin90"]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"])==F&is.na(aas[59,"pcmin90"])==F)
       bmonex[jjj6+1,"tn90p"]<-bmonex[jjj6+1,"tn90p"]+1
     if(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"]<aas[59,"pcmin10"]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"])==F&is.na(aas[59,"pcmin10"])==F)
       bmonex[jjj6+1,"tn10p"]<-bmonex[jjj6+1,"tn10p"]+1
   }

   jjj6<-jjj6+12
   year=year+1     
}

year=endyear+1;jjj6=1
for (i in 1:ys2){
   midvalue<-ddtem[ddtem$year==year,]
   exmax10<-midvalue[,4]-aas[,2]
   exmax10m1<-exmax10[1:31];exmax10m2<-exmax10[32:59];exmax10m3<-exmax10[60:90]
   exmax10m4<-exmax10[91:120];exmax10m5<-exmax10[121:151];exmax10m6<-exmax10[152:181]
   exmax10m7<-exmax10[182:212];exmax10m8<-exmax10[213:243];exmax10m9<-exmax10[244:273]
   exmax10m10<-exmax10[274:304];exmax10m11<-exmax10[305:334];exmax10m12<-exmax10[335:365]
   
   exmax90<-midvalue[,4]-aas[,3]
   exmax90m1<-exmax90[1:31];exmax90m2<-exmax90[32:59];exmax90m3<-exmax90[60:90]
   exmax90m4<-exmax90[91:120];exmax90m5<-exmax90[121:151];exmax90m6<-exmax90[152:181]
   exmax90m7<-exmax90[182:212];exmax90m8<-exmax90[213:243];exmax90m9<-exmax90[244:273]
   exmax90m10<-exmax90[274:304];exmax90m11<-exmax90[305:334];exmax90m12<-exmax90[335:365]

   exmin10<-midvalue[,5]-aas[,4]
   exmin10m1<-exmin10[1:31];exmin10m2<-exmin10[32:59];exmin10m3<-exmin10[60:90]
   exmin10m4<-exmin10[91:120];exmin10m5<-exmin10[121:151];exmin10m6<-exmin10[152:181]
   exmin10m7<-exmin10[182:212];exmin10m8<-exmin10[213:243];exmin10m9<-exmin10[244:273]
   exmin10m10<-exmin10[274:304];exmin10m11<-exmin10[305:334];exmin10m12<-exmin10[335:365]

   exmin90<-midvalue[,5]-aas[,5]
   exmin90m1<-exmin90[1:31];exmin90m2<-exmin90[32:59];exmin90m3<-exmin90[60:90]
   exmin90m4<-exmin90[91:120];exmin90m5<-exmin90[121:151];exmin90m6<-exmin90[152:181]
   exmin90m7<-exmin90[182:212];exmin90m8<-exmin90[213:243];exmin90m9<-exmin90[244:273]
   exmin90m10<-exmin90[274:304];exmin90m11<-exmin90[305:334];exmin90m12<-exmin90[335:365]
   
   ad[i,"txg10p"]<-length(exmax10[exmax10<0&is.na(exmax10)==F])
   ad[i,"txg90p"]<-length(exmax90[exmax90>0&is.na(exmax90)==F])
   ad[i,"tng10p"]<-length(exmin10[exmin10<0&is.na(exmin10)==F])
   ad[i,"tng90p"]<-length(exmin90[exmin90>0&is.na(exmin90)==F])
   
  amonex[jjj6,"tx10p"]<-length(exmax10m1[exmax10m1<0&is.na(exmax10m1)==F])
  amonex[jjj6,"tx90p"]<-length(exmax90m1[exmax90m1>0&is.na(exmax90m1)==F])
  amonex[jjj6,"tn10p"]<-length(exmin10m1[exmin10m1<0&is.na(exmin10m1)==F])
  amonex[jjj6,"tn90p"]<-length(exmin90m1[exmin90m1>0&is.na(exmin90m1)==F])

  amonex[jjj6+1,"tx10p"]<-length(exmax10m2[exmax10m2<0&is.na(exmax10m2)==F])
  amonex[jjj6+1,"tx90p"]<-length(exmax90m2[exmax90m2>0&is.na(exmax90m2)==F])
  amonex[jjj6+1,"tn10p"]<-length(exmin10m2[exmin10m2<0&is.na(exmin10m2)==F])
  amonex[jjj6+1,"tn90p"]<-length(exmin90m2[exmin90m2>0&is.na(exmin90m2)==F])   

  amonex[jjj6+2,"tx10p"]<-length(exmax10m3[exmax10m3<0&is.na(exmax10m3)==F])
  amonex[jjj6+2,"tx90p"]<-length(exmax90m3[exmax90m3>0&is.na(exmax90m3)==F])
  amonex[jjj6+2,"tn10p"]<-length(exmin10m3[exmin10m3<0&is.na(exmin10m3)==F])
  amonex[jjj6+2,"tn90p"]<-length(exmin90m3[exmin90m3>0&is.na(exmin90m3)==F])

  amonex[jjj6+3,"tx10p"]<-length(exmax10m4[exmax10m4<0&is.na(exmax10m4)==F])
  amonex[jjj6+3,"tx90p"]<-length(exmax90m4[exmax90m4>0&is.na(exmax90m4)==F])
  amonex[jjj6+3,"tn10p"]<-length(exmin10m4[exmin10m4<0&is.na(exmin10m4)==F])
  amonex[jjj6+3,"tn90p"]<-length(exmin90m4[exmin90m4>0&is.na(exmin90m4)==F])

  amonex[jjj6+4,"tx10p"]<-length(exmax10m5[exmax10m5<0&is.na(exmax10m5)==F])
  amonex[jjj6+4,"tx90p"]<-length(exmax90m5[exmax90m5>0&is.na(exmax90m5)==F])
  amonex[jjj6+4,"tn10p"]<-length(exmin10m5[exmin10m5<0&is.na(exmin10m5)==F])
  amonex[jjj6+4,"tn90p"]<-length(exmin90m5[exmin90m5>0&is.na(exmin90m5)==F])

  amonex[jjj6+5,"tx10p"]<-length(exmax10m6[exmax10m6<0&is.na(exmax10m6)==F])
  amonex[jjj6+5,"tx90p"]<-length(exmax90m6[exmax90m6>0&is.na(exmax90m6)==F])
  amonex[jjj6+5,"tn10p"]<-length(exmin10m6[exmin10m6<0&is.na(exmin10m6)==F])
  amonex[jjj6+5,"tn90p"]<-length(exmin90m6[exmin90m6>0&is.na(exmin90m6)==F])

  amonex[jjj6+6,"tx10p"]<-length(exmax10m7[exmax10m7<0&is.na(exmax10m7)==F])
  amonex[jjj6+6,"tx90p"]<-length(exmax90m7[exmax90m7>0&is.na(exmax90m7)==F])
  amonex[jjj6+6,"tn10p"]<-length(exmin10m7[exmin10m7<0&is.na(exmin10m7)==F])
  amonex[jjj6+6,"tn90p"]<-length(exmin90m7[exmin90m7>0&is.na(exmin90m7)==F])

  amonex[jjj6+7,"tx10p"]<-length(exmax10m8[exmax10m8<0&is.na(exmax10m8)==F])
  amonex[jjj6+7,"tx90p"]<-length(exmax90m8[exmax90m8>0&is.na(exmax90m8)==F])
  amonex[jjj6+7,"tn10p"]<-length(exmin10m8[exmin10m8<0&is.na(exmin10m8)==F])
  amonex[jjj6+7,"tn90p"]<-length(exmin90m8[exmin90m8>0&is.na(exmin90m8)==F])

  amonex[jjj6+8,"tx10p"]<-length(exmax10m9[exmax10m9<0&is.na(exmax10m9)==F])
  amonex[jjj6+8,"tx90p"]<-length(exmax90m9[exmax90m9>0&is.na(exmax90m9)==F])
  amonex[jjj6+8,"tn10p"]<-length(exmin10m9[exmin10m9<0&is.na(exmin10m9)==F])
  amonex[jjj6+8,"tn90p"]<-length(exmin90m9[exmin90m9>0&is.na(exmin90m9)==F])

  amonex[jjj6+9,"tx10p"]<-length(exmax10m10[exmax10m10<0&is.na(exmax10m10)==F])
  amonex[jjj6+9,"tx90p"]<-length(exmax90m10[exmax90m10>0&is.na(exmax90m10)==F])
  amonex[jjj6+9,"tn10p"]<-length(exmin10m10[exmin10m10<0&is.na(exmin10m10)==F])
  amonex[jjj6+9,"tn90p"]<-length(exmin90m10[exmin90m10>0&is.na(exmin90m10)==F])

  amonex[jjj6+10,"tx10p"]<-length(exmax10m11[exmax10m11<0&is.na(exmax10m11)==F])
  amonex[jjj6+10,"tx90p"]<-length(exmax90m11[exmax90m11>0&is.na(exmax90m11)==F])
  amonex[jjj6+10,"tn10p"]<-length(exmin10m11[exmin10m11<0&is.na(exmin10m11)==F])
  amonex[jjj6+10,"tn90p"]<-length(exmin90m11[exmin90m11>0&is.na(exmin90m11)==F])

  amonex[jjj6+11,"tx10p"]<-length(exmax10m12[exmax10m12<0&is.na(exmax10m12)==F])
  amonex[jjj6+11,"tx90p"]<-length(exmax90m12[exmax90m12>0&is.na(exmax90m12)==F])
  amonex[jjj6+11,"tn10p"]<-length(exmin10m12[exmin10m12<0&is.na(exmin10m12)==F])
  amonex[jjj6+11,"tn90p"]<-length(exmin90m12[exmin90m12>0&is.na(exmin90m12)==F])

   if(leapyear(year)){
     if(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"]>aas[59,"pcmax90"]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"])==F&is.na(aas[59,"pcmax90"])==F)
       amonex[jjj6+1,"tx90p"]<-amonex[jjj6+1,"tx90p"]+1
     if(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"]<aas[59,"pcmax10"]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"])==F&is.na(aas[59,"pcmax10"])==F)
       amonex[jjj6+1,"tx10p"]<-amonex[jjj6+1,"tx10p"]+1
     if(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"]>aas[59,"pcmin90"]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"])==F&is.na(aas[59,"pcmin90"])==F)
       amonex[jjj6+1,"tn90p"]<-amonex[jjj6+1,"tn90p"]+1
     if(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"]<aas[59,"pcmin10"]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"])==F&is.na(aas[59,"pcmin10"])==F)
       amonex[jjj6+1,"tn10p"]<-amonex[jjj6+1,"tn10p"]+1
   }

   jjj6<-jjj6+12
   year=year+1   }
bdm<-merge(bmonex,bd,by="year");assign("bdm",bdm,envir=.GlobalEnv)
adm<-merge(amonex,ad,by="year");assign("adm",adm,envir=.GlobalEnv)

} # end of nordaytem1 function

nordaytem<-function(){
nam1<-paste(nama,"_DAYNOR.csv",sep="")
write.table(daynor,file=nam1,append=F,quote=F,sep=", ",row.names=F)
}

dtr<-function(){# day temperature range(monthly average) 
  len<-yeare-years+1
  aa1<-matrix(NA,12*len,3)
  dimnames(aa1)<-list(NULL,c("year","month","dtr"))
  aa1[,"year"]<-years:yeare
  aa1[,"year"]<-mysort(aa1[,"year"],decreasing=F)
  aa1[,"month"]<-1:12
  temrange<-dd[,"tmax"]-dd[,"tmin"]
  temrange<-cbind(dd[,1:2],temrange)
  jjj1<-1
  for (year in years:yeare){    # start year loop
    temrange1<-temrange[temrange$year==year,]
    temrangem1<-temrange1[temrange1$month==1,"temrange"]
    temrangem2<-temrange1[temrange1$month==2,"temrange"]
    temrangem3<-temrange1[temrange1$month==3,"temrange"]
    temrangem4<-temrange1[temrange1$month==4,"temrange"]
    temrangem5<-temrange1[temrange1$month==5,"temrange"]
    temrangem6<-temrange1[temrange1$month==6,"temrange"]
    temrangem7<-temrange1[temrange1$month==7,"temrange"]
    temrangem8<-temrange1[temrange1$month==8,"temrange"]
    temrangem9<-temrange1[temrange1$month==9,"temrange"]
    temrangem10<-temrange1[temrange1$month==10,"temrange"]
    temrangem11<-temrange1[temrange1$month==11,"temrange"]
    temrangem12<-temrange1[temrange1$month==12,"temrange"]
    aa1[jjj1,3]<-mean(temrangem1,na.rm=T);aa1[jjj1+1,3]<-mean(temrangem2,na.rm=T)
    aa1[jjj1+2,3]<-mean(temrangem3,na.rm=T);aa1[jjj1+3,3]<-mean(temrangem4,na.rm=T)
    aa1[jjj1+4,3]<-mean(temrangem5,na.rm=T);aa1[jjj1+5,3]<-mean(temrangem6,na.rm=T)
    aa1[jjj1+6,3]<-mean(temrangem7,na.rm=T);aa1[jjj1+7,3]<-mean(temrangem8,na.rm=T)
    aa1[jjj1+8,3]<-mean(temrangem9,na.rm=T);aa1[jjj1+9,3]<-mean(temrangem10,na.rm=T)
    aa1[jjj1+10,3]<-mean(temrangem11,na.rm=T);aa1[jjj1+11,3]<-mean(temrangem12,na.rm=T)
    jjj1<-jjj1+12}               #end of year loop

    aa1[,"dtr"]<-aa1[,"dtr"]+nacor[,"mnatma>3"]+nacor[,"mnatmi>3"]
    ofile<-matrix(0,len,14)
    dimnames(ofile)<-list(NULL,c("year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","annual"))
    ofile<-as.data.frame(ofile)
    for(j in years:yeare){
      k<-j-years+1
      ofile[k,1]<-j
      ofile[k,2:13]<-round(aa1[aa1[,"year"]==j,"dtr"],digit=2)
      ofile[k,14]<-round(mean(t(ofile[k,2:13]),na.rm=T),digit=2)
    }
    ofile[,14]<-ofile[,14]+ynacor[,"ynatma>15"]+ynacor[,"ynatmi>15"]
    nam1<-paste(outinddir,paste(ofilename,"_DTR.csv",sep=""),sep="/")
    write.table(ofile,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(ofile[,14]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
      fit1<-lsfit(ofile[,1],ofile[,14])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"dtr",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

    nam2<-paste(outjpgdir,paste(ofilename,"_DTR.jpg",sep=""),sep="/")
    jpeg(nam2,width=1024,height=768)
    plotx(ofile[,1],ofile[,14],main=paste("DTR",ofilename,sep="   "),xlab="Year",ylab="DTR")
    dev.off()
   } # end of dtr

daysprcp10<-function(){
 ys<-yeare-years+1
R10<-rep(0,ys)
yearss<-c(years:yeare)
target<-as.data.frame(cbind(yearss,R10))
for (year in years:yeare){
  mid<-dd[dd$year==year,"prcp"]
  mid<-mid[is.na(mid)==F]
target[target$yearss==year,"R10"]<-length(mid[mid>=10])}
dimnames(target)[[2]][1]<-"year"
 target[,"R10"]<-target[,"R10"]+ynacor[,"ynapr>15"]
nam1<-paste(outinddir,paste(ofilename,"_R10mm.csv",sep=""),sep="/")
write.table(target,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(target[,"R10"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
      fit1<-lsfit(target[,1],target[,"R10"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"r10mm",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

nam2<-paste(outjpgdir,paste(ofilename,"_R10mm.jpg",sep=""),sep="/")
jpeg(nam2,width=1024,height=768)
plotx(target[,1],target[,2],main=paste("R10mm",ofilename,sep="   "),xlab="Year",ylab="R10mm")
dev.off()
}

extremedays<-function(opt=0){
  if(opt==0){
    euu=uu
    eul=ul
    elu=lu
    ell=ll
    }
  else{
    euu=uuu
    eul=uul
    elu=ulu
    ell=ull
  }
  ys<-yeare-years+1
#  beginyear<-dd[1,1]
#  endyear<-dd[dim(dd)[1],1]
  tclext<-c(years:yeare)
  su<-rep(0,ys)
  id<-su
  tr<-su
  fd<-su
  tclext<-cbind(tclext,su,id,tr,fd)
  dimnames(tclext)[[2]][1]<-"year"
  i=1
   for (year in years:yeare) {
    mid1<-dd[dd$year==year,"tmax"];mid1<-mid1[is.na(mid1)==F]
    mid2<-dd[dd$year==year,"tmin"];mid2<-mid2[is.na(mid2)==F]
    tclext[i,"su"]<-length(mid1[mid1>euu])
    tclext[i,"id"]<-length(mid1[mid1<eul])
    tclext[i,"tr"]<-length(mid2[mid2>elu])
    tclext[i,"fd"]<-length(mid2[mid2<ell])
    i<-i+1} #for end    
    tclext<-as.data.frame(tclext)
    tclext[,"su"]<-tclext[,"su"]+ynacor[,"ynatma>15"]
    tclext[,"id"]<-tclext[,"id"]+ynacor[,"ynatma>15"]
    tclext[,"tr"]<-tclext[,"tr"]+ynacor[,"ynatmi>15"]
    tclext[,"fd"]<-tclext[,"fd"]+ynacor[,"ynatmi>15"]
  #    assign("extdays",tclext,envir=.GlobalEnv)
    if(opt==0){
      nam1<-paste(outinddir,paste(ofilename,"_SU25.csv",sep=""),sep="/")
      nam2<-paste(outinddir,paste(ofilename,"_ID0.csv",sep=""),sep="/")
      nam3<-paste(outinddir,paste(ofilename,"_TR20.csv",sep=""),sep="/")
      nam4<-paste(outinddir,paste(ofilename,"_FD0.csv",sep=""),sep="/")
    }
    else{
      nam1<-paste(outinddir,paste(ofilename,"_SU",as.character(euu),".csv",sep=""),sep="/")
      nam2<-paste(outinddir,paste(ofilename,"_ID",as.character(eul),".csv",sep=""),sep="/")
      nam3<-paste(outinddir,paste(ofilename,"_TR",as.character(elu),".csv",sep=""),sep="/")
      nam4<-paste(outinddir,paste(ofilename,"_FD",as.character(ell),".csv",sep=""),sep="/")
    }

    write.table(tclext[,c("year","su")],file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
    write.table(tclext[,c("year","id")],file=nam2,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
    write.table(tclext[,c("year","tr")],file=nam3,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
    write.table(tclext[,c("year","fd")],file=nam4,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

# output trend base on annual indicies data
  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  for( i in c("su","id","tr","fd")){
    if(sum(is.na(tclext[,i]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
    else{
      fit1<-lsfit(tclext[,"year"],tclext[,i])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
    if(opt==0){
      if(i=="su") ii<-"su25"
      if(i=="id") ii<-"id0"
      if(i=="fd") ii<-"fd0"
      if(i=="tr") ii<-"tr20"
      }
    else{
      if(i=="su") ii<-paste("su",as.character(euu),sep="")
      if(i=="id") ii<-paste("id",as.character(eul),sep="")
      if(i=="tr") ii<-paste("tr",as.character(elu),sep="")
      if(i=="fd") ii<-paste("fd",as.character(ell),sep="")
      }
    cat(file=namt,paste(latitude,longitude,ii,years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)
  }
 
  namp<-c("","","","")
  if(opt==0){
    namp[1]<-paste(outjpgdir,paste(ofilename,"_SU25.jpg",sep=""),sep="/")
    namp[2]<-paste(outjpgdir,paste(ofilename,"_ID0.jpg",sep=""),sep="/")
    namp[3]<-paste(outjpgdir,paste(ofilename,"_TR20.jpg",sep=""),sep="/")
    namp[4]<-paste(outjpgdir,paste(ofilename,"_FD0.jpg",sep=""),sep="/")
    }
  else{
    namp[1]<-paste(outjpgdir,paste(ofilename,"_SU",as.character(euu),".jpg",sep=""),sep="/")
    namp[2]<-paste(outjpgdir,paste(ofilename,"_ID",as.character(eul),".jpg",sep=""),sep="/")
    namp[3]<-paste(outjpgdir,paste(ofilename,"_TR",as.character(elu),".jpg",sep=""),sep="/")
    namp[4]<-paste(outjpgdir,paste(ofilename,"_FD",as.character(ell),".jpg",sep=""),sep="/")
    }
  if(opt==0) ylab<-c("SU25","ID0","TR20","FD0")
  else ylab<-c(paste("SU",as.character(euu),sep=""), 
               paste("ID",as.character(eul),sep=""), 
               paste("TR",as.character(elu),sep=""), 
               paste("FD",as.character(ell),sep=""))

  xlab<-rep("year",4)
  for(i in 1:4){
  title1[i]<-paste(ylab[i],ofilename,sep="   ")
    jpeg(file=namp[i],width=1024,height=768)
    plotx(tclext[,1],tclext[,i+1],main=title1[i],ylab=ylab[i],xlab="Year")
    dev.off()
  }
}

exceedance<-function(){
  if (flag==T) return()
  a<-1:365
  ys<-endyear-startyear+1;yss<-ys-1
  mondays<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  mone<-rep(0,12);mons<-mone
  for(i in 1:12) mone[i]<-sum(mondays[1:i])
  mons[1]<-1
  for(i in 2:12) mons[i]<-mone[i-1]+1

  monex<-matrix(NA,ys*12,6)
  dimnames(monex)<-list(NULL,c("year","month","tx10p","tx90p","tn10p","tn90p"))
  monex[,"month"]<-rep(1:12,ys)
  monex[,"year"]<-startyear:endyear
  monex[,"year"]<-mysort(monex[,"year"],decreasing=F)
  monex<-as.data.frame(monex)


  b<-matrix(0,365,4)
  a<-cbind(a,b)
  aa<-array(a,c(365,5,ys))
  dimnames(aa)<-list(NULL,c("day","pcmax10","pcmax90","pcmin10","pcmin90"),NULL)
  ms<-winsize*ys
  i=winsize-round(winsize/2,digits=0)
  i1=round(winsize/2,digits=0)
  
#  daynorm2<-daynorm1[-(1:i1),] # daynorm2 is total base period normalized data
  daynorm2<-dd[dd$year>=startyear,]
  daynorm2<-daynorm2[daynorm2$year<=endyear,]
  daynorm2<-daynorm2[daynorm2$month!=2|daynorm2$day!=29,]
  daynorm2<-daynorm2[,-4]
#  i2<-dim(daynorm2)[1]
#  i3<-i2-i1+1
#  daynorm2<-daynorm2[-(i3:i2),]
  
  yearex<-c(startyear:endyear);txg10p<-rep(0,length(yearex))
  txg90p<-rep(0,length(yearex));tng10p<-rep(0,length(yearex))
  tng90p<-rep(0,length(yearex))
  d<-as.data.frame(cbind(yearex,txg10p,txg90p,tng10p,tng90p))
  colnames(d)[1]<-"year"

  monex<-matrix(0,ys*12,6)
  dimnames(monex)<-list(NULL,c("year","month","tx10p","tx90p","tn10p","tn90p"))
  monex[,"month"]<-rep(1:12,ys)
  monex[,"year"]<-startyear:endyear
  monex[,"year"]<-mysort(monex[,"year"],decreasing=F)
  monex<-as.data.frame(monex)

  ratecount<-matrix(0,365,4)
  dimnames(ratecount)<-list(NULL,c("pcmax10","pcmax90","pcmin10","pcmin90"))

  for (year in startyear:endyear){ # year loop start

    midvalue<-daynorm2[daynorm2$year==year,]
    zz=year-startpoint #index in base period, say, zzth year

    indd<-exwin[exwin[,3]!=ys,3]
    
    for (k in 1:(ys-1)){ # for k (boot strap) start

      for (i in 1:365){ # day loop start
        ppc<-exwins[,,i]
        ppc<-ppc[ppc[,3]!=zz,]
        ppc<-ppc[,-3]
        
        ppc<-cbind(ppc,indd)
        
        ppcc<-rbind(ppc[ppc[,"indd"]==k,],ppc)
	itmp<-percentile(ms,ppcc[,1],c(0.1,0.9))
        aa[i,"pcmax10",zz]<-itmp[1]-1e-5
        aa[i,"pcmax90",zz]<-itmp[2]+1e-5
	itmp<-percentile(ms,ppcc[,2],c(0.1,0.9))
        aa[i,"pcmin10",zz]<-itmp[1]-1e-5
        aa[i,"pcmin90",zz]<-itmp[2]+1e-5
      }
      ratecount[,"pcmax10"]<-midvalue[,"tmax"]-aa[,"pcmax10",zz]
      ratecount[,"pcmax90"]<-midvalue[,"tmax"]-aa[,"pcmax90",zz]
      ratecount[,"pcmin10"]<-midvalue[,"tmin"]-aa[,"pcmin10",zz]
      ratecount[,"pcmin90"]<-midvalue[,"tmin"]-aa[,"pcmin90",zz]
      for(mon in 1:12){
        tmptx10p<-ratecount[mons[mon]:mone[mon],"pcmax10"]
        tmptx90p<-ratecount[mons[mon]:mone[mon],"pcmax90"]
        tmptn10p<-ratecount[mons[mon]:mone[mon],"pcmin10"]
        tmptn90p<-ratecount[mons[mon]:mone[mon],"pcmin90"]
        monex[(zz-1)*12+mon,"tx10p"]<- monex[(zz-1)*12+mon,"tx10p"]+length(tmptx10p[tmptx10p<0&is.na(tmptx10p)==F])
        monex[(zz-1)*12+mon,"tx90p"]<- monex[(zz-1)*12+mon,"tx90p"]+length(tmptx90p[tmptx90p>0&is.na(tmptx90p)==F])
        monex[(zz-1)*12+mon,"tn10p"]<- monex[(zz-1)*12+mon,"tn10p"]+length(tmptn10p[tmptn10p<0&is.na(tmptn10p)==F])
        monex[(zz-1)*12+mon,"tn90p"]<- monex[(zz-1)*12+mon,"tn90p"]+length(tmptn90p[tmptn90p>0&is.na(tmptn90p)==F])
	if(leapyear(year)&mon==2){
	  if(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"]>aa[59,"pcmax90",zz]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"])==F&is.na(aa[58,"pcmax90",zz])==F)
	    monex[(zz-1)*12+mon,"tx90p"]<-monex[(zz-1)*12+mon,"tx90p"]+1
	  if(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"]<aa[59,"pcmax10",zz]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmax"])==F&is.na(aa[58,"pcmax10",zz])==F)
	    monex[(zz-1)*12+mon,"tx10p"]<-monex[(zz-1)*12+mon,"tx10p"]+1
	  if(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"]>aa[59,"pcmin90",zz]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"])==F&is.na(aa[58,"pcmin90",zz])==F)
	    monex[(zz-1)*12+mon,"tn90p"]<-monex[(zz-1)*12+mon,"tn90p"]+1
	  if(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"]<aa[59,"pcmin10",zz]&is.na(dd[dd$year==year&dd$month==2&dd$day==29,"tmin"])==F&is.na(aa[58,"pcmin10",zz])==F)
	    monex[(zz-1)*12+mon,"tn10p"]<-monex[(zz-1)*12+mon,"tn10p"]+1
	} #if end
      } #for mon end
    } #for k (boot strap) end
  }# for year (from startyear to endyear) end

  monex[,"tx10p"]<-monex[,"tx10p"]/(ys-1)
  monex[,"tx90p"]<-monex[,"tx90p"]/(ys-1)
  monex[,"tn10p"]<-monex[,"tn10p"]/(ys-1)
  monex[,"tn90p"]<-monex[,"tn90p"]/(ys-1)
#  monex<-rbind(bdm,monex,adm)

#  assign("dm",dm,envir=.GlobalEnv)

  dm<-merge(monex,d,by="year")
  dm<-rbind(bdm,dm,adm)
  
  len<-yeare-years+1
  for(i in c("tx10p","tx90p","tn10p","tn90p")){
    if (i=="tx10p") {ii<-"_TX10P.csv";kk<-3;nastat=7}#natma
    if (i=="tx90p") {ii<-"_TX90P.csv";kk<-4;nastat=7}#natma
    if (i=="tn10p") {ii<-"_TN10P.csv";kk<-5;nastat=8}#natmi
    if (i=="tn90p") {ii<-"_TN90P.csv";kk<-6;nastat=8}#natmi
  
    nam1<-paste(outinddir,paste(ofilename,ii,sep=""),sep="/")
    ofile<-matrix(0,len,14)
    dimnames(ofile)<-list(NULL,c("year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","annual"))
    ofile<-as.data.frame(ofile)
    for(j in years:yeare){
      if(leapyear(j)) fulldays<-c(31,29,31,30,31,30,31,31,30,31,30,31)
      else fulldays<-c(31,28,31,30,31,30,31,31,30,31,30,31)
      k<-j-years+1
      ofile[k,1]<-j
      ofile[k,2:13]<-dm[dm$year==j,kk]
      for(mon in 1:12){
      if(nastatistic[(k-1)*12+mon,nastat]>10) ofile[k,(mon+1)]<-NA
      else   ofile[k,(mon+1)]<-dm[(k-1)*12+mon,kk]*fulldays[mon]/(fulldays[mon]-nastatistic[(k-1)*12+mon,nastat])
    }
    ofile[k,14]<-sum(ofile[k,2:13],na.rm=T)
    }
    ofile[,14]<-ofile[,14]+ynacor[,nastat-4]
    for(j in years:yeare){
      k<-j-years+1
      if(leapyear(j)) fulldays<-c(31,29,31,30,31,30,31,31,30,31,30,31)
      else fulldays<-c(31,28,31,30,31,30,31,31,30,31,30,31)
      for(mon in 1:12) ofile[k,mon+1]<-ofile[k,mon+1]*100/fulldays[mon] # change output from counting days to %
    }
    ofile[,14]<-ofile[,14]*100/365 # change output from counting days to %
    write.table(round(ofile,2),file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
    
    namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
    if(sum(is.na(ofile[,14]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
    else{
      fit1<-lsfit(ofile[,1],ofile[,14])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
    cat(file=namt,paste(latitude,longitude,i,years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

    nam2<-paste(outjpgdir,paste(ofilename,"_",toupper(i),".jpg",sep=""),sep="/")
    jpeg(file=nam2,width=1024,height=768)
    plotx(ofile[,1],ofile[,14],main=paste(toupper(i),ofilename,sep="   "),ylab=toupper(i),xlab="Year")
    dev.off()
  }
}

index641cdd<-function(){
ys<-yeare-years+1
cdd<-rep(0,ys)
year<-c(years:yeare)
target<-as.data.frame(cbind(year,cdd))
year=years
for (i in 1:ys){
  mid<-dd[dd$year==year,"prcp"]
#  mid<-mid[is.na(mid)==F]
  if(i==1) kk<-0
  mm<-0
  for(j in 1:length(mid)){
    if(mid[j]<1&is.na(mid[j])==F) kk<-kk+1
    else {
      if(mm<kk) mm<-kk
      kk<-0
    }
  }
  if(mm<kk){
    if(year==yeare) mm<-kk
    else
      if(dd[dd$year==year+1&dd$month==1&dd$day==1,"prcp"]>=1|is.na(dd[dd$year==year+1&dd$month==1&dd$day==1,"prcp"])==T) mm<-kk
# in case whole year dry, the next year will have a CDD bigger than 365
# then the CDD indice for current year should not be 0 but NA
    if(mm==0) mm<-NA
  }
  target[i,"cdd"]<-mm
  year=year+1
}

#for(i in 1:(ys-1))
#  if(target[i,"cdd"]==0&target[i+1,"cdd"]>=365) target[i,"cdd"]<-NA

target[,"cdd"]<-target[,"cdd"]+ynacor[,"ynapr>15"]
nam1<-paste(outinddir,paste(ofilename,"_CDD.csv",sep=""),sep="/")
write.table(target,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(target[,"cdd"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
      fit1<-lsfit(target[,1],target[,"cdd"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"cdd",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

nam2<-paste(outjpgdir,paste(ofilename,"_CDD.jpg",sep=""),sep="/")
jpeg(nam2,width=1024,height=768)
plotx(target[,1],target[,2],main=paste("CDD",ofilename,sep="   "),xlab="Year",ylab="CDD")
dev.off()
}

index641cwd<-function(){
ys<-yeare-years+1
cwd<-rep(0,ys)
year<-years:yeare
target<-as.data.frame(cbind(year,cwd))
year=years
for (i in 1:ys){
  mid<-dd[dd$year==year,"prcp"]
#  mid<-mid[is.na(mid)==F]
  if(i==1) kk<-0
  mm<-0
  for(j in 1:length(mid)){
    if(mid[j]>=1&is.na(mid[j])==F) kk<-kk+1
    else {
      if(mm<kk) mm<-kk
      kk<-0
    }
  }
  if(mm<kk){
    if(year==yeare) mm<-kk
    else
      if(dd[dd$year==year+1&dd$month==1&dd$day==1,"prcp"]<1|is.na(dd[dd$year==year+1&dd$month==1&dd$day==1,"prcp"])==T) mm<-kk
  }
  
  target[i,"cwd"]<-mm
  year=year+1
}
target[,"cwd"]<-target[,"cwd"]+ynacor[,"ynapr>15"]
nam1<-paste(outinddir,paste(ofilename,"_CWD.csv",sep=""),sep="/")
write.table(target,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(target[,"cwd"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
    fit1<-lsfit(target[,1],target[,"cwd"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"cwd",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

nam2<-paste(outjpgdir,paste(ofilename,"_CWD.jpg",sep=""),sep="/")
jpeg(nam2,width=1024,height=768)
plotx(target[,1],target[,2],main=paste("CWD",ofilename,sep="   "),xlab="Year",ylab="CWD")
dev.off()
}

rx1d<-function(){
  len<-yeare-years+1
  aa1<-matrix(NA,12*len,3)
  dimnames(aa1)<-list(NULL,c("year","month","rx1d"))
  aa1[,"year"]<-years:yeare
  aa1[,"year"]<-mysort(aa1[,"year"],decreasing=F)
  aa1[,"month"]<-1:12
  jjj3=1
  mid<-dd[,1:4]
  for (year in years:yeare){
    aaaa<-mid[mid$year==year,]
    aaaam1<-aaaa[aaaa$month==1,"prcp"];aaaam2<-aaaa[aaaa$month==2,"prcp"]
    aaaam3<-aaaa[aaaa$month==3,"prcp"];aaaam4<-aaaa[aaaa$month==4,"prcp"]
    aaaam5<-aaaa[aaaa$month==5,"prcp"];aaaam6<-aaaa[aaaa$month==6,"prcp"]
    aaaam7<-aaaa[aaaa$month==7,"prcp"];aaaam8<-aaaa[aaaa$month==8,"prcp"]
    aaaam9<-aaaa[aaaa$month==9,"prcp"];aaaam10<-aaaa[aaaa$month==10,"prcp"]
    aaaam11<-aaaa[aaaa$month==11,"prcp"];aaaam12<-aaaa[aaaa$month==12,"prcp"]
    aa1[jjj3,"rx1d"]<-max(aaaam1,na.rm=T);aa1[jjj3+1,"rx1d"]<-max(aaaam2,na.rm=T)
    aa1[jjj3+2,"rx1d"]<-max(aaaam3,na.rm=T);aa1[jjj3+3,"rx1d"]<-max(aaaam4,na.rm=T)
    aa1[jjj3+4,"rx1d"]<-max(aaaam5,na.rm=T);aa1[jjj3+5,"rx1d"]<-max(aaaam6,na.rm=T)
    aa1[jjj3+6,"rx1d"]<-max(aaaam7,na.rm=T);aa1[jjj3+7,"rx1d"]<-max(aaaam8,na.rm=T)
    aa1[jjj3+8,"rx1d"]<-max(aaaam9,na.rm=T);aa1[jjj3+9,"rx1d"]<-max(aaaam10,na.rm=T)
    aa1[jjj3+10,"rx1d"]<-max(aaaam11,na.rm=T);aa1[jjj3+11,"rx1d"]<-max(aaaam12,na.rm=T)
    jjj3=jjj3+12}
    aa1[,"rx1d"]<-aa1[,"rx1d"]+nacor[,"mnapr>3"]
    ofile<-matrix(0,len,14)
#    aa1<-as.data.frame(aa1)
    dimnames(ofile)<-list(NULL,c("year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","annual"))
    ofile<-as.data.frame(ofile)
    for(j in years:yeare){
      k<-j-years+1
      ofile[k,1]<-j
      ofile[k,2:13]<-aa1[aa1[,1]==j,3]
      ofile[k,14]<-max(ofile[k,2:13],na.rm=F)
    }
  nam1<-paste(outinddir,paste(ofilename,"_RX1day.csv",sep=""),sep="/")
  write.table(ofile,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(ofile[,14]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
      fit1<-lsfit(ofile[,1],ofile[,14])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"rx1day",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

  nam2<-paste(outjpgdir,paste(ofilename,"_RX1day.jpg",sep=""),sep="/")
  jpeg(nam2,width=1024,height=768)
  plotx(ofile[,1],ofile[,14],main=paste("RX1day",ofilename,sep="   "),xlab="Year",ylab="RX1day")
  dev.off()
}# end of rx1d()

rx5d<-function(){
    a2<-c(0,0,0,0)
    a1<-dd[,"prcp"]
    a1<-append(a2,a1)
    n<-length(a1)
    a<-rep(0,n)
  for (i in 5:n){
  a[i]<-sum(a1[(i-4):i],na.rm=T)}
  a<-a[-(1:4)]
  a<-cbind(dd[,1:2],a)
    
  len<-yeare-years+1
  aa1<-matrix(NA,12*len,3)
  dimnames(aa1)<-list(NULL,c("year","month","rx5d"))
  aa1[,"year"]<-years:yeare
  aa1[,"year"]<-mysort(aa1[,"year"],decreasing=F)
  aa1[,"month"]<-1:12
  jjj2=1
  for (year in years:yeare){
    aaaa<-a[a$year==year,]
    aaaam1<-aaaa[aaaa$month==1,"a"];aaaam2<-aaaa[aaaa$month==2,"a"]
    aaaam3<-aaaa[aaaa$month==3,"a"];aaaam4<-aaaa[aaaa$month==4,"a"]
    aaaam5<-aaaa[aaaa$month==5,"a"];aaaam6<-aaaa[aaaa$month==6,"a"]
    aaaam7<-aaaa[aaaa$month==7,"a"];aaaam8<-aaaa[aaaa$month==8,"a"]
    aaaam9<-aaaa[aaaa$month==9,"a"];aaaam10<-aaaa[aaaa$month==10,"a"]
    aaaam11<-aaaa[aaaa$month==11,"a"];aaaam12<-aaaa[aaaa$month==12,"a"]
    aa1[jjj2,"rx5d"]<-max(aaaam1,na.rm=T);aa1[jjj2+1,"rx5d"]<-max(aaaam2,na.rm=T)
    aa1[jjj2+2,"rx5d"]<-max(aaaam3,na.rm=T);aa1[jjj2+3,"rx5d"]<-max(aaaam4,na.rm=T)
    aa1[jjj2+4,"rx5d"]<-max(aaaam5,na.rm=T);aa1[jjj2+5,"rx5d"]<-max(aaaam6,na.rm=T)
    aa1[jjj2+6,"rx5d"]<-max(aaaam7,na.rm=T);aa1[jjj2+7,"rx5d"]<-max(aaaam8,na.rm=T)
    aa1[jjj2+8,"rx5d"]<-max(aaaam9,na.rm=T);aa1[jjj2+9,"rx5d"]<-max(aaaam10,na.rm=T)
    aa1[jjj2+10,"rx5d"]<-max(aaaam11,na.rm=T);aa1[jjj2+11,"rx5d"]<-max(aaaam12,na.rm=T)
    jjj2=jjj2+12}

    aa1[,"rx5d"]<-aa1[,"rx5d"]+nacor[,"mnapr>3"]
    
    ofile<-matrix(0,len,14)
#    aa1<-as.data.frame(aa1)
    dimnames(ofile)<-list(NULL,c("year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","annual"))
    ofile<-as.data.frame(ofile)
    for(j in years:yeare){
      k<-j-years+1
      ofile[k,1]<-j
      ofile[k,2:13]<-aa1[aa1[,1]==j,"rx5d"]
      ofile[k,14]<-max(ofile[k,2:13],na.rm=F)
    }
#    print(dim(rx5d))
  nam1<-paste(outinddir,paste(ofilename,"_RX5day.csv",sep=""),sep="/")
  write.table(ofile,file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

    namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(ofile[,14]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
      fit1<-lsfit(ofile[,1],ofile[,14])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"rx5day",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

  nam2<-paste(outjpgdir,paste(ofilename,"_RX5day.jpg",sep=""),sep="/")
  jpeg(nam2,width=1024,height=768)
  plotx(ofile[,1],ofile[,14],main=paste("RX5day",ofilename,sep="   "),xlab="Year",ylab="RX5day")
  dev.off()
  }# end of rx5d()

extremedaytem<-function(){
  len<-yeare-years+1
  aa1<-matrix(NA,12*len,6)
  dimnames(aa1)<-list(NULL,c("year","month","txx","txn","tnx","tnn"))
  aa1[,"year"]<-years:yeare
  aa1[,"year"]<-mysort(aa1[,"year"],decreasing=F)
  aa1[,"month"]<-1:12
  jjj4=1
  for (year in years:yeare){
    aaaa<-dd[dd$year==year,]
    aaaama1<-aaaa[aaaa$month==1,"tmax"];aaaama2<-aaaa[aaaa$month==2,"tmax"]
    aaaama3<-aaaa[aaaa$month==3,"tmax"];aaaama4<-aaaa[aaaa$month==4,"tmax"]
    aaaama5<-aaaa[aaaa$month==5,"tmax"];aaaama6<-aaaa[aaaa$month==6,"tmax"]
    aaaama7<-aaaa[aaaa$month==7,"tmax"];aaaama8<-aaaa[aaaa$month==8,"tmax"]
    aaaama9<-aaaa[aaaa$month==9,"tmax"];aaaama10<-aaaa[aaaa$month==10,"tmax"]
    aaaama11<-aaaa[aaaa$month==11,"tmax"];aaaama12<-aaaa[aaaa$month==12,"tmax"]
    
    aaaami1<-aaaa[aaaa$month==1,"tmin"];aaaami2<-aaaa[aaaa$month==2,"tmin"]
    aaaami3<-aaaa[aaaa$month==3,"tmin"];aaaami4<-aaaa[aaaa$month==4,"tmin"]
    aaaami5<-aaaa[aaaa$month==5,"tmin"];aaaami6<-aaaa[aaaa$month==6,"tmin"]
    aaaami7<-aaaa[aaaa$month==7,"tmin"];aaaami8<-aaaa[aaaa$month==8,"tmin"]
    aaaami9<-aaaa[aaaa$month==9,"tmin"];aaaami10<-aaaa[aaaa$month==10,"tmin"]
    aaaami11<-aaaa[aaaa$month==11,"tmin"];aaaami12<-aaaa[aaaa$month==12,"tmin"]
    
    aa1[jjj4,"txx"]<-max(aaaama1,na.rm=T);aa1[jjj4+1,"txx"]<-max(aaaama2,na.rm=T)
    aa1[jjj4+2,"txx"]<-max(aaaama3,na.rm=T);aa1[jjj4+3,"txx"]<-max(aaaama4,na.rm=T)
    aa1[jjj4+4,"txx"]<-max(aaaama5,na.rm=T);aa1[jjj4+5,"txx"]<-max(aaaama6,na.rm=T)
    aa1[jjj4+6,"txx"]<-max(aaaama7,na.rm=T);aa1[jjj4+7,"txx"]<-max(aaaama8,na.rm=T)
    aa1[jjj4+8,"txx"]<-max(aaaama9,na.rm=T);aa1[jjj4+9,"txx"]<-max(aaaama10,na.rm=T)
    aa1[jjj4+10,"txx"]<-max(aaaama11,na.rm=T);aa1[jjj4+11,"txx"]<-max(aaaama12,na.rm=T)
    
    aa1[jjj4,"txn"]<-min(aaaama1,na.rm=T);aa1[jjj4+1,"txn"]<-min(aaaama2,na.rm=T)
    aa1[jjj4+2,"txn"]<-min(aaaama3,na.rm=T);aa1[jjj4+3,"txn"]<-min(aaaama4,na.rm=T)
    aa1[jjj4+4,"txn"]<-min(aaaama5,na.rm=T);aa1[jjj4+5,"txn"]<-min(aaaama6,na.rm=T)
    aa1[jjj4+6,"txn"]<-min(aaaama7,na.rm=T);aa1[jjj4+7,"txn"]<-min(aaaama8,na.rm=T)
    aa1[jjj4+8,"txn"]<-min(aaaama9,na.rm=T);aa1[jjj4+9,"txn"]<-min(aaaama10,na.rm=T)
    aa1[jjj4+10,"txn"]<-min(aaaama11,na.rm=T);aa1[jjj4+11,"txn"]<-min(aaaama12,na.rm=T)

    aa1[jjj4,"tnx"]<-max(aaaami1,na.rm=T);aa1[jjj4+1,"tnx"]<-max(aaaami2,na.rm=T)
    aa1[jjj4+2,"tnx"]<-max(aaaami3,na.rm=T);aa1[jjj4+3,"tnx"]<-max(aaaami4,na.rm=T)
    aa1[jjj4+4,"tnx"]<-max(aaaami5,na.rm=T);aa1[jjj4+5,"tnx"]<-max(aaaami6,na.rm=T)
    aa1[jjj4+6,"tnx"]<-max(aaaami7,na.rm=T);aa1[jjj4+7,"tnx"]<-max(aaaami8,na.rm=T)
    aa1[jjj4+8,"tnx"]<-max(aaaami9,na.rm=T);aa1[jjj4+9,"tnx"]<-max(aaaami10,na.rm=T)
    aa1[jjj4+10,"tnx"]<-max(aaaami11,na.rm=T);aa1[jjj4+11,"tnx"]<-max(aaaami12,na.rm=T)

    aa1[jjj4,"tnn"]<-min(aaaami1);aa1[jjj4+1,"tnn"]<-min(aaaami2,na.rm=T)
    aa1[jjj4+2,"tnn"]<-min(aaaami3,na.rm=T);aa1[jjj4+3,"tnn"]<-min(aaaami4,na.rm=T)
    aa1[jjj4+4,"tnn"]<-min(aaaami5,na.rm=T);aa1[jjj4+5,"tnn"]<-min(aaaami6,na.rm=T)
    aa1[jjj4+6,"tnn"]<-min(aaaami7,na.rm=T);aa1[jjj4+7,"tnn"]<-min(aaaami8,na.rm=T)
    aa1[jjj4+8,"tnn"]<-min(aaaami9,na.rm=T);aa1[jjj4+9,"tnn"]<-min(aaaami10,na.rm=T)
    aa1[jjj4+10,"tnn"]<-min(aaaami11,na.rm=T);aa1[jjj4+11,"tnn"]<-min(aaaami12,na.rm=T)
    
    jjj4=jjj4+12}
    exdaytem<-as.data.frame(aa1)
#    midnacor<-nacor[nacor$year>=startyear,]
#    midnacor<-midnacor[midnacor$year<=endyear,]
    exdaytem[,"txx"]<-exdaytem[,"txx"]+nacor[,"mnatma>3"]
    exdaytem[,"txn"]<-exdaytem[,"txn"]+nacor[,"mnatma>3"]
    exdaytem[,"tnx"]<-exdaytem[,"tnx"]+nacor[,"mnatmi>3"]
    exdaytem[,"tnn"]<-exdaytem[,"tnn"]+nacor[,"mnatmi>3"]

  for(i in c("txx","txn","tnx","tnn")){
    if (i=="txx") {ii<-"_TXx.csv";ij<-"_TXx.jpg";kk<-3;ik<-1;ki<-3}# ik=1, take max as yearly record
    if (i=="txn") {ii<-"_TXn.csv";ij<-"_TXn.jpg";kk<-4;ik<-0;ki<-3}# ik=0, take min as yearly record
    if (i=="tnx") {ii<-"_TNx.csv";ij<-"_TNx.jpg";kk<-5;ik<-1;ki<-4}# ki=3, take TMAX annual missing values
    if (i=="tnn") {ii<-"_TNn.csv";ij<-"_TNn.jpg";kk<-6;ik<-0;ki<-4}# ki=4, take TMIN annual missing values
    nam<-paste(outinddir,paste(ofilename,ii,sep=""),sep="/")
    ofile<-matrix(0,len,14)
#    ojpg<-rep(0,len)
    dimnames(ofile)<-list(NULL,c("year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","annual"))
    ofile<-as.data.frame(ofile)
    for(j in years:yeare){
      k<-j-years+1
      ofile[k,1]<-j
      ofile[k,2:13]<-exdaytem[exdaytem$year==j,kk]
    if(ik==1) ofile[k,14]<-max(ofile[k,2:13],na.rm=T)
    else ofile[k,14]<-min(ofile[k,2:13],na.rm=T)
    }
    ofile[,14]<-ofile[,14]+ynacor[,ki]
    write.table(ofile,file=nam,append=F,quote=F,sep=", ",na="-99.9",row.names=F)
    
    namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
    if(sum(is.na(ofile[,14]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
    else{
    fit1<-lsfit(ofile[,1],ofile[,14])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
    cat(file=namt,paste(latitude,longitude,i,years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

    nam2<-paste(outjpgdir,paste(ofilename,ij,sep=""),sep="/")
    jpeg(nam2,width=1024,height=768)
    plotx(ofile[,1],ofile[,14],main=paste(toupper(i),ofilename,sep="   "),xlab="Year",ylab=(paste(toupper(substr(i,1,2)),substr(i,3,3),sep="")))
    dev.off()
  }
}

index646<-function(){
  ys=yeare-years+1
   b<-matrix(0,ncol=2,nrow=ys)
   dimnames(b)<-list(NULL,c("year","sdii"))
   b[,"year"]<-c(years:yeare)
   b<-as.data.frame(b)
   year=years
 for (i in 1:ys){
   mid<-dd[dd$year==year,"prcp"]
   mid<-mid[mid>=1]
   b[i,"sdii"]<-mean(mid,na.rm=T)
  year=year+1  }
  b[,"sdii"]<-b[,"sdii"]+ynacor[,"ynapr>15"]
 nam1<-paste(outinddir,paste(ofilename,"_SDII.csv",sep=""),sep="/")
 write.table(round(b,digit=1),file=nam1,append=F,quote=F,sep=", ",na="-99.9",row.names=F)

  namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
  if(sum(is.na(b[,"sdii"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
  else{
      fit1<-lsfit(b[,"year"],b[,"sdii"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
  cat(file=namt,paste(latitude,longitude,"sdii",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

 nam2<-paste(outjpgdir,paste(ofilename,"_SDII.jpg",sep=""),sep="/")
 jpeg(nam2,width=1024,height=768)
 plotx(b[,1],b[,2],main=paste("SDII",ofilename,sep="   "),xlab="Year",ylab="SDII")
 dev.off()
}

main1<-function(){
        main<-tktoplevel()
        tkfocus(main)
        tkwm.title(main,"Calculating Climate Indices")
        tkgrid(tklabel(main,text="Check desired indices",font=fontHeading1))
	txt="It may take 5 minutes to compute all the indices, "
#	if(prallna==1) tkgrid(tklabel(main, text="PRCP all missing, indices related to PRCP may not be calculated",font=fontHeading2))
#	if(txallna==1|tnallna==1) tkgrid(tklabel(main,text="TMAX or TMIN all missing, indices related to TMAX and TMIN may not be calculated",font=fontHeading2))

#cb0 <- tkcheckbutton(main);cb0Val <- cbvalue0

cb1 <- tkcheckbutton(main);cb1Val <- cbvalue1
cb2 <- tkcheckbutton(main);cb2Val <- cbvalue2
cb3 <- tkcheckbutton(main);cb3Val <- cbvalue3
cb4 <- tkcheckbutton(main);cb4Val <- cbvalue4
cb5 <- tkcheckbutton(main);cb5Val <- cbvalue5
cb6 <- tkcheckbutton(main);cb6Val <- cbvalue6
cb7 <- tkcheckbutton(main);cb7Val <- cbvalue7
cb8 <- tkcheckbutton(main);cb8Val <- cbvalue8
cb9 <- tkcheckbutton(main);cb9Val <- cbvalue9
cb10 <- tkcheckbutton(main);cb10Val <- cbvalue10
cb11 <- tkcheckbutton(main);cb11Val <- cbvalue11
cb12 <- tkcheckbutton(main);cb12Val <- cbvalue12
cb13 <- tkcheckbutton(main);cb13Val <- cbvalue13
cb14 <- tkcheckbutton(main);cb14Val <- cbvalue14
cb15 <- tkcheckbutton(main);cb15Val <- cbvalue15
cb21 <- tkcheckbutton(main);cb21Val <- cbvalue21
      
#tkconfigure(cb0,variable=cb0Val)#,value=cb1Val)
tkconfigure(cb1,variable=cb1Val)#,value=cb1Val)
tkconfigure(cb2,variable=cb2Val)#,value=cb2Val)
tkconfigure(cb3,variable=cb3Val)#,value=cb3Val)#"dtr")
tkconfigure(cb4,variable=cb4Val)#,value=cb4Val)#"daysprcp10")
tkconfigure(cb5,variable=cb5Val)#,value=cb5Val)#"nordaytem1")
tkconfigure(cb6,variable=cb6Val)#,value=cb6Val)#"extremedays")
tkconfigure(cb7,variable=cb7Val)#,value=cb7Val)#"exceedance")
tkconfigure(cb8,variable=cb8Val)#,value=cb8Val)#"ind144hwd")
tkconfigure(cb9,variable=cb9Val)#,value=cb9Val)#"ind641cdd")
tkconfigure(cb10,variable=cb10Val)#,value=cb10Val)#"rx5d")
tkconfigure(cb11,variable=cb11Val)#,value=cb11Val)#"ind646")
tkconfigure(cb12,variable=cb12Val)#,value=cb12Val)#"ind695")
tkconfigure(cb13,variable=cb13Val)#,value=cb13Val)#"rx1d"
tkconfigure(cb14,variable=cb14Val)#,value=cb14Val)#"extreme day tem"
tkconfigure(cb15,variable=cb15Val)
tkconfigure(cb21,variable=cb21Val)
      
#tkgrid(tklabel(main,text="Select the indices you want to calculate:"))

#tkgrid(tklabel(main,text="ALL 26 indices!!"),cb0)
tkgrid(tklabel(main,text="SU25, FD0, TR20, ID0"),cb1)
tkgrid(tklabel(main,text="User Defined SU, FD, TR, ID"),cb21)

tkgrid(tklabel(main,text="GSL, growing season length"),cb2)#143

tkgrid(tklabel(main,text="TXx, TXn, TNx, TNn"),cb3)
#tkgrid(tklabel(main,text="TXn, TNx, TNn, following by same choice"),cb3)#extremedaytem

tkgrid(tklabel(main,text="TX10p, TX90p, TN10p, TN90p"),cb4)
#tkgrid(tklabel(main,text="TX90p, TN10p, TN90p, following by same choice"),cb4)#exceedance

tkgrid(tklabel(main,text="WSDI"),cb5)#hwfi
tkgrid(tklabel(main,text="CSDI"),cb6)#cwdi
      
#tkgrid(tklabel(main,text="Normal day temperature with user defined window size"),cb2)
tkgrid(tklabel(main,text="DTR"),cb7)#dtr
      
tkgrid(tklabel(main,text="Rx1day"),cb8)#rx1d
tkgrid(tklabel(main,text="Rx5day"),cb9)#rx5d
      
tkgrid(tklabel(main,text="SDII"),cb10)#index646

tkgrid(tklabel(main,text="R10mm"),cb11)#daysprcp10()
tkgrid(tklabel(main,text="R20mm"),cb12)#daysprcp20()
tkgrid(tklabel(main,text="Rnnmm"),cb13)#daysprcpn()

tkgrid(tklabel(main,text="CDD, CWD"),cb14)
#tkgrid(tklabel(main,text="CWD"),cb14)#641

#tkgrid(tklabel(main,text="daynortem     "),cb5)
#tkgrid(tklabel(main,text="ind144hwd      "),cb8)#144
      
tkgrid(tklabel(main,text="R95p, R99p, PRCPTOT"),cb15)#695
#tkgrid(tklabel(main,text="R99pTOT"),cb15)
      
#tkgrid(tklabel(main,text="Annual Days with PRCP>=95 percentile"),cb17)#r95ptot
#tkgrid(tklabel(main,text="Annual Days with PRCP>=99 percentile"),cb17)

      OnOK <- function(){
#filename<-tclvalue(tkgetSaveFile(filetypes="{{EXCEL Files} {.csv}} {{All files} *}"))
#if (!nchar(filename))
#tkmessageBox(message="No file was selected!")
#else tkmessageBox(message=paste("The results will be saved under",filename))
#nam<-substr(filename,start=1,stop=(nchar(filename)-4))
#assign("nam",nam,envir=.GlobalEnv)

#    cbv0 <- as.character(tclvalue(cb0Val))
    cbv1 <- as.character(tclvalue(cb1Val))
    cbv21 <- as.character(tclvalue(cb21Val))
    cbv2 <- as.character(tclvalue(cb2Val))
    cbv3 <- as.character(tclvalue(cb3Val))
    cbv4 <- as.character(tclvalue(cb4Val))
    cbv5 <- as.character(tclvalue(cb5Val))
    cbv6 <- as.character(tclvalue(cb6Val))
    cbv7 <- as.character(tclvalue(cb7Val))
    cbv8 <- as.character(tclvalue(cb8Val))
    cbv9 <- as.character(tclvalue(cb9Val))
    cbv10 <- as.character(tclvalue(cb10Val))
    cbv11 <- as.character(tclvalue(cb11Val))
    cbv12 <- as.character(tclvalue(cb12Val))
    cbv13 <- as.character(tclvalue(cb13Val))
    cbv14 <- as.character(tclvalue(cb14Val))
    cbv15 <- as.character(tclvalue(cb15Val))
    tkdestroy(main)
#    if (cbv0==1) {cbv1<-1;cbv2<-1;cbv3<-1;cbv4<-1;cbv5<-1;
    namt<-paste(outtrddir,paste(ofilename,"_trend.csv",sep=""),sep="/")
    cat(file=namt,paste("Lat","Lon","Indices","SYear","EYear","Slope","STD_of_Slope","P_Value",sep=","),fill=180,append=F)
    if(sum(is.na(aa2[,"tmaxm"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
    else{
      fit1<-lsfit(years:yeare,aa2[,"tmaxm"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
    cat(file=namt,paste(latitude,longitude,"TMAXmean",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

    if(sum(is.na(aa2[,"tminm"]))>=(yeare-years+1-10)){
      betahat<-NA
      betastd<-NA
      pvalue<-NA
    }
    else{
      fit1<-lsfit(years:yeare,aa2[,"tminm"])
      out1<-ls.print(fit1,print.it=F)
      pvalue<-round(as.numeric(out1$summary[1,6]),3)
      betahat<-round(as.numeric(out1$coef.table[[1]][2,1]),3)
      betastd<-round(as.numeric(out1$coef.table[[1]][2,2]),3)
    }
    cat(file=namt,paste(latitude,longitude,"TMINmean",years,yeare,betahat,betastd,pvalue,sep=","),fill=180,append=T)

    if (cbv1==1) if(txallna==0&tnallna==0) extremedays()
    if (cbv21==1) if(txallna==0&tnallna==0) extremedays(opt=1)
    if (cbv2==1) if(txallna==0&tnallna==0) ind143gsl()
    if (cbv3==1) if(txallna==0&tnallna==0) extremedaytem()
    if (cbv4==1) if(txallna==0&tnallna==0) exceedance()
    if (cbv5==1) if(txallna==0) hwfi()
    if (cbv6==1) if(tnallna==0) cwdi()
    if (cbv7==1) if(txallna==0&tnallna==0) dtr()
    if (cbv8==1) if(prallna==0) rx1d()
    if (cbv9==1) if(prallna==0) rx5d()
    if (cbv10==1) if(prallna==0) index646()
    if (cbv11==1) if(prallna==0) daysprcp10()
    if (cbv12==1) if(prallna==0) daysprcp20()
    if (cbv13==1) if(prallna==0) daysprcpn()
    if (cbv14==1) if(prallna==0) {index641cdd();index641cwd()}
    if (cbv15==1) if(prallna==0) r95ptot()
    cbvalue1<-cb1Val;assign("cbvalue1",cbvalue1,envir=.GlobalEnv)
    cbvalue2<-cb2Val;assign("cbvalue2",cbvalue2,envir=.GlobalEnv)
    cbvalue3<-cb3Val;assign("cbvalue3",cbvalue3,envir=.GlobalEnv)
    cbvalue4<-cb4Val;assign("cbvalue4",cbvalue4,envir=.GlobalEnv)
    cbvalue5<-cb5Val;assign("cbvalue5",cbvalue5,envir=.GlobalEnv)
    cbvalue6<-cb6Val;assign("cbvalue6",cbvalue6,envir=.GlobalEnv)
    cbvalue7<-cb7Val;assign("cbvalue7",cbvalue7,envir=.GlobalEnv)
    cbvalue8<-cb8Val;assign("cbvalue8",cbvalue8,envir=.GlobalEnv)
    cbvalue9<-cb9Val;assign("cbvalue9",cbvalue9,envir=.GlobalEnv)
    cbvalue10<-cb10Val;assign("cbvalue10",cbvalue10,envir=.GlobalEnv)
    cbvalue11<-cb11Val;assign("cbvalue11",cbvalue11,envir=.GlobalEnv)
    cbvalue12<-cb12Val;assign("cbvalue12",cbvalue12,envir=.GlobalEnv)
    cbvalue13<-cb13Val;assign("cbvalue13",cbvalue13,envir=.GlobalEnv)
    cbvalue14<-cb14Val;assign("cbvalue14",cbvalue14,envir=.GlobalEnv)
    cbvalue15<-cb15Val;assign("cbvalue15",cbvalue15,envir=.GlobalEnv)
    cbvalue21<-cb21Val;assign("cbvalue21",cbvalue21,envir=.GlobalEnv)
    nstation<-tktoplevel()
    tkwm.title(nstation,"Calculation Done")
    tkfocus(nstation)
    okk<-function(){tkdestroy(nstation);tkfocus(start1)}
    textlabel0<-tklabel(nstation,text="     ")
    textlabel1<-tklabel(nstation,text="Indices calculation completed",font=fontHeading1)
    textlabel2<-tklabel(nstation,text=paste("Plots are in: ",outjpgdir,sep=" "),font=fontHeading1)
    okk.but<-tkbutton(nstation,text="   OK   ",command=okk,width=20)
    tkgrid(textlabel0)
    tkgrid(textlabel1)
    tkgrid(textlabel2)
    tkgrid.configure(textlabel1,sticky="w")
    tkgrid.configure(textlabel2,sticky="w")
    tkgrid.configure(textlabel0,sticky="e")
#    cancell.but<-tkbutton(nstation,text="   NO   ",command=cancell,width=15)
#    tkgrid(textlabel2,okk.but,cancell.but,textlabel0)
    tkgrid(okk.but,textlabel0)
#    tkgrid.configure(cancell.but,sticky="w")
    tkgrid(textlabel0)
  }
  done2<-function(){
  tkdestroy(main)
  tkfocus(start1)
#  return()
 }

    ok.but <-tkbutton(main,text="   OK   ",command=OnOK,width=30)
    cancel.but<-tkbutton(main,text="CANCEL",command=done2,width=30)
    tkgrid(ok.but)
    tkgrid(cancel.but)
    tkgrid(tklabel(main,text="It may take more than 5 minutes to compute all the indices",font=fontHeading2))
    tkgrid(tklabel(main,text="Please be patient, you will be informed once computations are done",font=fontHeading2))
    tkgrid(tklabel(main,text="",font=fontHeading))#empty line
    
      }




# End of Part II ( Functions of Calculating climate indecies )

# Part III
# Main program
# call getfile, read data file and store in dd.
start1<-tktoplevel()

plotx<-
function (x,y,main="",xlab="",ylab="")
{
op<-par(no.readonly=T)
#par(font.axis=2, font.lab=2, font.main=2, font.sub=2)
par(cex=2,mgp=c(2.5,.5,0))
plot(x,y,xlab=xlab,ylab=ylab,type="b")
fit<-lsfit(x,y)
out<-ls.print(fit,print.it=F)
r2<-round(100*as.numeric(out$summary[1,2]),1)
pval<-round(as.numeric(out$summary[1,6]),3)
beta<-round(as.numeric(out$coef.table[[1]][2,1]),3)
betaerr<-round(as.numeric(out$coef.table[[1]][2,2]),3)
abline(fit,lwd=3)
xy<-cbind(x,y)
xy<-na.omit(xy)
lines(lowess(xy[,1],xy[,2]),lwd=3,lty=2)
subtit=paste("R2=",r2," p-value=",pval," Slope estimate=",beta," Slope error=",betaerr)
title(main=main)
title(sub=subtit,cex=0.5)
par(op)
}


ts<-function(ys=x[1,1],ye=x[nrow(x),1],x=dd) 
{
#
# EDA function
#
par(mfrow=c(3,1))
xs<-x[(x[,1]>=ys)&(x[,1]<=ye),]
ts.plot(xs[,4])
title("Daily precipitation",xlab="day",ylab="precip")
ts.plot(xs[,5])
title("Daily maximum temperature",xlab="day",ylab="tmax")
ts.plot(xs[,6])
title("Daily minimum temperature",xlab="day",ylab="tmin")
par(mfrow=c(1,1))

cat(paste("Station series defined from ",x[1,1]," to ",x[nrow(x),1],"\n"))
cat(paste("Summary statistics for window from ",ys," to ", ye,"\n"))
}

function (y1=x[1,1],y2=dd[nrow(x),1],m=1,v=4,x=dd) 
{
#
# Little function to see how many NAs there are in month m
# in period y1 to y2 for variable v:
# 
# Usage: nas(1960,1990,1,4)
#

xs<-x[(x[,1]>=y1)&(x[,1]<=y2)&(x[,2]==m),]
cat(paste("Years from ",y1,"-",y2," Month=",m," Variable=",v,"\n"))
cat(paste("Total number of days=",length(xs[,v])," Total missing=",sum(is.na(xs[,v])),"\n"))
}

startss<-function(){
tkwm.title(start1,"RclimDex")
tkgrid(tklabel(start1,text="             RClimDex 1.0             ",font=fontHeading))
tkgrid(tklabel(start1,text="",font=fontHeading))#empty line
tkgrid(tklabel(start1,text="",font=fontHeading))#empty line
#tkgrid(tklabel(start1,text="",font=fontHeading))#empty line
start.but<-tkbutton(start1,text="Load Data and Run QC",command=getfile,width=30)
cal.but<-tkbutton(start1,text="Indices Calculation",command=nastat,width=30)
cancel.but<-tkbutton(start1,text="Exit",command=done,width=30)
tkgrid(start.but)
tkgrid(cal.but)
tkgrid(cancel.but)
tkgrid(tklabel(start1,text="",font=fontHeading))   }#end of startss function

startss()
