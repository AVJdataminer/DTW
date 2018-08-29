
## Create directories first
t=format(Sys.time(), "%d_%b_%Y")
dir.create(paste("C:/MinuteClinic/Dynamic_time_warping/modeling_work_",t,sep=""))
setwd(paste("C:/MinuteClinic/Dynamic_time_warping/modeling_work_",t,sep=""))
subDir=paste(getwd())
version_path=paste(subDir,"/version_name",sep="")
ifelse(file.exists(version_path),,dir.create(file.path(version_path)))
indata_path=paste(subDir,"/dtw_indata",sep="")
ifelse(file.exists(indata_path),,dir.create(file.path(indata_path)))

modelpath=version_path
outpath=paste(subDir,"/version_name/output",sep="")
ifelse(file.exists(outpath),,dir.create(file.path(outpath)))
#create figures folder
#figures=paste(subDir,"/version_name/figures",sep="")
#ifelse(file.exists(figures),,dir.create(file.path(figures)))

list.files()
setwd(modelpath)
list.files()

require(dplyr)
require(lubridate)
# Develop best version hourly data ------
options(warn=0)
visit_level=read.csv('C:/MinuteClinic/Data/MC_data_01JAN2016_to_02JUL2018.csv')
visit_level$Employee=ifelse(visit_level$VisitOrigin=='Employee',1,0)
visit_level$Hmpil=ifelse(visit_level$VisitOrigin=='Hmpil',1,0)
visit_level$Scheduled=ifelse(visit_level$VisitOrigin=='Scheduled',ifelse(visit_level$ScheduleVisitType=="PSS",1,0),0)
visit_level$WalkIn=ifelse(visit_level$VisitOrigin=='Walk-In',1,0)
visit_level$RevenueGenerating=ifelse(visit_level$VisitStatus=='Revenue Generating',1,0)
visit_level$CompleteNonGenerating=ifelse(visit_level$VisitStatus=='Complete Non Revenue',1,0)
visit_level$ReferOutNoCharge=ifelse(visit_level$VisitStatus=='Refer Out No Charge',1,0)
visit_level$Canceled=ifelse(visit_level$VisitStatus=='Canceled',1,0)
visit_level$year=substr(visit_level$AppointmentDate,1,4)
visit_level$day=substr(visit_level$AppointmentDate,5,6)
visit_level$week=week(ymd(visit_level$AppointmentDate))
visit_level$Wait.Time=ifelse(visit_level$Canceled==1,0.01,visit_level$Wait.Time)
#subset to after Q4 of 2017
visit_level=filter(visit_level, AppointmentDate>=20171001)
visit_level$Hour=visit_level$AppointmentTimeOfDay/10000
visit_level=filter(visit_level, Hour>8 & Hour<=20)
visit_level$Hour=round(visit_level$Hour)
head(visit_level)

#convert visit level to daily to calc percent visits per hour
hourly=data.frame(visit_level%>%group_by(ClinicNumber,AppointmentDate,Hour)%>%summarise(provider.count=n_distinct(ProviderEpicid),
        Employee_Sum=sum(Employee),Hmpil_Sum=sum(Hmpil),Scheduled_Sum=sum(Scheduled),WalkIn_sum=sum(WalkIn),
        ReferOutNoCharge_Sum=sum(ReferOutNoCharge),Canceled_Sum=sum(Canceled),wait_time_avg=mean(Wait.Time,na.rm=T)))
head(hourly)

hourly$AllVisits=hourly$Employee_Sum+hourly$Hmpil_Sum+hourly$Scheduled_Sum+hourly$WalkIn_sum
head(hourly)
daily=hourly%>%group_by(ClinicNumber, AppointmentDate)%>%summarise(daily_sum=sum(AllVisits))
head(daily)

hrly=left_join(select(hourly,ClinicNumber, AppointmentDate,Hour,AllVisits), daily, by=c("ClinicNumber","AppointmentDate"))
hrly=unique(hrly)
hrly$PerVisits=hrly$AllVisits/hrly$daily_sum
head(hrly)

#check the data
range(hrly$AppointmentDate)
hrly=filter(hrly, AppointmentDate<20180101)
hrly_out=data.frame(hrly%>%group_by(ClinicNumber,Hour)%>%summarise(per_visits=mean(PerVisits, na.rm=T)))
summary(hrly_out)
write.csv(hrly_out, 'C:/MinuteClinic/Dynamic_time_warping/DTW_Input_data/hourly_percent_visits_2018.csv', row.names=F)

memory.limit()
rm(list=ls())
gc()

require(lubridate)
hrly=read.csv('C:/MinuteClinic/Dynamic_time_warping/DTW_Input_data/hourly_percent_visits_2018.csv')
head(hrly)
hrly=filter(hrly, Hour>=9 & Hour <19)
#install.packages('Hmisc')
Hmisc::describe(hrly)

#check for missing hour
counts=data.frame(table(hrly$ClinicNumber))
#counts

#create base size dataframe
clinic_list=as.character(unique(hrly$ClinicNumber))
df1=data.frame(rep(clinic_list,each=10),rep(9:18,length(clinic_list)))
head(df1)
names(df1)<-c("ClinicNumber","Hour")
df1$ClinicNumber=as.character(df1$ClinicNumber)
hrly$ClinicNumber=as.character(hrly$ClinicNumber)
df2=left_join(df1,hrly, by=c("ClinicNumber", "Hour"))
head(df2)

df2[is.na(df2)] <- 0
Hmisc::describe(df2)

#full clean dataset
write.csv(df2,'C:/MinuteClinic/Dynamic_time_warping/DTW_Input_data/hourly_percent_visits_Q42017_clean.csv', row.names=F)
head(df2)

indata=data.frame(matrix(df2$per_visits,nrow = (nrow(df2)/1133),ncol = 1133, byrow = F))
head(indata)
dim(indata)
names(indata)<-clinic_list
head(indata)
write.csv(indata,'C:/MinuteClinic/Dynamic_time_warping/DTW_Input_data/hourly_percent_visits_Q42017_column_wise.csv', row.names=F)

#let's create the dataset as rowwise with hour across the top
rdata=data.frame(matrix(df2$per_visits,nrow =1133,ncol = 10, byrow = F))
head(rdata)
dim(rdata)

#install.packages('dtw')
require(dtw)
dmatrix=(dist(rdata, method="DTW"))
clusters<-hclust(dmatrix,method="average")
str(clusters)

dim(dmatrix)
plot(clusters)

# Cut tree into 10 groups
sub_grp <- cutree(clusters, k = 10)
sub_grp2<- cutree(clusters, k = 2)
sub_grp3 <- cutree(clusters, k = 3)
sub_grp4 <- cutree(clusters, k = 4)
sub_grp5 <- cutree(clusters, k = 5)
sub_grp6<- cutree(clusters, k = 6)
sub_grp7 <- cutree(clusters, k = 7)
sub_grp8 <- cutree(clusters, k = 8)
sub_grp9 <- cutree(clusters, k = 9)
# Number of members in each cluster
table(sub_grp)
table(sub_grp3)
table(sub_grp5)

clustdf=rdata %>%
  mutate(cluster10 = sub_grp, cluster2=sub_grp2,cluster3 = sub_grp3, cluster4=sub_grp4,cluster5 = sub_grp5, cluster6=sub_grp6,
        cluster7= sub_grp7, cluster8=sub_grp8,cluster9 = sub_grp9)
clustdf$ClinicNumber<-clinic_list
head(clustdf)
write.csv(clustdf,'C:/MinuteClinic/Dynamic_time_warping/hourly_percent_visits_Q42017_clusters.csv')
#table(clustdf$cluster,clustdf$ClinicNumber)

#create long data
dflong=left_join(df2,select(clustdf,cluster10,cluster2,cluster3,cluster4,cluster5,cluster6,cluster7,cluster8,
                            cluster9,ClinicNumber), by="ClinicNumber")
head(dflong)
#plot the cluster groups with a few number of clusters by clinic by hour
write.csv(dflong,'C:/MinuteClinic/Dynamic_time_warping/hourly_percent_visits_Q42017_clusters_longformat.csv')

ggplot(dflong, aes(x=Hour, y=per_visits)) + 
    geom_line(aes(colour=as.factor(cluster10), group=as.factor(cluster10))) + # colour, group both depend on cond2
    geom_point(aes(colour=as.factor(cluster10)),               # colour depends on cond2
               size=3)+facet_wrap(~cluster10)+theme_bw()

#install.packages("factoextra")
#library(factoextra)
fviz_nbclust(rdata, FUN = hcut, method = "wss")

#install.packages('gplots')
#library(gplots) 
y <- matrix(rnorm(500), 100, 5, dimnames=list(paste("g", 1:100, sep=""), paste("t", 1:5, sep=""))) 
#head(y)
#heatmap.2(y)
heatmap.2(as.matrix(rdata))


plot(as.dendrogram(clusters), edgePar=list(col=3, lwd=4), horiz=T) 

install.packages('')
library(pheatmap); library("RColorBrewer")
pheatmap(clusterdf, color=brewer.pal(9,"Blues"))
