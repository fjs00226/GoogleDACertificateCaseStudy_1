## check if the directory exists
if(!file.exists("./raw_data")){
  dir.create("./raw_data")
}

## load ggplot2
library(ggplot2)
install.packages("patchwork")
library(patchwork)
## down load the file
url<-"https://divvy-tripdata.s3.amazonaws.com"
file_name <- c("/202109-divvy-tripdata.zip",
               "/202110-divvy-tripdata.zip",
               "/202111-divvy-tripdata.zip",
               "/202112-divvy-tripdata.zip",
               "/202201-divvy-tripdata.zip",
               "/202202-divvy-tripdata.zip",
               "/202203-divvy-tripdata.zip",
               "/202204-divvy-tripdata.zip",
               "/202205-divvy-tripdata.zip",
               "/202206-divvy-tripdata.zip",
               "/202207-divvy-tripdata.zip",
               "/202208-divvy-tripdata.zip")
for (i in file_name){
  download.file(paste(url,i,sep=""),paste("./raw_data/",i,sep=""))
  unzip(zipfile=paste("./raw_data",i,sep=""),exdir="./raw_data")
}

## read all csv files into one data frame: data_all
list_csv_files <- list.files(path = "./raw_data/",pattern="*.csv")
data_all <-data.frame()
for (i in 1:12){
  data_all<-rbind(data_all,read.csv(paste("./raw_data/",list_csv_files[i],sep="")))
} ## got 5883043 obs. of 13 variables


## check if the data is clean
## view data_all
head(data_all) ## ride_id, type, station, latitude and longitude, member type
str(data_all) ## dates are chr, needs to convert to date when use
nrow(data_all) ##5883043

## any duplication of data?
nrow(data_all[duplicated(data_all$ride_id),]) # no duplication

## is time stamp consistent? The time unit is not specified, I assumed it used 
## the Chicago local time, which is EST.
data_all$started_at <- as.POSIXct(data_all$started_at, format="%Y-%m-%d %H:%M:%S")
data_all$ended_at <- as.POSIXct(data_all$ended_at, format="%Y-%m-%d %H:%M:%S")
class(data_all$started_at)
class(data_all$ended_at)

## add the year-month as a column to the very right of the data, this will be used to group the data
data_all$year_month<-format(data_all$started_at, "%y/%m")

## add the weekdays
data_all$weekday<-weekdays(data_all$started_at)

## add the riding time
data_all$riding_time <-difftime(data_all$ended_at,data_all$started_at,units = "mins")

## remove riding_time less than 0
data_all_v1 <- data_all[!(data_all$riding_time <= 0),]
nrow(data_all_v1) # 5882437

## remove irregular riding time
data_all_v1$riding_time<-as.numeric(data_all_v1$riding_time)
summary(data_all_v1$riding_time) ## max 40705.02 weird data
# remove riding_time larger than 1 day = 1440 min
data_all_v1<-data_all_v1[which(data_all_v1$riding_time<=1440),] 
nrow(data_all_v1)  # 5877239
summary(data_all_v1$riding_time)

## remove unnecessary columns
data_all_v1 <- data_all_v1[,-which(names(data_all_v1) %in%
                           c("start_station_id","end_station_id","start_lat","start_lng","end_lat","end_lng")),drop=FALSE]

## add the time of rides use the start time
## assign am peak (6:00-8:00), pm peak (16:00-18:00), and other to time 
## don't use for loop, it is very slow
data_all_v1$time <- as.numeric(format(data_all_v1$started_at, "%H"))
data_all_v1$time_cat<- NA
#for (i in 1:length(data_all$time)){
#  if(data_all[i,"time"]>=6 & data_all[i,"time"]<=8){
#    data_all[i,"time_cat"] = "am_peak"
#  }
#  else if(data_all[i,"time"]>=16 & data_all[i,"time"]<=18){
#    data_all[i,"time_cat"] = "pm_peak"
#  }
#  else{
#    data_all[i,"time_cat"] = "other"
#  }
#}

## use match
data_all_v1 <- data_all_v1[order(data_all_v1$time),]
am_match_result <- which(data_all_v1$time %in% c(6,7,8))
data_all_v1[am_match_result,"time_cat"]="am_peak"

pm_match_result <- which(data_all_v1$time %in% c(16,17,18))
data_all_v1[pm_match_result,"time_cat"]="pm_peak"

na_match_result <- which(is.na(data_all_v1$time_cat))
data_all_v1[na_match_result,"time_cat"]="other"

## use all lower case for station name, replace missing value with "unknown station"
## is it NA or blank? it is "" (nothing)
data_all_v1$start_station_name <- tolower(data_all_v1$start_station_name)
data_all_v1$end_station_name <- tolower(data_all_v1$end_station_name)
missing <- which(data_all_v1$start_station_name %in% "")
data_all_v1$start_station_name[missing] = "unknown station"
missing <- which(data_all_v1$end_station_name %in% "")
data_all_v1$end_station_name[missing] = "unknown station"

## use all lower case for bike type
data_all_v1$rideable_type <- tolower(data_all_v1$rideable_type)

## check is the data frame is complete
complete <- complete.cases(data_all_v1)
length(which(complete=="TRUE")) #5877239
nrow(data_all_v1) # no missing values, data is complete
## save this data frame
write.csv(data_all_v1,"./data_all_v1.csv",row.names = FALSE)

## how many casual/member do we have?
table(data_all_v1$member_casual) # casual 2463604 member 3413635

## what is the mean riding time for casual/member?
tapply(data_all_v1$riding_time,data_all_v1$member_casual,mean) # casual 23,member 12

## let's see usage for members and casual users for different months
year_month_count<-as.data.frame(table(data_all_v1$member_casual,data_all_v1$year_month))
colnames(year_month_count)<-c("user_type","year_month","counts")
write.csv(year_month_count,"./year_month_count.csv",row.names = FALSE)
## mean
year_month_mean<-as.data.frame(aggregate(data_all_v1$riding_time, 
                                         list(data_all_v1$member_casual,data_all_v1$year_month), FUN=mean)) 
colnames(year_month_mean)<-c("user_type","year_month","mean_riding_time")
write.csv(year_month_mean,"./year_month_mean.csv",row.names = FALSE)

## visualization
g1<-ggplot(data=year_month_count,aes(x = year_month, y = counts, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))
g2<-ggplot(data=year_month_mean,aes(x = year_month, y = mean_riding_time, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))
g1|g2
## total riding time
total_by_month<-as.data.frame(aggregate(data_all_v1$riding_time,list(data_all_v1$member_casual,data_all_v1$year_month), FUN=sum))
colnames(total_by_month)<-c("user_type","year_month","total_riding_time")
ggplot(data=total_by_month,aes(x = year_month, y = total_riding_time, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))+ theme(aspect.ratio = 1)+ggtitle("Total_riding_time_vs_months")
## how about different days
day_count<-as.data.frame(table(data_all_v1$member_casual,data_all_v1$weekday))
colnames(day_count)<-c("user_type","day","counts")
day_count$day <- ordered(day_count$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
day_count<-day_count[order(day_count$day),]
write.csv(day_count,"./day_count.csv",row.names = FALSE)
## mean
day_mean<-as.data.frame(aggregate(data_all_v1$riding_time, 
                                         list(data_all_v1$member_casual,data_all_v1$weekday), FUN=mean)) 
colnames(day_mean)<-c("user_type","day","mean_riding_time")
day_mean$day <- ordered(day_mean$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
day_mean<-day_mean[order(day_mean$day),]
write.csv(day_mean,"./day_mean.csv",row.names = FALSE)
## visualization
g3<-ggplot(data=day_count,aes(x = day, y = counts, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))
g4<-ggplot(data=day_mean,aes(x = day, y = mean_riding_time, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))
g3|g4

## And different time?
time_count<-as.data.frame(table(data_all_v1$member_casual,data_all_v1$time))
colnames(time_count)<-c("user_type","time","counts")
write.csv(time_count,"./time_count.csv",row.names = FALSE)
ggplot(data=time_count,aes(x = time, y = counts, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))

## any pattern in peak hours and non-peak hours?
time_cat_count<-as.data.frame(table(data_all_v1$member_casual,data_all_v1$time_cat))
colnames(time_cat_count)<-c("user_type","time_cat","counts")
time_cat_count$time_cat <- ordered(time_cat_count$time_cat, levels=c("am_peak", "pm_peak", "other"))
time_cat_count<-time_cat_count[order(time_cat_count$time_cat),]
write.csv(time_cat_count,"./time_cat_count.csv",row.names = FALSE)
## mean
time_cat_mean<-as.data.frame(aggregate(data_all_v1$riding_time, 
                                        list(data_all_v1$member_casual,data_all_v1$time_cat), FUN=mean))
colnames(time_cat_mean)<-c("user_type","time_cat","mean_riding_time")
time_cat_mean$time_cat <- ordered(time_cat_mean$time_cat, levels=c("am_peak", "pm_peak", "other"))
time_cat_mean<-time_cat_mean[order(time_cat_mean$time_cat),]
write.csv(time_cat_mean,"./time_cat_mean.csv",row.names = FALSE)
## visualization
g5<-ggplot(data=time_cat_count,aes(x = time_cat, y = counts, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))
g6<-ggplot(data=time_cat_mean,aes(x = time_cat, y = mean_riding_time, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))
g5|g6

## at which station did the users use the bike?
start_station<-as.data.frame(table(data_all_v1$member_casual,data_all_v1$start_station_name))
colnames(start_station)<-c("user_type", "start_station","counts")
start_station_casual<-start_station[which(start_station$user_type=="casual"),]
start_station_casual<-start_station_casual[order(start_station_casual$counts,decreasing=TRUE),]
start_station_casual[2:11,"start_station"] # show the most visited 10 stations

ggplot(data=start_station,aes(x = time_cat, y = counts, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))

## how about type of bike?
bike_type<-as.data.frame(table(data_all_v1$member_casual,data_all_v1$rideable_type))
colnames(bike_type)<-c("user_type", "bike_type","counts")
write.csv(bike_type,"./bike_type.csv",row.names = FALSE)

ggplot(bike_type,aes(x = bike_type, y = counts, fill = user_type))+geom_col(width=0.5, position = position_dodge(width=0.5))
