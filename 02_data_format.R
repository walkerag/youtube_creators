##############################################
#NAME: 02_data_parse.R
#PURPOSE: Wrangle pulled API data into convenient dataframes for analysis
#DATE: October 2017
##############################################

rm(list=ls())
options(scipen=999)

library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(jsonlite)
library(httr)
library(RSelenium)
library(V8)
library(rvest)
library(knitr)
library(scales)
library(reshape)
library(ggrepel)
library(stringr)
library(igraph)
library(ggraph)
library(widyr)
library(broom)
library(gridExtra)
library(gtable)
library(extrafont)
library(tidytext)

path<-'/Users/walkerag/Documents/youtube_creators/data/'

#############################################
#DATA READ-IN
#############################################

stats_all<-readRDS(file = paste0(path,"stats_all.rds"))
content_all<-readRDS(file = paste0(path,"content_all.rds"))
branding_all<-readRDS(file = paste0(path,"branding_all.rds"))
vids_all<-readRDS(file = paste0(path,"vids_all.rds"))
vid_stats_all<-readRDS(file = paste0(path,"vid_stats_all.rds"))
vid_details_all<-readRDS(file = paste0(path,"vid_details_all.rds"))
vid_content_all<-readRDS(file = paste0(path,"vid_content_all.rds"))
channel_id<-readRDS(file = paste0(path,"channel_id.rds"))

#############################################
#VARIABLE PARSING
#############################################

#Some very hacky lapply, sapply code to get things in dataframes
#Had issues with missing elements

##############
#CHANNEL STATS
##############

#Branding
channel_desc<-sapply(branding_all, function(l) l$items[[1]]$brandingSettings$channel$description)
channel_desc[sapply(channel_desc,is.null)]<-NA
channel_desc<-unlist(channel_desc)
branding<-data.frame(channel_title=sapply(branding_all, function(l) l$items[[1]]$brandingSettings$channel$title)
                     ,channel_desc,stringsAsFactors = FALSE)
head(branding)

#Other stats
channel_stats<-sapply(stats_all, unlist)
channel_stats<-data.frame(t(channel_stats),stringsAsFactors = FALSE)
channel_stats$channel_id<-channel_id
head(channel_stats)

#Add branding
channel_stats<-cbind(channel_stats,branding)
rm(branding)

##############
#VIDEO STATS
##############

#DURATION
duration_all<-NULL
for(i in 1:length(vid_content_all)){
  
  x<-vid_content_all[[i]]
  duration<-unlist(lapply(x, function(l) l[[4]][[1]]$contentDetails$duration))
  duration_all<-append(duration_all,duration)
  rm(duration)
  rm(x)
  
}

head(duration_all)

#TITLE
title_all<-NULL
for(i in 1:length(vid_content_all)){
  
  x<-vid_details_all[[i]]
  title<-unlist(lapply(x, function(l) l[[4]][[1]]$snippet$localized$title))
  title_all<-append(title_all,title)
  rm(title)
  rm(x)
  
}

head(title_all)

#DESCRIPTION
description_all<-NULL
for(i in 1:length(vid_content_all)){
  
  x<-vid_details_all[[i]]
  description<-unlist(lapply(x, function(l) l[[4]][[1]]$snippet$localized$description))
  description_all<-append(description_all,description)
  rm(description)
  rm(x)
  
}

#TAGS
tags_all<-NULL
for(i in 1:length(vid_details_all)){
  
  x<-vid_details_all[[i]]
  tags<-lapply(x, function(l) l[[4]][[1]]$snippet$tags)
  
  #Concat the tags together
  tags<-sapply(tags,function(x) paste(unlist(x),collapse=" "))
  
  tags_all<-append(tags_all,tags)
  rm(tags)
  rm(x)
  
}


#VIDEO STATS
vid_stats_all.df<-NULL
for(i in 1:length(vid_stats_all)){
  
  print(i)
  x<-vid_stats_all[[i]]
  
  x.id<-sapply(x,function(x) x$id,simplify=FALSE)
  x.id[sapply(x.id, is.null)] <- NA
  x.id<-unlist(x.id)
  
  x.viewCount<-sapply(x,function(x) x$viewCount,simplify=FALSE)
  x.viewCount[sapply(x.viewCount, is.null)] <- NA
  x.viewCount<-unlist(x.viewCount)
  
  x.likeCount<-sapply(x,function(x) x$likeCount,simplify=FALSE)
  x.likeCount[sapply(x.likeCount, is.null)] <- NA
  x.likeCount<-unlist(x.likeCount)
  
  x.dislikeCount<-sapply(x,function(x) x$dislikeCount,simplify=FALSE)
  x.dislikeCount[sapply(x.dislikeCount, is.null)] <- NA
  x.dislikeCount<-unlist(x.dislikeCount)
  
  x.favoriteCount<-sapply(x,function(x) x$favoriteCount,simplify=FALSE)
  x.favoriteCount[sapply(x.favoriteCount, is.null)] <- NA
  x.favoriteCount<-unlist(x.favoriteCount)
  
  x.commentCount<-sapply(x,function(x) x$commentCount,simplify=FALSE)
  x.commentCount[sapply(x.commentCount, is.null)] <- NA
  x.commentCount<-unlist(x.commentCount)
  
  x.comb<-data.frame(x.id,x.viewCount,x.likeCount,x.dislikeCount,x.favoriteCount,x.commentCount,stringsAsFactors = FALSE)
  
  x.comb$channel_id<-channel_id[i]
  
  vid_stats_all.df<-rbind(vid_stats_all.df,x.comb)
  rm(x.comb)
  rm(x.viewCount,x.likeCount,x.dislikeCount,x.favoriteCount,x.commentCount)
  
}

head(vid_stats_all.df)

#PUBLICATION
pub_all<-NULL
for(i in 1:length(vids_all)){
  
  x<-vids_all[[i]]
  x <- data.frame(lapply(x, as.character), stringsAsFactors=FALSE)
  
  x$channel_id<-channel_id[i]
  
  pub_all<-rbind(pub_all,x)
  rm(x)
  
}
head(pub_all)

###################################
#CHECK MISMATCH
###################################

summ.pub<-pub_all %>% group_by(channel_id) %>% summarise(total=n())
summ.stats<-vid_stats_all.df %>% group_by(channel_id) %>% summarise(total=n())

summ.pub[summ.pub[,2]!=summ.stats[,2],]
summ.stats[summ.pub[,2]!=summ.stats[,2],]

head(pub_all[pub_all$channel_id=='UCf4ktrnK_jWG1mC7U7fNphA',])
head(vid_stats_all.df[vid_stats_all.df$channel_id=='UCf4ktrnK_jWG1mC7U7fNphA',])

head(pub_all[pub_all$channel_id=='UCYPaj6OXNItxuDotrkF192w',])
head(vid_stats_all.df[vid_stats_all.df$channel_id=='UCYPaj6OXNItxuDotrkF192w',])

#Remove the two very recent uploads
keep<-!(pub_all$contentDetails.videoId %in% c('yWatz9TAgT0','az49MSDsGHM'))
sum(keep)

duration_all<-duration_all[keep]
pub_all<-pub_all[keep,]

rm(keep)
rm(summ.pub)
rm(summ.stats)

##################################
#Format channel stats
##################################

#Keep necessary variables
names(channel_stats)
channel_stats<-subset(channel_stats,select=c(items.statistics.viewCount,items.statistics.commentCount
                             ,items.statistics.subscriberCount
                             ,items.statistics.videoCount,channel_id,channel_title,channel_desc))
names(channel_stats)<-c('channel_views','channel_comments','channel_subs','channel_videos','channel_id','channel_title','channel_desc')
channel_stats[,1:4] <- sapply( channel_stats[,1:4], as.numeric )
head(channel_stats)

#Views per subscriber
channel_stats$channel_views_per_sub<-channel_stats$channel_views/channel_stats$channel_subs

###################################
#Format video stats
###################################

video_all<-cbind(pub_all,vid_stats_all.df[,1:6])
video_all<-cbind(video_all,description_all,stringsAsFactors=FALSE)
video_all<-cbind(video_all,title_all,stringsAsFactors=FALSE)
video_all<-cbind(video_all,tags_all,stringsAsFactors=FALSE)
video_all<-cbind(video_all,duration_all,stringsAsFactors=FALSE)

#Restrict to needed variables
video_all<-subset(video_all,select=c(contentDetails.videoId,contentDetails.videoPublishedAt
                                     ,channel_id,x.viewCount,x.likeCount,x.dislikeCount,x.favoriteCount
                                     ,x.commentCount,description_all,title_all,tags_all,duration_all))
names(video_all)
names(video_all)<-c('id','published','channel_id','views','likes','dislikes','favorites','comments','description','title','tags','duration')

#Format date
video_all$Date<-as.Date(substr(video_all$published,1,10),format="%Y-%m-%d")
min(video_all$Date)
max(video_all$Date)

#Format duration
video_all$duration2<-gsub("PT","",video_all$duration)
video_all$H<-as.numeric(regexpr("H",video_all$duration2))
video_all$M<-as.numeric(regexpr("M",video_all$duration2))
video_all$S<-as.numeric(regexpr("S",video_all$duration2))

video_all$HN<-as.numeric(ifelse(video_all$H>0,substr(video_all$duration2,1,(video_all$H-1)),0))

video_all$MN<-as.numeric(ifelse(video_all$H>0 & video_all$M>0,substr(video_all$duration2,(video_all$H+1),(video_all$M-1)),0))
video_all$MN<-as.numeric(ifelse(video_all$H<0 & video_all$M>0,substr(video_all$duration2,1,(video_all$M-1)),video_all$MN))
summary(video_all$MN)

video_all$SN<-as.numeric(ifelse(video_all$M>0 & video_all$S>0,substr(video_all$duration2,(video_all$M+1),(video_all$S-1)),0))
video_all$SN<-as.numeric(ifelse(video_all$M<0 & video_all$S>0,substr(video_all$duration2,1,(video_all$S-1)),video_all$SN))
summary(video_all$SN)

#Final time in seconds
video_all$seconds<-(video_all$HN*3600)+(video_all$MN*60)+(video_all$SN)
summary(video_all$seconds)

#Merge on channel data
video_all<-merge(video_all,channel_stats,by=c('channel_id'))

#Make some variables numeric
video_all[,4:8] <- sapply(video_all[,4:8], as.numeric )
summary(video_all$views)

video_all<-subset(video_all,select=-c(H,M,S,HN,MN,SN,duration2))

saveRDS(video_all, file = paste0(path,"video_dat_format.rds"))
saveRDS(channel_stats, file = paste0(path,"channel_dat_format.rds"))
