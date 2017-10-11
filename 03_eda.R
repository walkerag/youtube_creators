##############################################
#NAME: 03_eda.R
#PURPOSE: Make some fancy(ish) charts for blogging purposes
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

#Import fonts
#font_import(prompt=FALSE)
#fonts()

#############################################
#DATA READ-IN
#############################################

video_all<-readRDS(file = paste0(path,"video_dat_format.rds"))
channel_stats<-readRDS(file = paste0(path,"channel_dat_format.rds"))

#############################################
#START CHARTS
#############################################

#CHANNELS BY FIRST VIDEO

#Additional channel summary
channel_summ<-video_all %>% group_by(channel_title,channel_id) %>% 
  summarise(total=n()
            ,first=min(Date)
            ,last=max(Date)
            ,days=last-first
            ,avg_views=mean(views)
            ,median_views=median(views)
            ,sd_views=sd(views)
            ,max_views=max(views)
            ,subs=max(channel_subs)
  )

channel_summ$first_posix<-as.POSIXct(channel_summ$first, format="%Y-%m-%d")

channel_summ$color<-'#d71e17'
lims <- as.POSIXct(strptime(c("2006-01-01","2017-10-01"), format = "%Y-%m-%d"))    
ggplot(channel_summ,aes(x=first_posix,y=subs)) +
  ggtitle("Not A Celebrity Overnight: Channels by First Video") +
  geom_point(color=channel_summ$color,alpha=0.9,size=5.5) +
  scale_x_datetime(labels = date_format("%Y"),date_breaks="1 year",limits=lims
  ) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,300000),breaks=seq(0,300000,by=50000)) +
  theme(
    panel.grid.minor = element_blank() # get rid of minor grid
    ,text = element_text(size = 36
                         ,family="Arial Bold")
    #    ,title = element_text(face="bold")
    ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
    ,panel.background = element_rect(fill = '#f1f1f1')
  ) +
  xlab("First Video Published") +
  ylab("Current Channel Subs") +
  geom_text_repel(data=(channel_summ[(channel_summ$first<"2008-01-01") |
                                       (channel_summ$subs>200000) |
                                       (channel_summ$subs>120000 & channel_summ$first<"2015-01-01")
                                     ,]), aes(label=channel_title)
                  ,fontface="bold"
                  ,color="#4ebdc0"
                  ,force=3
                  ,size=10) 

#Check % of videos over 1M views
sum(video_all$views>1000000)/dim(video_all)[1]

#Summary of each channel's most viewed video
max_video_views<-video_all %>% group_by(channel_title) %>% summarise(max_views=max(views))
head(max_video_views)
summary(max_video_views)

rm(max_video_views,lims)

#############################
#TAG PROCESSING
#############################

#Tokenize tags
video_all.tags<-video_all %>% dplyr::select(channel_id,id,tags) %>%
  unnest_tokens(input=tags, word,to_lower=TRUE,drop=FALSE)

#Remove stop words
video_all.tags <- video_all.tags %>%
  anti_join(stop_words)

#Add summaries
tags.distinct<-video_all.tags %>% 
  group_by(word) %>% 
  summarise(
    count=n()
    ,channels=n_distinct(channel_id)
    ,videos=n_distinct(id)
  )

#Mutate version
video_all.tags<-video_all.tags %>% 
  group_by(word) %>% 
  mutate(
    count=n()
    ,channels=n_distinct(channel_id)
    ,videos=n_distinct(id)
  )

#TAGS CORRELATION GRAPH

keyword_cors <- video_all.tags %>% filter(channels>=10) %>%
  group_by(word) %>%
  filter(n() >= 100) %>%
  pairwise_cor(word, id, sort = TRUE, upper = FALSE)

keyword_cors$Correlation<-keyword_cors$correlation
set.seed(1234)
keyword_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = Correlation, edge_width = Correlation), edge_colour = "#4ebdc0") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size=9.5, 
                 point.padding = unit(0.2, "lines")) +
  theme(text = element_text(size = 9.5,family="Arial Bold")
        ,legend.key.size = unit(2.5,"line")
        ,legend.text = element_text(size=20)
        ,legend.title = element_text(size=20)
        ,panel.border = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_blank()
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,axis.title = element_blank()
        ,legend.box.background = element_blank()
        ,legend.key=element_blank()
        ,title = element_text(size=35)
  ) +
  ggtitle("Creator Topics: Fitness, Disney, Family, And More")

#Not much political content

############################
#POSTING FREQUENCY
############################

#Add first video
video_all<-video_all %>% group_by(channel_id) %>% mutate(first_video=min(Date))

#Subset down to past year
video_all.pastyear<-video_all[video_all$Date>='2016-10-10' & video_all$first_video<="2016-10-10",]

#VIDEOS BY DAY OF WEEK

video_all.pastyear$dayofweek<-weekdays(video_all.pastyear$Date)
video_all.pastyear$dayofweek <- factor(video_all.pastyear$dayofweek, c( "Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))
ggplot(video_all.pastyear,aes(x=dayofweek)) +
  geom_bar(aes(y = (..count..)/sum(..count..))
           ,fill=c('#d71e17','#d71e17','#d71e17','#d71e17','#3bcfd4','#d71e17','#d71e17')
           ,alpha=0.9,width=0.7
  ) +
  ggtitle("Creators Post The Most On Fridays") +
  theme(
    panel.grid.minor = element_blank() # get rid of minor grid
    ,text = element_text(size = 36
                         ,family="Arial Bold")
    #    ,title = element_text(face="bold")
    ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
    ,panel.background = element_rect(fill = '#f1f1f1')
  ) +
  xlab(NULL) +
  ylab("Total Videos") +
  scale_y_continuous(labels = percent,limits=c(0,0.2),breaks=c(0,0.04,0.08,0.12,0.16,0.2))

#Summarize by channel
channel.pastyear<-video_all.pastyear %>% group_by(channel_id, channel_title) %>% summarise(
  total_videos=n()
  ,total_views=sum(views)
  ,median_views=median(views)
  ,avg_views=mean(views)
  ,videos_per_week=total_videos/52
)

head(channel.pastyear)

#CHANNELS BY POST FREQUENCY

ggplot(channel.pastyear,aes(x=videos_per_week,avg_views)) +
  geom_point(color="#d71e17",alpha=0.9,size=5.5) +
  ggtitle("The Majority Of Creators Post 0-2 Videos Per Week") +
  theme(
    panel.grid.minor = element_blank() # get rid of minor grid
    ,text = element_text(size = 36
                         ,family="Arial Bold")
    ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
    ,panel.background = element_rect(fill = '#f1f1f1')
  ) +
  xlab("Videos Per Week") +
  ylab("Average Views Per Video") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,700000),breaks=seq(0,700000,by=100000)) +
  scale_x_continuous(limits=c(0,7),breaks=seq(0,7,by=1)) +
  geom_text_repel(data=channel.pastyear[(channel.pastyear$avg_views>200000) |
                                          (channel.pastyear$videos_per_week>3.5),]
                  , aes(label=channel_title)
                  ,color="#4ebdc0"
                  ,fontface="bold"
                  ,force=7
                  ,size=10) 

#######################################
#VIDEO LENGTH
#######################################

video_all$minutes<-video_all$seconds/60

ggplot(video_all %>% filter(minutes<=30),aes(x=minutes,y=views)) +
  geom_point(color="#d71e17",alpha=0.9,size=5.5) +
  ggtitle("Viral Videos Are Few And Far Between") +
  theme(
    panel.grid.minor = element_blank() # get rid of minor grid
    ,text = element_text(size = 36
                         ,family="Arial Bold")
    ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
    ,panel.background = element_rect(fill = '#f1f1f1')
  ) +
  xlab("Video Length (Minutes)") +
  ylab("Views") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,6000000),breaks=seq(0,6000000,by=1000000)) +
  scale_x_continuous(limits=c(0,30),breaks=seq(0,30,by=5)) +
  geom_text_repel(data=video_all[(video_all$views>5000000)
                                 | (video_all$views>1000000 & video_all$minutes>20)
                                 | (video_all$views>2500000 & video_all$minutes>10)
                                 | (video_all$views>3000000 & video_all$minutes>5)
                                 | (video_all$views>4000000 & video_all$minutes>0)
                                 ,]
                  , aes(label=title)
                  ,color="#4ebdc0"
                  ,fontface="bold"
                  ,force=7
                  ,size=8) 