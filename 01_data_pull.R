##############################################
#NAME: 01_data_pull.R
#PURPOSE: Use tuber package to collect channel data on "Creators On The Rise"
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

#Place to save
path<-'/Users/walkerag/Documents/youtube_creators/data/'

#########################################################
#Feed in current creators page, plus some cached versions
#########################################################

feed_urls<-c(
  "https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20171004163139/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20171001133554/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170917062554/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170910070309/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170826122150/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170827084418/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170810041732/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170708232602/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170702001437/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170716031814/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170723062851/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170624214530/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170617215729/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170610204650/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170603192142/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170614142710/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170605014831/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170513191949/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170520172819/https://www.youtube.com/channels/creator_on_the_rise"
  ,"https://web.archive.org/web/20170527183417/https://www.youtube.com/channels/creator_on_the_rise"
)

#Initialize scraper
rD<-rsDriver()
remDr <- rD[["client"]]
remDr$open()

#Loop through the URLs
links_all<-NULL
for(f in 1:length(feed_urls)){
  
  #Be considerate
  Sys.sleep(1)
  
  furl<-feed_urls[f]
  
  #SCRAPE
  remDr$navigate(furl)
  page_source<-remDr$getPageSource()
  feed_dat <- read_html(page_source[[1]])
  rm(page_source)
  
  #Get YouTube links
  links<-feed_dat %>% html_nodes(xpath='//*[@id="builder-root"]/div/div/a') %>% html_nodes(xpath='@href') %>% html_text()
  
  links_all<-unique(append(links_all,links))
  #links_all
  
  rm(links)
  rm(feed_dat)
  rm(furl)
  
  #Save output
  saveRDS(links_all, file = paste0(path,"creator_links.rds"))
  
}

########################
#GET CHANNEL INFO
########################

#Get relevant part of URL
channel_id<-substr(links_all,sapply(links_all, function(x) rev(gregexpr("\\/", x)[[1]])[1]+1),nchar(links_all))

#Save unique channels
channel_id<-unique(channel_id)
saveRDS(channel_id, file = paste0(path,"channel_id.rds"))

#Initialize empty objects
stats_all<-{}
content_all<-{}
branding_all<-{}
vids_all<-{}

#Using tuber functions to collect various aspects of channel via API
for(i in 1:length(channel_id)){
  
  print(i)
  
  #Get ID
  channel_lookup<-channel_id[i]
  
  #Check if user ID
  #Otherwise it's a channel ID
  if(!grepl("^UC", channel_lookup)){
    
    print('User')
    
    #Get channel stats
    stats<-list_channel_resources(filter = c(username = channel_lookup), part="statistics")
    stats_all[[i]]<-unlist(stats)
    
    #Get content details
    Sys.sleep(1)
    content<-list_channel_resources(filter = c(username = channel_lookup), part="contentDetails")
    content_all[[i]]<-content
    
    #Get branding
    Sys.sleep(1)
    branding<-list_channel_resources(filter = c(username = channel_lookup), part="brandingSettings")
    branding_all[[i]]<-branding
    
  }else{
    
    print('Channel')
    
    #Get channel stats
    stats<-list_channel_resources(filter = c(channel_id = channel_lookup), part="statistics")
    stats_all[[i]]<-unlist(stats)
    
    #Get content details
    Sys.sleep(1)
    content<-list_channel_resources(filter = c(channel_id = channel_lookup), part="contentDetails")
    content_all[[i]]<-content
    
    #Get branding
    Sys.sleep(1)
    branding<-list_channel_resources(filter = c(channel_id = channel_lookup), part="brandingSettings")
    branding_all[[i]]<-branding
    
  }
  
  rm(stats)
  
  #Get upload playlist ID
  playlist_id <- content$items[[1]]$contentDetails$relatedPlaylists$uploads
  print(playlist_id)
  rm(content)
  
  # Get videos on the playlist
  Sys.sleep(1)
  vids <- get_playlist_items(filter= c(playlist_id=playlist_id))
  vids_all[[i]]<-vids
  rm(playlist_id)
  
  #Save every 10 to be safe
  if((i %% 10)==0){
    
    #Save everything
    saveRDS(stats_all, file = paste0(path,"stats_all.rds"))
    saveRDS(content_all, file = paste0(path,"content_all.rds"))
    saveRDS(branding_all, file = paste0(path,"branding_all.rds"))
    saveRDS(vids_all, file = paste0(path,"vids_all.rds"))
    
  }
  
}
saveRDS(stats_all, file = paste0(path,"stats_all.rds"))
saveRDS(content_all, file = paste0(path,"content_all.rds"))
saveRDS(branding_all, file = paste0(path,"branding_all.rds"))
saveRDS(vids_all, file = paste0(path,"vids_all.rds"))


########################
#VIDEO STATS
########################

#Initialize empty objects
vid_stats_all<-{}
vid_details_all<-{}
vid_content_all<-{}

#Now getting data on specific videos from the channel using the channel's upload playlist
for(i in 1:length(channel_id)){
  
  print(i)
  
  #Get upload playlist ID
  playlist_id <- content_all[[i]]$items[[1]]$contentDetails$relatedPlaylists$uploads
  print(playlist_id)
  
  #Video count
  video_count<-as.numeric(stats_all[[i]][12])
  print(video_count)
  
  # Get videos on the playlist
  vids <- get_playlist_items(filter= c(playlist_id=playlist_id),max_results=video_count)
  
  #Setting max count above 50 gets all of the channel's videos
  #But if channel has fewer videos than the max count variable it will return no data
  #So keep reducing max count until videos are returned
  while(length(vids)==0){
    
    video_count<-video_count-1
    print(video_count)
    vids <- get_playlist_items(filter= c(playlist_id=playlist_id),max_results=video_count)
    
  }
  
  vids_all[[i]]<-vids
  rm(playlist_id)
  rm(video_count)
  
  if(length(vids)>0){
    
    #Get video IDs
    ids<-as.character(vids$contentDetails.videoId)
    rm(vids)
    
    #Go through the channel's videos, get stats and details
    vid_stats<-{}
    vid_details<-{}
    vid_content<-{}
    print('Get Videos')
    for(v in 1:length(ids)){
      
      print(v)
      id<-ids[v]
      
      #Get info on each video
      Sys.sleep(0.1)
      vid_stats[[v]]<-get_stats(video_id=id)
      Sys.sleep(0.1)
      vid_details[[v]]<-get_video_details(video_id=id)
      Sys.sleep(0.1)
      vid_content[[v]]<-get_video_details(video_id=id,part=c('contentDetails'))
      
      rm(id)
      
    }
    print('End Videos')
    rm(v)
    
    #Save
    vid_stats_all[[i]]<-vid_stats
    vid_details_all[[i]]<-vid_details
    vid_content_all[[i]]<-vid_content
    rm(vid_stats)
    rm(vid_details)
    rm(vid_content)
    
  }else{
    
    print("Playlist not found")
    
  }
  
  
  #Save every 10
  
  if((i %% 10)==0){
    
    #Save everything
    saveRDS(vid_stats_all, file = paste0(path,"vid_stats_all.rds"))
    saveRDS(vid_details_all, file = paste0(path,"vid_details_all.rds"))
    saveRDS(vid_content_all, file = paste0(path,"vid_content_all.rds"))
    
  }
  
}
saveRDS(vid_stats_all, file = paste0(path,"vid_stats_all.rds"))
saveRDS(vid_details_all, file = paste0(path,"vid_details_all.rds"))
saveRDS(vid_content_all, file = paste0(path,"vid_content_all.rds"))

