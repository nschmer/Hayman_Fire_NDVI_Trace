{
library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)
}
# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files("/Users/natalieschmer/Documents/GitHub/CSU/WR_674/Hayman_Fire_NDVI_Trace/hayman_data",full.names=T)


#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
                gather(key='site',value='value',-DateTime,-data) %>%
                filter(!is.na(value))

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

## Your code here
data_wide <- full_long %>%
                  spread(., key= 'data', value= 'value') %>%
                  filter_if(is.numeric, all_vars(!is.na(.))) %>% 
                  mutate(month= month(DateTime),
                        year= year(DateTime))

ggplot(data= data_wide %>% filter(month %in% c(6:9)), 
       aes(x= ndmi, y= ndvi, color= site))+
          geom_point()+
          geom_line()+
          theme_clean()+
          theme(legend.position = c(0.7, 0.8))

## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 


## Your code here
#make new dataframes that calculate the yearly average of each index
#use distict() to remove duplicates 
ndsi.2 <- data_wide %>% 
              filter(month %in% c(1:4)) %>%               
              group_by(year, site) %>% 
              mutate(yearly_avg = mean(ndsi)) %>% 
              ungroup() %>% 
              select(yearly_avg, site, year) %>% 
              distinct() %>% 
              mutate(index= "ndsi")

  
ndvi.2 <- data_wide %>% 
              filter(month %in% c(6:8)) %>% 
              group_by(year, site) %>% 
              mutate(yearly_avg = mean(ndvi)) %>% 
              ungroup() %>% 
              select(yearly_avg, site, year) %>% 
              distinct() %>% 
              mutate(index= "ndvi")

#create a new dataframe with the 2 indices
q2_df <- rbind(ndsi.2, ndvi.2)

#spread so each index has its own column 
q2_df <- q2_df %>% 
            spread(., key= 'index', value= 'yearly_avg') %>%
                      filter_if(is.numeric, all_vars(!is.na(.)))

#plot 
ggplot(data= q2_df, aes(x= ndsi, y= ndvi, color= site))+
         geom_point()+
         theme_few()

## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

## Your code here

#burned vs unburned 
ggplot(data= q2_df, aes(x= ndsi, y= ndvi, color =site))+
    geom_point()+
    theme_few()

#fire happened in 2002, create column for pre vs post burned   
q2_df <- q2_df %>% 
            mutate(burn = ifelse(year %in% c(1984:2001), "Pre-burned", "Post-burned"))

#pre vs post burned 
ggplot(data= q2_df, aes(x= ndsi, y= ndvi, color= site))+
        geom_point()+
        facet_wrap(~burn)+
        theme_few()

## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire?        

#greenness is ndvi 
#create columns for month, year, pre/post burn status, 
ndvi.4 <- ndvi %>% 
          mutate(year= year(DateTime),
                 month= month(DateTime),
                 burn = ifelse(year %in% c(1984:2001), "Pre-burned", "Post-burned")) %>% 
          gather(key='site',value='value', burned, unburned) %>%
                filter(!is.na(value)) %>%
          group_by(month) %>% 
          mutate(month_avg= mean(value))
  
ggplot(data = ndvi.4, aes(x= month, y= value, shape= site, color= site))+
      geom_point()+
      scale_x_discrete(name ="Month", 
                        limits=c(1:12))+
  geom_smooth(method = "loess", se= FALSE)+
  theme_few()+
  scale_color_few()+
  facet_wrap(~burn)
      

##### Question 5 ####
#What month is the snowiest on average?
#snow is ndsi 

ndsi.5 <- ndsi %>% 
            mutate(month= month(DateTime),
                   year= year(DateTime),
            burn = ifelse(year %in% c(1984:2001), "Pre-burned", "Post-burned")) %>%  
            gather(key='site',value='value', burned, unburned) %>%
            filter(!is.na(value)) %>% 
            group_by(month,site) %>% 
            mutate(monthly_avg = mean(value))

ggplot(data= ndsi.5, aes(x= month, y= value))+
  geom_point()+
  scale_x_discrete(name ="Month", 
                   limits=c(1:12))+
  geom_smooth(method = "loess", se= FALSE)+
  labs(y= "NDSI value")+
  theme_few()

#also could separate by burned/ not 
ggplot(data= ndsi.5, aes(x= month, y= value, color= site))+
  geom_point()+
  scale_x_discrete(name ="Month", 
                   limits=c(1:12))+
  geom_smooth(method = "loess", se= FALSE)+
  labs(y= "NDSI value")+
  theme_few()



