#1. Libraries

#Using RENV to attempt to keep the same packages in use throughout 
install.packages("renv")
library(renv)
#renv::init() #useed when started collecting packages, not used after initial set up. make functioning line if required

#Install packages required (may not be if RENV has worked but uncomment if needed)
#install.packages('here')
#install.packages('tidyverse')
#install.packages('readxl')
#install.packages('plotly') 
#install.packages('htmlwidgets') 
#tinytex::install_tinytex()
#renv::snapshot() #puts into the RENV folder for future use, snapshot after each new package

#Libraries
renv::restore()
library(here)#Here for loading in the data
library(tidyverse)#Tidyverse for data management
library(ggplot2) #chart making
library(readxl)#Reading in Excel sheets as that is how the data is stored 
library(plotly)#Making scroll over charts
library(htmlwidgets)#To save interactive chart

# 2. Load the data in

#Variables required for data cleaning of the ONS files

ONSList <- 68 #The length of the ONS data table, these are consistent

yearsdf1721 <- c("2017","2018","2019",
                 "2020","2021") # These are the years that we will want to take from this data frame

yearsdf0919 <- c("2009","2010","2011",
                 "2012","2013","2014",
                 "2015","2016") #Years wanted, always choose most recent

summaries <- list('Total World',
                  'Other Countries',"Europe",
                  '- of which EU',
                  '- of which EU Oth',
                  '- of which EU15',
                  "North America") #summary values used in the dataset

# Raw data locations
data1721 = (here("raw_data","section3ukresidentsvisitsabroad2017to2021.xlsx")) #data for 2017 to 2021
data0919 = (here ("raw_data","section3ukresidentsvisitsabroad2009to2019.xlsx")) #data for 2009 to 2017

# Load in latest dataset 2017-2021            
df1721<- read_excel(data1721,
                    sheet="3.06", 
                    skip=10,
                    n_max=ONSList)

##rename columns 2017-2021
df1721_n <- tibble(x = 1:9, y = c("country", "coltoremove", "2017","2018",
                                  "2019","2020","2021","blankroremove",
                                  "avgrowth1519"))#list of column names

names(df1721) <- df1721_n %>% select(y) %>% pull()#function to change the names 

##Remove NAs in rows and select years only 2017 to 2021
df1721 <- df1721 %>% 
          select("country",all_of(yearsdf1721)) %>% #Select correct years
          drop_na("country")# remove blank rows

## Read the excel file for 2009 to 2019
df0919 <- read_excel(data0919,
                     sheet="3.10", 
                     skip=10,
                     n_max=ONSList)

###rename columns 2009 - 2019
df0919_n <- tibble(x=1:19, y= c("country", 
                                "coltoremove",
                                "2009","2010","2011",
                                "2012","2013","2014",
                                "2015","2016","2017",
                                "2018","2019",
                                "blank1",
                                "change1819",
                                "blank2",
                                "growth1819",
                                "blank3",
                                "Avgrowth1519"))#list of column names

names(df0919) <- df0919_n %>% select(y) %>% pull() #function to change the names 

###Remove NAs in rows and select years only to 2017
df0919 <- df0919 %>% 
          select("country",all_of(yearsdf0919)) %>% #Select correct years
          drop_na("country")# remove blank rows

#Merge the datasets
df0921 <- left_join(df0919, df1721,by="country")

##Remove summaries in the dataset
df0921 <-df0921 %>% 
         filter((!country %in% summaries)) 

## Add ranking variable for 2021 (this will be used to decide on the cases kept)
df0921 <- df0921 %>% 
           mutate(rank_cut = rank(-`2021`, ties.method = "average"))

#save combined data set
comData_n = paste(here("created_data"), "/travel0921.csv", sep = "")   
write.csv(df0921, comData_n)   #Write data to CSV

#check data
head(df0921)




# 3. Visualisation 1 - Viewing the actual number of visits

## Create variables to allow us to choose the number of 2021 ranks 
cutrank1 <- 20
## Create variables for the years to include in visualisation
yrs_inc1 <- c(2009, 2021) 

## Filter by rank
dfv1 <- df0921 %>% 
        filter (rank_cut<=cutrank1)

## Convert to long format

longdfv1 <- dfv1 %>% select("country", 
                            "2009", "2010", "2011", "2012", "2013",
                            "2014", "2015", "2016", "2017", "2018",
                            "2019", "2021") %>% #2020 dropped due to no data 
                      pivot_longer(!country, 
                                   names_to = "year", 
                                   names_transform = list(year = as.factor), #Changed to factor to allow for it to be treated as a categor
                                   values_to = "visits") %>%
                      arrange(desc(visits)) %>% 
                      filter(year %in% yrs_inc1)
                      


## Dumbell plot
subtitle1 <- paste("Showing the top", toString(cutrank1), "most visited countries in 2021")
  
p1 <- ggplot(longdfv1, 
             aes(x = visits, y = reorder(country, visits)), text = paste("There were", visits, "visits")) + 
             geom_line(color="grey") + 
             geom_point(aes(color = year), size = 3, alpha=0.8) + 
             theme(legend.position = "bottom") +
             labs(x = "Number of visits from the UK (000s)", 
                  y = "Country", 
                  title = "Number of visits abroad from the UK in 2009 and 2021 by main country visited",
                  subtitle = subtitle1,
                  caption= "Source: Office for National Statistics International Passenger Survey")

             
p1
## Save the plot 
ggsave(here("plots", "dumbell.png"), p1)

##Dumbell plot 2 (interactive)
ip1 <- ggplotly(p1) #Use plotly for scrollover
ip1


##save the plot
saveWidget(ip1, file='interactivedumbell.html')

#Visualisation 2 - Using ranks 
## Create variables to allow us to choose the number of 2021 ranks 
cutrank2 <- 15
## Create variables for the years to include in visualisation
yrs_inc2 <- c(2009, 2021)
##Create new DF of just ranks 
### create the ranks
dfv2 <- df0921 %>% 
        mutate(r09 = rank(-`2009`, ties.method = "average"),
               r10 = rank(-`2010`, ties.method = "average"),
               r11 = rank(-`2011`, ties.method = "average"),
               r12 = rank(-`2012`, ties.method = "average"),
               r13 = rank(-`2013`, ties.method = "average"),
               r14 = rank(-`2014`, ties.method = "average"),
               r15 = rank(-`2015`, ties.method = "average"),
               r16 = rank(-`2016`, ties.method = "average"),
               r17 = rank(-`2017`, ties.method = "average"),
               r18 = rank(-`2018`, ties.method = "average"),
               r19 = rank(-`2019`, ties.method = "average"),
               r20 = r19, #no data so saying that it would have been steady from 19, needs added to data files
               r21 = rank(-`2021`, ties.method = "average")) %>%
        select("country", 
               "r09", "r10", "r11", 
               "r12", "r13", "r14", 
               "r15", "r16", "r17", 
               "r18", "r19", "r21",
               'rank_cut') %>% #2020 dropped due to no data
        rename('2009'='r09',
               '2010'='r10',
               '2011'='r11',
               '2012'='r12',
               '2013'='r13',
               '2014'='r14',
               '2015'='r15',
               '2016'='r16',
               '2017'='r17',
               '2018'='r18',
               '2019'='r19',
               '2021'='r21',
               'rank_cut2'='rank_cut')
#choose the ranks wanted
dfv2 <- dfv2 %>% 
        filter (rank_cut2 <=cutrank2)
### Change to Long format        
longdfv2 <- dfv2 %>% 
            select("country", 
                   "2009", "2010", 
                   "2011", "2012", 
                   "2013", "2014", 
                   "2015", "2016", 
                   "2017", "2018",
                   "2019", "2021") %>%
            pivot_longer(!country, 
                          names_to = "year", 
                          names_transform = list(year = as.factor), #Changed to factor to allow for it to be treated as a categor
                          values_to = "rank") %>%
            filter(year %in% yrs_inc2)


#PLOT!
### Add in own colours 
#Retrieve colour associated with each country based on their continent
dataColour <- here("created_data", "colours.csv")
dfColour <- read.csv(file = dataColour)
#match in 
longdfv2 <- left_join(longdfv2, dfColour,by="country")
#length of df for the colours
dflen <- (length(yrs_inc2)*cutrank2)




## basic plot
p <- longdfv2 %>% 
     ggplot(aes(x=year, y=rank,group=country, colour= country))


## Plot with formatting 
### titles
subtitle2 <- paste("Changes in the rank for overall visits (business and travel) for the top", toString(cutrank2)," countries from the UK between"
                   ,toString(first(yrs_inc2)), "to" ,toString(last(yrs_inc2)))
#Label for the graph
label2 <-  paste("Note: colours indicate the country grouping: Africa= pink; Asia = green; Caribbean = violet;
                         Central and South America = orange-red;Europe = blue; Middle East = orange;
                         North America = red Oceania = sea green; Other = grey")

source2 <- paste("Source: Office for National Statistics International Passenger Survey (IPS).")
note2 <- paste("*Note: 2021 data excludes data from the EuroTunnel, the Irish land border and Dover traffic is only included from Q3
               Estimates for 2017-2021 were completed using a different method, but all are based on the IPS")          
title2 <- paste("Viva Espania! Spain remains the most visited country from the UK*")

### full plot
final <- p+ 
  geom_line(alpha = 0.5, linewidth=1.5) + 
  geom_point(size = 3.5) + 
  geom_text(data= longdfv2 %>% filter(year=="2009"), aes(x=year, y=rank,label = country), size = 4, nudge_x = -0.35, fontface = "bold", color = "#000000") +
  geom_text(data= longdfv2 %>% filter(year=="2021"), aes(x=year, y=rank,label = country), size = 4, nudge_x = 0.35, fontface = "bold", color = "#000000") +
  geom_text(data= longdfv2 %>% filter(year =="2009"), aes(x=year, y=rank,label = rank), size = 4, nudge_x = -0.05, fontface = "bold", color = "#000000") +
  geom_text(data= longdfv2 %>% filter(year =="2021"), aes(x=year, y=rank,label = rank), size = 4, nudge_x = 0.05,fontface = "bold", color = "#000000") +
  scale_y_reverse( breaks=NULL) + #scale reversed so that the top visited is at the top
  scale_color_manual(values = setNames(longdfv2$hex_value, longdfv2$country)) +
  labs(x = "Year", 
       y = "Rank of visits from the UK", 
       title = title2,
       subtitle = subtitle2,
       caption= paste(toString(source2),"
                      ",
                      toString(note2))) +
  #Specify the size of the plot title and make it bold
  theme(plot.title = element_text(size=16, face = "bold"),
        #Specify the size of the tick labels on the x/y axis
        axis.text.x = element_text(size=14, face="bold", colour="black"),
        axis.text.y = element_text(size=12, face="bold"),
        #Specify the size of the axis titles and make them bold
        axis.title = element_text(size=14,face = "bold"),
        #Make the background blank
        panel.background = element_blank(),
        #Remove the legend 
        legend.position = "none",
        plot.caption = element_text(size = 10))
    
         
final

