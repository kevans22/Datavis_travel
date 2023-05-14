#1. Libraries

#Using RENV to attempt to keep the same packages in use throughout 
install.packages("renv") # version 0.17.3
library(renv)
#renv::init() #used when started collecting packages, not used after initial set up. make functioning line if required

#Install packages required (may not be if RENV has worked but uncomment if needed)

#install.packages('here') #version 1.0.1
#install.packages('tidyverse') #version 2.0.0
#install.packages('readxl') #version 1.4.2
#install.packages('plotly') #version 4.10.1
#install.packages('htmlwidgets') # version 1.6.2
#tinytex::install_tinytex() #version 0.45
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

# 2.1. Create the variables that are required for data cleaning of the ONS files

ONSList <- 68 #The length of the ONS data table, these are consistent every year but the sheet name is not

yearsdf1721 <- c("2017","2018","2019",
                 "2020","2021") # These are the years that we will want to take from this data frame

yearsdf0919 <- c("2009","2010","2011",
                 "2012","2013","2014",
                 "2015","2016") #Years wanted, always choose most recent df

summaries <- list('Total World',
                  'Other Countries',"Europe",
                  '- of which EU',
                  '- of which EU Oth',
                  '- of which EU15',
                  "North America") #summary values used in the dataset

# Raw data locations
data1721 = (here("raw_data","section3ukresidentsvisitsabroad2017to2021.xlsx")) #data for 2017 to 2021
data0919 = (here ("raw_data","section3ukresidentsvisitsabroad2009to2019.xlsx")) #data for 2009 to 2017

# 2.2. Load in latest dataset 2017-2021            

df1721<- read_excel(data1721,
                    sheet="3.06", 
                    skip=10,
                    n_max=ONSList)

#2.2.1. Initial tidy of the 2017-2021 data set

#rename columns 2017-2021

df1721_n <- tibble(x = 1:9, 
                   y = c("country", "coltoremove", "2017","2018",
                                  "2019","2020","2021","blankroremove",
                                  "avgrowth1519"))#list of column names

names(df1721) <- df1721_n %>% select(y) %>% pull()#function to change the names 

#Remove NAs in rows and select years only 2017 to 2021
df1721 <- df1721 %>% 
          select("country",all_of(yearsdf1721)) %>% #Select correct years
          drop_na("country")# remove blank rows


# 2.3. Read the excel file for 2009 to 2019

df0919 <- read_excel(data0919,
                     sheet="3.10",
                     skip=10,
                     n_max=ONSList)

#rename columns for the 2009 - 2019 data

df0919_n <- tibble(x=1:19, 
                   y= c("country", 
                        "coltoremove",
                        "2009","2010",
                        "2011","2012",
                        "2013","2014",
                        "2015","2016",
                        "2017","2018",
                        "2019",
                        "blank1",
                        "change1819",
                        "blank2",
                        "growth1819",
                        "blank3",
                        "Avgrowth1519"))#list of column names

names(df0919) <- df0919_n %>% select(y) %>% pull() #function to change the names 

#Remove NAs in rows and select years only to 2017

df0919 <- df0919 %>% 
          select("country",all_of(yearsdf0919)) %>% #Select correct years
          drop_na("country")# remove blank rows

#2.4. Merge the data sets using country as the matching variable 

df0921 <- left_join(df0919, df1721,by="country")

#2.5. Remove the summaries from the dataset (use created list)

df0921 <-df0921 %>% 
         filter((!country %in% summaries)) 

#2.6. Add ranking variable for 2021 (this will be used to decide on the cases kept)

df0921 <- df0921 %>% 
           mutate(rank_cut = rank(-`2021`, ties.method = "average"))

#2.7. save combined data set (this is the base for all plots)

comData_n = paste(here("created_data"), "/travel0921.csv", sep = "") #create a file location
write.csv(df0921, comData_n)   #Write data to CSV

#check data
head(df0921)


# 3. Visualisation 1 - Viewing the actual number of visits

#3.1. Create variables to allow us to choose the number of 2021 ranks 

cutrank1 <- 20 #number of countries that we want included in the chart
yrs_inc1 <- c(2009, 2021) # years that we want to include (can be any year from 2009-2021 apart from 2020)

#3.2 Create summary DF based on the number of countries

dfv1 <- df0921 %>% 
        filter (rank_cut<=cutrank1)

#3.3. Convert to long format

longdfv1 <- dfv1 %>% select("country", 
                            "2009", "2010", 
                            "2011", "2012", 
                            "2013", "2014", 
                            "2015", "2016", 
                            "2017", "2018",
                            "2019", "2021") %>% #2020 dropped due to no data 
                      pivot_longer(!country, 
                                   names_to = "year", 
                                   names_transform = list(year = as.factor), #Changed to factor to allow for it to be treated as a categor
                                   values_to = "visits") %>%
                      arrange(desc(visits)) %>% #order by the most visited to the least
                      filter(year %in% yrs_inc1) #removes years that we are not interested in as defined above
                      
#3.4. Create a dumbbell plot 

#3.4.1. Create a subtitle that changes with the data selected. 
subtitle1 <- paste("Showing the top", toString(cutrank1), "most visited countries in 2021")

#3.4.2. Match the aesthetics (basic plot)

p1 <- ggplot(longdfv1, 
            aes(x = visits, y = reorder(country, visits)))

#3.4.3. Add layers 
v1 <- p1 +   #lines to show the years are connected 
             geom_line(color="grey") + 
             #shows the year
             geom_point(aes(color = year), size = 3, alpha=0.8) +  
             #Legend at the bottom for ease
             theme(legend.position = "bottom") + 
             #Labels for the chart
             labs(x = "Number of visits from the UK (000s)", 
                  y = "Country", 
                  title = "Number of visits abroad from the UK in 2009 and 2021 by main country visited",
                  subtitle = subtitle1,
                  caption= "Source: Office for National Statistics International Passenger Survey") 

#Display visualisation 1             
v1

#3.5. Save the dumbbell plot 
ggsave(here("plots", "dumbell.png"), v1)

#3.6. Add the dumbbell to plotly to make it interactive

ip1 <- ggplotly(v1) #Use plotly for scrollover
#Display interactive visualisation 1
ip1

#3.7. Save the interactive plot
saveWidget(ip1, file='interactivedumbell.html')

#4.0. Visualisation 2 (final) - Use ranks to show the changes in popularity 

#4.1. Create variable to allow for filtering the data

cutrank2 <- 15 # Create variables to allow us to choose the number of 2021 ranks 
yrs_inc2 <- c(2009, 2021) #Create variables for the years to include in visualisation


#4.2. Create new DF of just ranks 

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
               r21 = rank(-`2021`, ties.method = "average")) %>% #r09-#r21 are the rank scores for each year
        select("country", 
               "r09", "r10", "r11", 
               "r12", "r13", "r14", 
               "r15", "r16", "r17", 
               "r18", "r19", "r21",
               'rank_cut') %>% #selecting the ranked data but 2020 dropped due to no data
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
               'rank_cut2'='rank_cut') #changing rank names to years to allow for changing to long format

#4.3. Create summary DF based on the number of countries

dfv2 <- dfv2 %>% 
        filter (rank_cut2 <=cutrank2)

#4.4. Change to Long format        

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
            filter(year %in% yrs_inc2) #remove years that are irrelevant

#4.5. Create colours for the visualisation 

#4.5.1. Retrieve data file containing the colours for each country/ continent
dataColour <- here("created_data", "colours.csv") #location of file 
dfColour <- read.csv(file = dataColour) # read in data

#4.5.2. Match the colour file to the data by country
longdfv2 <- left_join(longdfv2, dfColour,by="country")

#4.6. Create the final visualisation (slope plot)

#4.6.1. Create titles for the plot

subtitle2 <- paste("Changes in the rank for overall visits (business and travel) for the top", 
                   toString(cutrank2),
                   " most visited countries from the UK between"
                   ,toString(first(yrs_inc2)), 
                   "to" ,
                   toString(last(yrs_inc2))) # Label explaining the data 

Source2 <- bquote(paste(~bold('Source:'),"Office for National Statistics International Passenger Survey",
                        ~bold('Note:'),"Colours indicate country grouping; 2021 had disrupted data collection")) 
           #data source and interpretation notes 
  
title2 <- paste("Viva Espania! Spain remains the UK's most visited country")

#4.6.2. Match the aesthetics (basic plot)

p2 <- longdfv2 %>% 
      ggplot(aes(x=year, y=rank,group=country, colour= country))


#4.6.3. Add the formatting  

final <- p2+
         #slope lines
         geom_line(alpha = 0.5, linewidth=1.5) + 
         
         #End points for the data 
         geom_point(size = 3.5) + 
         
         #Labels to show the country and rank next to each of the countries 
         geom_text(data= longdfv2 %>% filter(year==(toString(first(yrs_inc2)))), #selects the first year
                   aes(x=year, y=rank,label = country), #add the country name 
                   size = 4, nudge_x = -0.35, fontface = "bold", color = "#000000") + # format so next to text
  
         geom_text(data= longdfv2 %>% filter(year==(toString(last(yrs_inc2)))), #selects the last year 
                   aes(x=year, y=rank,label = country), # add the country name
                   size = 4, nudge_x = 0.35, fontface = "bold", color = "#000000") + #format so next to text 
  
         geom_text(data= longdfv2 %>% filter(year ==(toString(first(yrs_inc2)))), #select the first year
                   aes(x=year, y=rank,label = rank), #add the rank position
                   size = 4, nudge_x = -0.05, fontface = "bold", color = "#000000") + #place next to country name
  
         geom_text(data= longdfv2 %>% filter(year ==(toString(last(yrs_inc2)))), #select the last year
                   aes(x=year, y=rank,label = rank), # add the rank to the chart
                   size = 4, nudge_x = 0.05,fontface = "bold", color = "#000000") + #formatting 
         
         #Reverse the scale so that the lowest ranked (most visited) is at the top
         scale_y_reverse( breaks=NULL) +
         
         #Use custom colours 
         scale_color_manual(values = setNames(longdfv2$hex_value, longdfv2$country)) +
  
         #Add labels
         labs(x = "Year", #X-axis
              y = "Rank of visits from the UK (1=most)", #y-axis
              title = title2, #main title
              subtitle = str_wrap(subtitle2, width=130), #subtitle 
              caption= Source2) + #caption with each on a new line
         
         #Specify the theme (fonts, background etc.)
         theme(plot.title = element_text(size=18, face = "bold"), #Main title font
         axis.text.x = element_text(size=14, face="bold", colour="black"), #X-axis element font
         axis.text.y = element_text(size=12, face="bold"), #y-axis years font
         axis.title = element_text(size=14,face = "bold"), #Axis title fonts 
         panel.background = element_blank(), #Make the background blank
         legend.position = "none",#Remove the legend 
         plot.caption = element_text(size = 10, hjust = 1)) #Caption font 
        
         
final

#4.6.4. Save the dumbbell plot 
ggsave(here("plots", "finalplot.png"), final)
