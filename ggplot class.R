
PropertyInspections <- 
  read.csv("https://data.louisvilleky.gov/sites/default/files/PM_Cases_Verbose.csv")

library(ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(lubridate)


#is our data fairly clean? 
#no it isn't, it has quite a few missing values


PropertyInspections <-
  PropertyInspections[complete.cases(PropertyInspections),] #filter to only non-NAs

#plot numeric data as-is
#which columns are numbers?
#which data makes sense? 
#what type of plot makes sense?

#a density plot of violation counts
ggplot(PropertyInspections, #what data do we use?
       aes(VIOLATION_COUNT))+ #what are we graphing within the data set?
  geom_density() #what type of graph --- the bare bones of ggplot 

#the majority have less than 10 violations

ggplot(PropertyInspections,
       aes(CASE_FEES))+
  geom_density()

#much more closely bunch toward zero
#ummmmmm, less than zero?


PropertyInspections$INSPECTIONADD <- #turn into a date with lubridate
  ymd_hms(PropertyInspections$INSPECTIONADD)


# now plot date against how many violations
ggplot(PropertyInspections,
       aes(INSPECTIONADD,
           VIOLATION_COUNT))+
geom_point()

ggplot(PropertyInspections,
       aes(INSPECTIONADD,
           CASE_FEES))+
geom_point()


#what are some possible reasons why there are way more in 2012?
#are there more records overall? 
#are violations grouped into one record now?
#are records removed for some reason from the earlier years? 
#what are there so many more case fees totalling $25K or more?

PropertyInspectionsBY_YEAR <-
  PropertyInspections %>%
  mutate(Year = year(INSPECTIONADD)) %>% #create a column with the year the inspection was added
  filter(Year != "2017" & #get rid of 2017, not complete
           Year != "2002") %>% # get rid of 2002, not complete -- probably should automate check in a real script
  group_by(Year) %>%
  summarise(MedianNumberOfRecords = median(VIOLATION_COUNT),
            MeanNumberOfRecords = mean(VIOLATION_COUNT)) %>%
  gather(key = Metric, # NOTE: GGPLOT NEEDS LONG DATA FOR A LOT OF ITS GOOD FUNCTIONALITY
         value = Measure,
         2:3) %>% #you can pass to ggplot through a pipe
  ggplot(aes(Year,
             Measure)) +
  geom_point(aes(color = Metric), # TELLING GGPLOT TO COLOR BY OUR CATEGORY
             size = 4)

PropertyInspectionsBY_YEAR # looks like the median is riding slower than the average
#the records with the highest number of violations 
# have much more than they used to




PropertyInspectionsBY_YEARII <-
  PropertyInspections %>%
  mutate(Year = year(INSPECTIONADD)) %>% 
  filter(Year != "2017" & 
           Year != "2002") %>% 
  group_by(Year) %>%
  summarise(MedianNumberOfViolations = median(VIOLATION_COUNT),
            MeanNumberOfViolations = mean(VIOLATION_COUNT),
            NumberOfRecords = n()) %>% #count how many records
  gather(key = Metric, 
         value = Measure,
         2:4) # last four columns this time


# FACET WRAP
ggplot(PropertyInspectionsBY_YEARII,
       aes(Year,
           Measure)) +
  geom_point(aes(color = Metric), # telling ggplot to color by our category 
             size = 4)+
  facet_wrap(~Metric,
             scales = "free") # with this addition we can let the y axis float on its own

#so, 2016 was just ridiculous for the number of records, the mean & median number of 
#violations per record. however, the number of records had previously been declining

TopTwentyFiveWorstAddressesByYear <-
  PropertyInspections %>%
  mutate(Year = year(INSPECTIONADD),
         StreetAddressANDYear = paste(FULLSTREETADDRESS,
                                      " (",
                                      Year,
                                      ")",
                                      sep = "")) %>% #give me a reasonable label
  group_by(StreetAddressANDYear) %>%
  summarise(TotalViolations = sum(VIOLATION_COUNT)) %>%
  top_n(n = 25, 
        wt = TotalViolations) %>%
  arrange(-TotalViolations,
          StreetAddressANDYear) #order them so the graph sorts correctly
              


#COORD FLIP AND XLAB & YLAB
ggplot(TopTwentyFiveWorstAddressesByYear,
       aes(StreetAddressANDYear,
           TotalViolations))+
  geom_bar(stat="identity")+
  coord_flip()+ 
  xlab("Number of Violations") +
  ylab("Address & Year")


#same thing with Case fees


TopTwentyFiveWorstAddressesByYearDollars <-
  PropertyInspections %>%
  mutate(Year = year(INSPECTIONADD),
         StreetAddressANDYear = paste(FULLSTREETADDRESS,
                                      " (",
                                      Year,
                                      ")",
                                      sep = "")) %>% #give me a reasonable label
  group_by(StreetAddressANDYear) %>%
  summarise(TotalCaseFees = sum(CASE_FEES)) %>%
  top_n(n = 25, 
        wt = TotalCaseFees) %>%
  arrange(-TotalCaseFees,
          StreetAddressANDYear)
              


#COORD FLIP AND XLAB & YLAB
ggplot(TopTwentyFiveWorstAddressesByYearDollars,
       aes(StreetAddressANDYear,
           TotalCaseFees))+
  geom_bar(stat="identity")+
  coord_flip()+ 
  xlab("Total Case Fees") +
  ylab("Address & Year")




#why are case fees so high?
#maybe most closed fees end up being zero?

PropertyInspectionsOpenANDClosed <-
  PropertyInspections %>%
   mutate(Year = year(INSPECTIONADD)) %>%
   filter(Year != "2017" & 
           Year != "2002") %>% 
  group_by(Year,
           CASESTATUS) %>%
  summarise(TotalFees = sum(CASE_FEES))

ggplot(PropertyInspectionsOpenANDClosed,
       aes(Year,
           TotalFees/1000000,#you can adjust the data here when it is easier
           fill = CASESTATUS))+ #which variable is coloring the bars?
  geom_bar(stat = "identity")+ #use the number as-is, no summary
  ylab("Total Fees (millions)")+
  theme_tufte()+
  annotate("text",
           x = 2010,
           y = 10,
           label = "no old closed $$$$?????!!!",
           size = 5,
           color = "red",
           alpha = 1/3)


ggplot(subset(PropertyInspectionsOpenANDClosed,
              CASESTATUS == "Closed"),
       aes(Year,
           TotalFees/1000000,#you can adjust the data here when it is easier
           fill = CASESTATUS))+ #which variable is coloring the bars?
  geom_bar(stat = "identity")+ #use the number as-is, no summary
  ylab("Total Fees (millions)")+
  theme_tufte()


PropertyInspectionsOpenANDClosedCount <-
  PropertyInspections %>%
   mutate(Year = year(INSPECTIONADD)) %>%
   filter(Year != "2017" & 
           Year != "2002") %>% 
  group_by(Year,
           CASESTATUS) %>%
  summarise(TotalViolations = sum(VIOLATION_COUNT))

ggplot(PropertyInspectionsOpenANDClosedCount,
       aes(Year,
           TotalViolations,#you can adjust the data here when it is easier
           fill = CASESTATUS))+ #which variable is coloring the bars?
  geom_bar(stat = "identity")+ #use the number as-is, no summary
  ylab("Total Violations")+
  theme_tufte()


ggplot(subset(PropertyInspectionsOpenANDClosedCount,
               CASESTATUS == "Closed"),
       aes(Year,
           TotalViolations,
           fill = CASESTATUS))+ 
  geom_bar(stat = "identity")+ 
  ylab("Total Violations")+
  theme_tufte()


#it is very likely that most are settled for no money 
#however, there isn't a record of what the original fees were
# while the number of violations appears to have increased
#it seems less interesting on first glance without additional associated money 
#some of the Result = FAILED don't have fees, so what does that mean?





#DATA HILARITY
MoneyMakingOpportunity <-
  PropertyInspections %>%
  filter(CASESTATUS == "Closed") %>%
  group_by(FULLSTREETADDRESS) %>%
  summarise(TotalFees = sum(CASE_FEES),
            TotalViolations = sum(VIOLATION_COUNT)) %>%
  mutate(ifelse(TotalFees < 0,
                "LessThanZero",
                "MoreThan"))

colnames(MoneyMakingOpportunity)[4] <- 
  "RelativeToZero"

ggplot(MoneyMakingOpportunity,
       aes(TotalFees,
           TotalViolations))+
  geom_point(aes(color = RelativeToZero),
             alpha= 1/10)+
  theme_economist()+
  geom_vline(xintercept = 0, 
             color = "red")+
  xlab("Total Case Fees ($)")+
  ylab("Number of Violations")+
  scale_color_economist()+
  scale_x_continuous(limits = c(-500,30000))


#questionssssssssss
#does closed mean they paid these fees?
#where does this money go?
#if there are negative values for fees...
#what does that say about overall data integrity?



