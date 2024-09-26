install.packages("stringr") 
install.packages("lubridate") 
if(!require("DT")) install.packages("DT")

install.packages("dplyr")
library(stringr)
library(dplyr)
library(lubridate)
library(tidyr)
library(DT)


FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(-`State/Parent NTD ID`, 
         -`Reporter Type`,
         -`Reporting Module`,
         -`TOS`,
         -`Passenger Paid Fares`,
         -`Organization Paid Fares`) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`) |>
  group_by(`NTD ID`,       # Sum over different `TOS` for the same `Mode`
           `Agency Name`,  # These are direct operated and sub-contracted 
           `Mode`) |>      # of the same transit modality
  # Not a big effect in most munis (significant DO
  # tends to get rid of sub-contractors), but we'll sum
  # to unify different passenger experiences
  summarize(`Total Fares` = sum(`Total Fares`)) |>
  ungroup()


EXPENSES <- readr::read_csv("2022_expenses.csv") |>
  select(`NTD ID`, 
         `Agency`,
         `Total`, 
         `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))

TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet="UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="UPT") |>
  drop_na() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

MILES <- readxl::read_xlsx("ridership.xlsx", sheet="VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="VRM") |>
  drop_na() |>
  group_by(`NTD ID`, `Agency`, `UZA Name`, 
           `Mode`, `3 Mode`, month) |>
  summarize(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))


#View
sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable() 

#Task 1
USAGE <- USAGE |>
  rename(metro_area='UZA Name')

#Task2
##find all distinct values
USAGE |>
  distinct(Mode)

##renaming for our purposes
USAGE <- USAGE |>
  mutate(Mode=case_when(
    Mode == "DR" ~ "Double Decker Buses", 
    Mode == "FB" ~ "Ferryboat",
    Mode == "MB" ~ "Bus",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "VP" ~ "Vanpool",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "LR" ~ "Light Rail",
    Mode == "YR" ~ "Hybrid Rail",
    Mode == "MG" ~ "Monorail and Automated Guideway",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "TR" ~ "Aerial Tramways",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Intercity Bus",
    Mode == "PB" ~ "Publico",
    Mode == "CC" ~ "Cable Car",
    TRUE ~ "Unknown"))
 
   
#View
sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()    

#Task 3
##Q1- agency with most VRM
topVRM_Ag <- USAGE |>
  group_by(Agency) |>
  summarize(sumVRM_Ag = sum(VRM,na.rm=TRUE)) |>
  arrange(desc(sumVRM_Ag)) |> 
  top_n(1,sumVRM_Ag) 

print(topVRM_Ag)

##Q2- Mode with most VRM
topVRM_Mo <- USAGE |>
  group_by(Mode) |>
  summarize(sumVRM_Mo = sum(VRM,na.rm=TRUE)) |>
  arrange(desc(sumVRM_Mo)) |> 
  top_n(1,sumVRM_Mo)

print('The transit mode with the most VRM is:')
print(topVRM_Mo)

##Q3- How many trips taken on NYC Subway in May 2024


subwayRides <- USAGE |>
  mutate( yr = year(month)) |>
  mutate(mo = month(month)) |>
  filter(yr==2024 & mo==5) |>
  group_by(Mode) |>
  summarize(UPTsum=sum(UPT,na.rm=TRUE)) |>
  filter(Mode == "Heavy Rail")

print(subwayRides)

##Q4- How much did NYC subway ridership fall between April 2019 and April 2020?

A2019_subwayRides <- USAGE |>
  mutate( yr = year(month)) |>
  mutate(mo = month(month)) |>
  filter(yr==2019 & mo==4) |>
  filter(Mode == "Heavy Rail") |>
  summarize(a2019_UPTsum=sum(UPT,na.rm=TRUE)) |>
  pull(a2019_UPTsum)

A2020_subwayRides <- USAGE |>
  mutate( yr = year(month)) |>
  mutate(mo = month(month)) |>
  filter(yr==2020 & mo==4) |>
  filter(Mode == "Heavy Rail") |>
  summarize(a2020_UPTsum=sum(UPT,na.rm=TRUE)) |>
  pull(a2020_UPTsum)
diff = A2019_subwayRides - A2020_subwayRides
cat(" In April 2019 there were", A2019_subwayRides, "riders.\n", "In April 2019 there were", A2020_subwayRides, "riders.\n","The difference between the years is", diff, "riders.")
   

#Task 4

##Interesting Fact 1 
cableCarsRides <- USAGE |>
  filter(Mode=="Cable Car") |>
  group_by(metro_area) |>
  summarize(ccUPT = sum(UPT,na.rm=TRUE)) |>
  arrange(desc(ccUPT)) |> 
  top_n(1,ccUPT) |>
  pull(ccUPT)

cableCarsCity <- USAGE |>
  filter(Mode=="Cable Car") |>
  group_by(metro_area) |>
  summarize(cccUPT = sum(UPT,na.rm=TRUE)) |>
  arrange(desc(cccUPT)) |> 
  top_n(1,cccUPT) |>
  pull(metro_area)

cat(cableCarsCity, "has the most number of cables cars rides, with a total of", cableCarsRides, "within this dataset")
   
##Interesting Fact 2    
NYC <- USAGE |>
  filter(metro_area=="New York--Jersey City--Newark, NY--NJ") |>
  group_by(Agency) |>
  summarize(nycUPT = sum(UPT,na.rm=TRUE)) |>
  arrange(desc(nycUPT)) |> 
  top_n(1,nycUPT) |>
  pull(Agency)
cat("The", NYC, "is the busiest transporation agency in NYC.")   
   
   
##Interesting Fact 3    
low <- USAGE |>
  filter(year(month)==2023) |>
  group_by(metro_area) |>
  summarize(lowUPT = sum(UPT,na.rm=TRUE)) |>
  arrange(lowUPT) |> 
  slice_min(lowUPT) |>
  pull(metro_area)

cat(low, "had the lowest amount of public transportation in 2023")     
   
   
#Task 5 Table Summarization
USAGE_2022_ANNUAL <- USAGE |> 
  filter(year(month)==2022) |>
  select(-`3 Mode`,-month) |>
  group_by(`NTD ID`, Agency, metro_area,Mode) |>
  summarize(sumUPT=sum(UPT,na.rm=TRUE),sumVRM=sum(VRM,na.rm=TRUE)) |>
  rename(UPT="sumUPT") |>
  rename(VRM="sumVRM")
  
  


##Joining tables and clean up

USAGE_AND_FINANCIALS <- FINANCIALS |>
  left_join(USAGE_2022_ANNUAL, by = "NTD ID") |>
  drop_na()

glimpse(USAGE_AND_FINANCIALS)
#TASK 6
##q1

transit <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode.y) |>
  summarize(totalUPT = sum(UPT, na.rm = TRUE)) |>
  ungroup() |>
  filter(totalUPT>400000)|>
  arrange(desc(totalUPT)) |>
  slice(1)
print(transit)

#q2
farebox_recovery <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode.y) |>
  summarize(
    totalFares = sum(`Total Fares`, na.rm = TRUE),
    totalExpenses = sum(Expenses, na.rm = TRUE)) |>
  ungroup() |>
  mutate(fareboxRatio = ifelse(totalExpenses > 0, totalFares / totalExpenses, NA)) |>  
  arrange(desc(fareboxRatio)) |>  
  slice(1)  

print(farebox_recovery)

##q3
exp2UPT <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode.y) |>
  summarize(
    totalUPT = sum(UPT, na.rm = TRUE),
    totalExpenses = sum(Expenses, na.rm = TRUE)) |>
  ungroup() |>
  filter(totalUPT>400000)|>
  mutate(exp2UPT_ratio = ifelse(totalExpenses > 0 & totalUPT>0,  totalExpenses /totalUPT, NA)) |> #taking out any UPT that is 0. 
  arrange(exp2UPT_ratio) |>  
  slice(2)  

print(exp2UPT)

##q4
fare2UPT <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode.y) |>
  summarize(
    totalUPT = sum(UPT, na.rm = TRUE),
    totalFares = sum(`Total Fares`, na.rm = TRUE) ) |>
  ungroup() |>
  filter(totalUPT>400000)|>
  mutate(fare2UPT_ratio = ifelse(totalFares > 0 & totalUPT>0, totalFares /totalUPT, NA)) |> #taking out any UPT that is 0. 
  arrange(fare2UPT_ratio) |>  
  slice(1)  

print(fare2UPT)

##q5
exp2VRM <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode.y) |>
  summarize(
    totalVRM = sum(VRM, na.rm = TRUE),
    totalExpenses = sum(Expenses, na.rm = TRUE)) |>
  ungroup() |>
  mutate(exp2VRM_ratio = ifelse(totalExpenses > 0 & totalVRM>0, totalExpenses /totalVRM, NA)) |> #taking out any UPT that is 0. 
  arrange(exp2VRM_ratio) |>  
  slice(1)  

print(exp2VRM)

##q6
fare2VRM <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode.y) |>
  summarize(
    totalVRM = sum(VRM, na.rm = TRUE),
    totalFares = sum(`Total Fares`, na.rm = TRUE)) |>
  ungroup() |>
  mutate(fare2VRM_ratio = ifelse(totalFares > 0 & totalVRM>0, totalFares /totalVRM, NA)) |> #taking out any UPT that is 0. 
  arrange(desc(fare2VRM_ratio)) |>  
  slice(1)  

print(fare2VRM)

#efficiency
efficiency <- transit |>
  pull(Agency)
  
cat("In my opinion, the ",efficiency, "is the most efficient because it has the highest number of UPT. We do not know the complexity of volume in planning. There are more schedules to consider in a larger city, so the fact it has the most UPT means it the most efficient in my eyes.")