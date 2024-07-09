#Code for the article Kazlou T., Cherp A., Jewell J.
#Feasible deployment of carbon capture and storage and the requirements of climate targets

#Calculating failure rates of CCS projects: historical (overall) and subsector-adjusted (by application) (replication of Supplementary Table 1)
#Working directory should be set to the directory containing input files
#Necessary packages (should be installed)
library(dplyr)

data <- read.csv("CCS_Projects_database.csv") #set the path to the source file "CCS_Projects_database.csv"

##Calculating the historical failure rate of CCS projects in the first wave: overall and by subsector
#Preparing the data
failure_wave1 <- data %>% filter(!is.na(AnnouncedCapacity | ActualCapacity) & Status != "Future" & ProjectStart <= 2017) %>% 
  mutate(AnnouncedCapacity = ifelse(is.na(AnnouncedCapacity), ActualCapacity, AnnouncedCapacity),
         ActualCapacity = ifelse(is.na(ActualCapacity), 0, ActualCapacity))
#Calculating the overall failure rate of CCS projects in the first wave
wave1_total <- failure_wave1 %>% select(Name, ActualCapacity, AnnouncedCapacity) %>%
  summarise(Announced = sum(AnnouncedCapacity*365/1000000), Implemented = sum(ActualCapacity*365/1000000), N = n()) %>%
  mutate(Failure_Rate = 100-Implemented/Announced*100, Subsector = "Total") %>% select(Subsector, N, Announced, Implemented, Failure_Rate) 
#Calculating failure rates by subsector
wave1_grouped <- failure_wave1 %>% 
  select(Name, ActualCapacity, AnnouncedCapacity, Subsector) %>% group_by(Subsector) %>% 
  summarise(Announced = sum(AnnouncedCapacity*365/1000000), Implemented = sum(ActualCapacity*365/1000000), N = n()) %>%
  mutate(Failure_Rate = 100-Implemented/Announced*100) %>% select(Subsector, N, Announced, Implemented, Failure_Rate) %>%
  arrange(desc(N)) 
#Identifying subsectors with a small N of projects (N<5) and grouping them together
wave1_low_n <- wave1_grouped %>% filter(N < 5) %>% 
  summarise(N = sum(N), Announced = sum(Announced), Implemented = sum(Implemented), Failure_Rate = 100-Implemented/Announced*100) %>%
  mutate(Subsector = "Other")
#Combining the two
wave1 <- rbind(wave1_grouped, wave1_low_n, wave1_total) %>% filter(N > 5)


##Calculating the subsectoral failure rate of CCS projects in the second wave
#Preparing the data
wave2 <- data %>% filter(!is.na(AnnouncedCapacity | ActualCapacity) & Status == "Future") 

wave2_grouped <- wave2 %>% 
  select(Name, AnnouncedCapacity, Subsector) %>% group_by(Subsector) %>% 
  summarise(Announced = sum(AnnouncedCapacity*365/1000000), N = n()) %>%
  left_join(., wave1, by = c("Subsector"))  %>%
  select(Subsector, N.x, Announced.x, Failure_Rate) %>% rename(N = N.x, Announced = Announced.x) %>% 
  arrange(desc(N))

#calculating total announced capacity
wave2_total <- wave2_grouped %>% summarise(N=sum(N), Announced = sum(Announced)) %>% mutate(Subsector = "Total", Failure_Rate = NA) 
#applying the historical failure rate from the previous part to projects in subsectors with insufficient historical data (N<5 in wave1)
wave2_other <- wave2_grouped %>% filter(N<5 | is.na(Subsector)) %>% summarise(N=sum(N),Announced = sum(Announced)) %>% mutate(Subsector = "Other", Failure_Rate = wave1$Failure_Rate[wave1$Subsector == "Total"])
#calculating the subsector-adjusted failure rate of current plans
wave2_failure <- rbind(wave2_grouped, wave2_other, wave2_total) %>% filter(N >= 5 & !is.na(Subsector)) %>%
  mutate(Failure_Rate = ifelse(is.na(Failure_Rate) & Subsector != "Total", wave1$Failure_Rate[wave1$Subsector == "Total"], Failure_Rate)) %>%
  mutate(Failure_Rate = ifelse(Subsector == "Total", 100-sum(coalesce(Announced*((100-Failure_Rate)/100),0))/Announced[Subsector == "Total"]*100, Failure_Rate))

#combining the two waves of interest in CCS to replicate Supplementary Table 1
wave2_grouped2 <- wave2_grouped %>% rename(N_2030 = N, Announced_2030 = Announced) %>% select(-Failure_Rate)
st1_total <- full_join(wave1_grouped, wave2_grouped2, by = "Subsector")
st1_other <- st1_total %>% filter((N < 5 | is.na(N)) & (N_2030 < 5 | is.na(N_2030))) %>% 
  summarise(N=sum(N, na.rm = TRUE),Announced = sum(Announced, na.rm = TRUE),N_2030 = sum(N_2030, na.rm = TRUE), 
            Announced_2030 = sum(Announced_2030, na.rm = TRUE), Implemented = sum(Implemented, na.rm = TRUE)) %>% 
  mutate(Subsector = "Other", Failure_Rate = 100-Implemented/Announced*100)  %>% 
  select(-Implemented)
st1 <- st1_total %>% filter(!((N < 5 | is.na(N)) & (N_2030 < 5 | is.na(N_2030)))) %>% 
  select(-Implemented) %>% 
  rbind(., st1_other)
st1_summary <- st1 %>%
  summarise(N=sum(N, na.rm = TRUE),Announced = sum(Announced, na.rm = TRUE),N_2030 = sum(N_2030, na.rm = TRUE), 
                               Announced_2030 = sum(Announced_2030, na.rm = TRUE), 
                               Failure_Rate = 100-sum(wave1_grouped$Implemented)/Announced*100, Subsector = "Total")
 
st1_final <- rbind(st1, st1_summary)

write.csv(st1_final, "./st1_final.csv") 
#saves a CSV file of Supplementary Table 1 in the working directory


