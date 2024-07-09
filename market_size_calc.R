#Code for the article Kazlou T., Cherp A., Jewell J.
#Feasible deployment of carbon capture and storage and the requirements of climate targets

#Estimating the future potential market for CCS technologies based on scenario emission data (replication of Supplementary Figure 4)
#Working directory should be set to the directory containing input files
#Necessary packages (should be installed)

library(tidyverse)
library(writexl)
`%nin%` = Negate(`%in%`)

scenario <- read_csv("single_scenario.csv") #set the path to the source file "single_scenario.csv"

#Step 1: calculating final (net) CO2 emissions
step1 <- scenario  %>%
  filter(Variable %in% c("Emissions|CO2|Energy|Demand|Industry", "Emissions|CO2|Energy|Supply",
                         "Emissions|CO2|Industrial Processes", "Emissions|CO2|Waste", "Emissions|CO2|Other")) %>% 
  select(model_scenario, Variable, Category, `2020`, `2030`, `2040`, `2050`, `2060`, `2070`, `2080`, `2090`, `2100`) %>% 
  pivot_longer(cols = -c("model_scenario", "Category", "Variable"), names_to = "Year") %>% 
  group_by(model_scenario, Year, Category) %>% 
  summarise(value = sum(value))

#Step 2: creating a dataset for non-NET CCS AND changing zeros into actual values
step2 <- scenario %>% select(-...1) %>% mutate(Variable = chartr("|", "_", Variable)) %>% 
  pivot_longer(cols = -c("model_scenario", "Category", "Variable"), names_to = "Year") %>%
  pivot_wider(names_from = Variable) %>% rename_with(make.names) %>% 
  group_by(model_scenario, Year, Category) %>%
  mutate(nonnetCCS = Carbon.Sequestration_CCS  - coalesce(Carbon.Sequestration_CCS_Biomass, 0), Year = as.factor(Year)) %>% 
  select(c("model_scenario", "Year", "Category", "nonnetCCS"))

step2$nonnetCCS[step2$nonnetCCS < 34.62] <- 34.62

#Step 3: subsetting DACCS
step3 <- scenario  %>% 
  filter(Variable == "Carbon Sequestration|Direct Air Capture") %>% 
  select(model_scenario, Variable, Category, `2020`, `2030`, `2040`, `2050`, `2060`, `2070`, `2080`, `2090`, `2100`) %>%
  pivot_longer(cols = -c("model_scenario", "Category", "Variable"), names_to = "Year") %>% 
  group_by(model_scenario, Year, Category) %>% summarise(value = abs(value)) 

#Step 4-5: subsetting BECCS and adding it to DACCS from the previous step
step4 <- scenario %>%
  filter(Variable == "Carbon Sequestration|CCS|Biomass") %>%
  select(model_scenario, Variable, Category, `2020`, `2030`, `2040`, `2050`, `2060`, `2070`, `2080`, `2090`, `2100`) %>%
  pivot_longer(cols = -c("model_scenario", "Category", "Variable"), names_to = "Year") %>% 
  group_by(model_scenario, Year, Category) %>% summarise(value = abs(value)) 
step4$Year <- as.factor(step4$Year)

step5 <- rbind(step3, step4) %>% 
  group_by(model_scenario, Year, Category) %>% summarise(value = sum(value)) %>% rename(bedaccs = value)

#Step 6-7: combining the three resulting datasets
step6 <- left_join(step1, step2, by = c("model_scenario", "Year", "Category")) #nonnetCCS and net emissions
step7 <- left_join(step5, step6, by = c("model_scenario", "Year", "Category")) %>% rename(f_emissions = value) %>%
  unique() %>% ungroup()  #nonnetCCS, net emissions, and netCCS

#Step 8: creating non-CCS NET dataset
step8 <- scenario  %>%
  filter(Variable %in% c("Carbon Sequestration|Enhanced Weathering", "Carbon Sequestration|Land Use",
                         "Carbon Sequestration|Other", "Carbon Sequestration|Feedstocks")) %>% 
  select(model_scenario, Variable, Category, `2020`, `2030`, `2040`, `2050`, `2060`, `2070`, `2080`, `2090`, `2100`) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = -c("model_scenario", "Category", "Variable"), names_to = "Year") %>% 
  group_by(model_scenario, Year, Category) %>% summarise(value = sum(abs(value))) %>%
  rename(other_net = value) 

#Step 9: combining the two and calculating the final market
step9 <- left_join(step7, step8, by = c("model_scenario", "Year", "Category")) %>% 
  mutate(marketsize1 = coalesce(f_emissions,0) + coalesce(bedaccs,0) + coalesce(other_net,0) + coalesce(nonnetCCS,0) + coalesce(bedaccs,0)) 
  
#Plotting
step10 <- step9 %>%  
  mutate(f_emissions_act = f_emissions+bedaccs+coalesce(other_net,0), other_net = other_net*(-1), bedaccs = bedaccs*(-1)) %>% 
  select(-marketsize1) %>% 
  pivot_longer(cols = -c("model_scenario", "Category", "Year"), names_to = "Variable")
step10$Variable <- factor(step10$Variable, levels = c("f_emissions","nonnetCCS", "f_emissions_act", "other_net", "bedaccs" )) 

market_plot_sf <- ggplot(step10, aes(x=Year, y = value/1000))+
  geom_area(data = subset(step10, Variable != "f_emissions"), aes(fill = Variable, group = Variable), alpha = 0.5)+
  geom_line(data = subset(step10, Variable == "f_emissions"), aes(color = Variable, group = model_scenario))+
  scale_fill_manual("Market\nsegments", labels = c("Captured emissions", "Unabated capturable emissions", "Other NET", "Captured negative emissions"), values = c("skyblue", "grey", "pink", "chartreuse4"))+
  scale_color_manual("", labels = c("Net-CO2 emissions in CCS-applicable sectors"), values = c("black"))+
  labs(y = "Market size, Gt/yr (negative for NETs)")+
  facet_wrap(vars(model_scenario), ncol = 2)+
  theme_bw(base_size = 7)+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical", axis.title.x = element_blank(), panel.grid = element_blank(),
        strip.text.x = element_text(size = 7))
ggsave("sf4.png", dpi = 500, width = 140, height = 140, unit = "mm", path = "./")
#saves a PNG file of Supplementary Figure 2 in the working directory
