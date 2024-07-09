#Code for the article Kazlou T., Cherp A., Jewell J.
#Feasible deployment of carbon capture and storage and the requirements of climate targets

#Producing a feasibility space for CCS deployment in the acceleration phase (replication of Figure 3)
#Working directory should be set to the directory containing input files
#Necessary packages (should be installed)
library(tidyverse)
library(mosaic)
library(ggdensity)
library(readxl)
`%nin%` = Negate(`%in%`)

scenarios <- read.csv("CCS_data.csv") #set the path to "CCS_data.csv"
wind <- read.csv("IEA_electricity.csv") #set the path to "IEA_electricity.csv"

####Preparing the dataframe to display CCS acceleration in the IPCC AR6 scenarios####
abs <- scenarios %>%
  group_by(model_scenario) %>%  
  filter(Year == 2030) %>% mutate(share2030 = Value/21072*100) #21.072 Gt of capturable emissions - current market (calculated from EDGAR v6, see Methods)

abs2040 <- scenarios %>% 
  filter(Year == 2040) %>%
  rename(Value2040 = Value) %>% select(model_scenario, Value2040)

perc_decades <- scenarios %>% 
  rename(value=Value) %>% 
  mutate(value = (((lead(value)/value)^(0.1)-1)*100)) %>% 
  filter(Year == 2030) %>% pivot_wider(names_from = Year, values_from = value) %>% rename(u2030 = `2030`) %>% select(-temp)

data <- left_join(abs, perc_decades, by = "model_scenario") %>% select(-Year)
data <- left_join(data, abs2040, by = "model_scenario")

####Preparing the dataframe for the feasibility space - in this example, we only use wind power data####
iea <- wind %>% filter(Country == "WD" & Fuel %in% c("Solar", "Nuclear", "Wind", "Total"))
iea_total <- iea %>% filter(Fuel == "Total") %>% select(-c("Country", "Fuel")) %>% rename(Total = Value)
iea <- iea %>% filter(Fuel == "Wind") %>% left_join(., iea_total, by = c("Year")) %>% mutate(mshare = Value/Total*100) 
wind_o2 <- iea %>% filter(Fuel == "Wind" & Year >= 1995) %>% mutate(front10 = (lead(Value, n = 10)/Value)^(1/10)-1) %>% 
  filter(Year <= 2010) %>% 
  rename(Share = mshare) %>% mutate(front10=front10*100, share_max = Share*2.5) %>%
  select(Fuel, Year, share_max, front10, Value) %>% 
  mutate(dashed = ifelse(Year >= 2005, "y", "n"), solid = ifelse(Year <= 2006, "y", "n")) %>% rename(u2030 = front10)

ref_o2_2 <- data.frame(Reference = c("Wind", "Wind", "Wind", "Wind"),
                       ID = c("0.3%, W98-08", "0.5%, W00-10", "0.9%, W03-13", "1.8%, W06-16"),
                       share2030 = c(0.3, 0.5, 0.9, 1.75),
                       u2030 = c(29.8,27.1,25.9,21.7)) #this is also the main input for Table 2

####Plotting####
figure3 <- ggplot(data = subset(data, temp != "2.5°C"), aes(share2030, u2030))+
  annotate("segment", x = -0.001, y = -0, xend = 0.5, yend = 32.5, linewidth = 0.6, color = "white", alpha = 0) + #this is a dummy
  geom_density_2d_filled(aes(alpha = after_stat(level), fill = after_stat(level)), contour_var = "ndensity")+
  geom_point(aes(colour = temp), size = 1, alpha = 0.7)+
  scale_colour_manual("IPCC AR6 Scenarios",labels = c("1.5°C", "2.0°C", "2.5°C"), values = c("1.5°C" = "#a6cee3", "2.0°C" = "#7fc97f", "2.5°C" = "#ef3b2c"))+
  geom_point(data=ref_o2_2, aes(x = share2030, y = u2030, shape = Reference), size = 1) +
  scale_shape_manual("Reference cases", values = 15)+
  geom_line(data=subset(wind_o2, solid == "y"), aes(x=share_max, y = u2030, group = Fuel), color = "black")+
  geom_line(data=subset(wind_o2, dashed == "y"), aes(x=share_max, y = u2030, group = Fuel), color = "black", linetype = "dashed")+
  annotate("segment", x = 1.75, y = 0, xend = 1.75, yend = 21.7, linewidth = 0.6, color = "black") +
  facet_grid(rows=vars(temp))+
  scale_alpha_manual("Density of IPCC pathways,\n scaled to a maximum of 1", values = seq(0, 0.9, 0.1))+
  scale_fill_viridis_d("Density of IPCC pathways,\n scaled to a maximum of 1", option = "plasma")+
  theme_bw(base_size = 7)+
  theme(legend.position="bottom",  legend.box="vertical", legend.spacing.y = unit(-0.25, "cm"), panel.grid = element_blank(), strip.background = element_blank(),
        strip.text.y = element_blank())+ 
  labs(x = "CCS share of market potential (%) in 2030", y = "CCS 10-yr CAGR in 2030-40, %")+
  coord_cartesian(ylim = c(0,60), xlim = c(0,10))+
  scale_y_continuous(breaks = c(10,20,30,40,50), expand = c(0.005,0))+ 
  scale_x_continuous(breaks = c(1:9), expand = c(0,0))

ggsave("figure3.png", dpi = 500, width = 140, height = 202, unit = "mm", path = "./") 
#saves a PNG file of Figure 3 (with wind power as the reference case) in the working directory


####Calculating the number of scenarios within the frontier####
scenarios <- data %>% select(model_scenario, temp, share2030, u2030) %>% filter(temp != "2.5°C") #218 1.5 and 423 2.0 scenarios
wind_line <- wind_o2 %>% filter(Year %in% c(1995:2006)) %>% select(c("Fuel", "share_max", "u2030")) %>% rename(share2030 = share_max)
#first, remove scenarios that are filtered out as infeasible in the previous step
scenarios1 <- scenarios %>% filter(share2030 <= 1.73)
#second, we compare the remaining scenarios with the reference case and group them into two groups: above and within the feasibility space
scenarios1$wind_value <- approxfun(wind_line$share2030, wind_line$u2030)(scenarios1$share2030)
scenarios1$group <- with(scenarios1, 1 + (u2030 > wind_value))
scenarios1$group <- c('within', 'above')[scenarios1$group]
