#Code for the article Kazlou T., Cherp A., Jewell J.
#Feasible deployment of carbon capture and storage and the requirements of climate targets

#Producing a feasibility space for CCS deployment in the stable growth phase (replication of Extended Data Figure 4)
#Working directory should be set to the directory containing input files
#Necessary packages (should be installed)

library(tidyverse)
library(ggrepel)
library(patchwork)
library(writexl)
`%nin%` = Negate(`%in%`)

####Preparing the data####
ref10 <- read_csv("formatted_scenario_data.csv") %>% #set the path to "formatted_scenario_data.csv"
  select(Country, temp)
results10 <- read_csv("formatted_scenario_data_fit.csv")  #set the path to "formatted_scenario_data_fit.csv"
market <- read_csv("CCS_market.csv")  #set the path to "CCS_market.csv"

####normalising to the current market size####
normalisation_2022 <- results10 %>% 
  mutate(G = ifelse(L < 35, 0, G), G_2022 = G/21079*100, L = as.integer(L/1000))

####normalising G to the current and future market sizes####
normalisation_s <- market %>% 
  select(-...1) %>% unique() %>% 
  dplyr::rename(Country = model_scenario) %>% left_join(., results10, by = "Country") %>% filter(Fit == "S") %>% group_by(Country) %>%
  filter(Year %in% c(TMax_top, TMax_bot)) %>% ungroup() %>% group_by(Country) %>%
  mutate(diff = marketsize-lag(marketsize)) %>% filter(Year == max(Year) & diff != 0) %>%
  mutate(marketsize_new = diff*(TMax-TMax_bot)/10+(marketsize-diff)) %>% #future market size (at TMax)
  mutate(G = ifelse(G < 35, 0, G), G_TMax = G/marketsize_new*100) %>% 
  select(Country, Fit, G_TMax)
#now doing the same for the Gompertz fit (separately because it has a different TMax)
normalisation_g <- market %>%
  select(-...1) %>% unique() %>% 
  dplyr::rename(Country = model_scenario) %>% left_join(., results10, by = "Country") %>% filter(Fit == "G") %>% group_by(Country) %>%
  filter(Year %in% c(TMax_top, TMax_bot)) %>% ungroup() %>% group_by(Country) %>%
  mutate(diff = marketsize-lag(marketsize)) %>% filter(Year == max(Year) & diff != 0) %>%
  mutate(marketsize_new = diff*(TMax-TMax_bot)/10+(marketsize-diff)) %>% 
  mutate(G = ifelse(G < 35, 0, G), G_TMax = G/marketsize_new*100)  %>% 
  select(Country, Fit, G_TMax)
#combining the results for G_TMax
normalisation_tmax <- rbind(normalisation_s, normalisation_g)

#combining the results for both fits and calculating average parameters
normalisation <- normalisation_2022 %>% left_join(., normalisation_tmax, by = c("Country", "Fit")) %>% 
  select(c("Country", "Fit", "L", "TMax", "dT", "G", "G_TMax", "G_2022")) %>% 
  group_by(Country) %>% summarise(L = mean(L, na.rm = TRUE), TMax = mean(TMax, na.rm = TRUE), dT = mean(dT, na.rm = TRUE), G = mean(G, na.rm = TRUE), G_TMax = mean(G_TMax, na.rm = TRUE),
                                 G_2022 = mean(G_2022, na.rm = TRUE)) %>% 
  left_join(., ref10, by = "Country") %>% unique()
write.csv(normalisation, "normalisation.csv")
####Plotting####
dots15 <- ggplot(subset(normalisation, temp == "1.5°C"), aes(TMax, G_TMax))+
  annotate(geom = "rect", ymin=0.5, ymax=1.3, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.15)+ #IQR solar
  annotate(geom = "rect", ymin=0.6, ymax=1.7, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.1)+  #IQR wind
  annotate(geom = "rect", ymin=1.3, ymax=6.0, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.05)+ #IQR nuclearv
  geom_density_2d_filled(aes(alpha = after_stat(level), fill = after_stat(level)), contour_var = "ndensity")+
  scale_fill_manual("Density of IPCC pathways,\n scaled to a maximum of 1", values = c("#FFFFFF", "#EFEBF6", "#E6D6EE", "#E2C2E3", "#E5B6D1", "#ECB1BA", "#F4B1A0", "#FCBB7F","#FED35C", "#F2F940"))+
  annotate("text", y = 0.85, x = 2094, label = "Solar National (IQR)", color = "black", size = 6/.pt, hjust = "right", alpha=0.45) +
  annotate("text", y = 1.3, x = 2026, label = "Nuclear Global G", color = "black", size = 6/.pt, hjust = "left") +
  annotate("text", y = 0.85, x = 2026, label = "Wind Global G", color = "black", size = 6/.pt, hjust = "left") +
  annotate("text", y = 0.37, x = 2026, label = "Solar Global R3", color = "black", size = 6/.pt, hjust = "left") +
  annotate("text", y = 1.6, x = 2094, label = "Wind National (IQR)", color = "black", size = 6/.pt, hjust = "right", alpha=0.35) +
  annotate("text", y = 5.8, x = 2094, label = "Nuclear National (IQR)", color = "black", size = 6/.pt, hjust = "right", alpha=0.25) +
  geom_point(aes(colour = temp, size = L), alpha = 0.7)+ 
  scale_color_manual("Temperature",labels = c("1.5°C", "2.0°C"), values = c("1.5°C" = "#a6cee3", "2.0°C" = "#7fc97f"), guide = "none")+ 
  scale_size_binned("Saturation level, Gt", breaks = c(10,20,30), range = c(0,1.5))+
  labs(y="Max growth rate as share of the market at TMax")+
  geom_hline(yintercept = 1.42, color = "black", linewidth = 0.3)+ #nuclear (gen) G for the global market
  geom_hline(yintercept = 0.5, color = "black", linewidth = 0.3)+
  geom_hline(yintercept = 0.7, color = "black", linewidth = 0.3)+
  scale_alpha_manual("Density of IPCC pathways,\n scaled to a maximum of 1", values = c(0,1,1,1,1,1,1,1,1,1))+ 
  scale_y_continuous(limits = c(0,7), breaks = seq(1,6,1), expand = c(0,0))+
  scale_x_continuous(limits = c(2025,2095), breaks = seq(2030,2090,10), expand = c(0,0))+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal", panel.grid = element_blank())

dots20 <- ggplot(subset(normalisation, temp == "2.0°C"), aes(TMax, G_TMax))+
  annotate(geom = "rect", ymin=0.5, ymax=1.3, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.15)+ #IQR solar
  annotate(geom = "rect", ymin=0.6, ymax=1.7, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.1)+  #IQR wind
  annotate(geom = "rect", ymin=1.3, ymax=6.0, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.05)+ #IQR nuclear
  geom_density_2d_filled(aes(alpha = after_stat(level), fill = after_stat(level)), contour_var = "ndensity")+
  scale_fill_manual("Density of IPCC pathways,\n scaled to a maximum of 1", values = c("#FFFFFF", "#EFEBF6", "#E6D6EE", "#E2C2E3","#E5B6D1", "#ECB1BA", "#F4B1A0", "#FCBB7F","#FED35C", "#F2F940"))+
  geom_point(aes(colour = temp, size = L), alpha = 0.7)+ 
  scale_color_manual("Temperature",labels = c("1.5°C", "2.0°C"), values = c("1.5°C" = "#a6cee3", "2.0°C" = "#7fc97f"), guide = "none")+ 
  scale_size_binned("Saturation level, Gt", breaks = c(10,20,30), range = c(0,1.5), guide = "none")+
  labs(y="Max growth rate as share of the market at TMax")+
  geom_hline(yintercept = 1.42, color = "black", linewidth = 0.3)+ #nuclear (gen) G for the global market
  geom_hline(yintercept = 0.5, color = "black", linewidth = 0.3)+
  geom_hline(yintercept = 0.7, color = "black", linewidth = 0.3)+
  scale_alpha_manual("Density of IPCC pathways,\n scaled to a maximum of 1", values = c(0,1,1,1,1,1,1,1,1,1))+ 
  scale_y_continuous(limits = c(0,7), breaks = seq(1,6,1), expand = c(0,0))+
  scale_x_continuous(limits = c(2025,2095), breaks = seq(2030,2090,10), expand = c(0,0))

tmax_box15 <- ggplot(subset(normalisation, temp == "1.5°C"), aes(TMax, temp))+
  scale_fill_manual("Temperature",labels = c("1.5°C"), values = c("1.5°C" = "#a6cee3"), guide = "none")+ 
  labs(x="TMax, year of maximum growth")+
  geom_boxplot(aes(fill = temp), show.legend=FALSE, size = 0.1, outlier.color = "#a6cee3", outlier.alpha = 0.7, outlier.size = 0.7)+
  scale_x_continuous(limits = c(2025,2095), breaks = seq(2030,2090,10), expand = c(0,0))

tmax_box20 <- ggplot(subset(normalisation, temp == "2.0°C"), aes(TMax, temp))+
  scale_fill_manual("Temperature",labels = c("2.0°C"), values = c("2.0°C" = "#7fc97f"), guide = "none")+ 
  labs(x="TMax, year of maximum growth")+
  geom_boxplot(aes(fill = temp), show.legend=FALSE, size = 0.1, outlier.color = "#7fc97f", outlier.alpha = 0.7, outlier.size = 0.7)+
  scale_x_continuous(limits = c(2025,2095), breaks = seq(2030,2090,10), expand = c(0,0))

colss <- c("1.5°C"="#a6cee3", "2.0°C"="#7fc97f")
g_box_tmax <- ggplot(data = subset(normalisation, temp != "2.5°C"), aes(temp, G_TMax))+
  annotate(geom = "rect", ymin=0.5, ymax=1.3, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.15)+ #IQR solar
  annotate(geom = "rect", ymin=0.6, ymax=1.7, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.1)+  #IQR wind
  annotate(geom = "rect", ymin=1.3, ymax=6.0, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.05)+ #IQR nuclear
  geom_hline(yintercept = 1.42, color = "black", linewidth = 0.3)+ #nuclear (gen) G for the global market
  geom_hline(yintercept = 0.5, color = "black", linewidth = 0.3)+
  geom_hline(yintercept = 0.7, color = "black", linewidth = 0.3)+
  geom_boxplot(aes(colour=factor(temp)), outlier.alpha = 0.7, size = 0.1, outlier.size = 0.7) +
  geom_boxplot(aes(fill=factor(temp)),  outlier.colour = NA, size = 0.1)+
  scale_fill_manual("Temperature",labels = c("1.5°C", "2.0°C"), values = colss, guide = "none")+ 
  scale_colour_manual("Temperature",labels = c("1.5°C", "2.0°C"), values = colss, guide = "none")+ 
  scale_y_continuous("Max growth rate as share of the market at TMax", limits = c(0,7), breaks = seq(1,6,1), expand = c(0,0))

g_box_t0 <- ggplot(data = subset(normalisation, temp != "2.5°C"), aes(temp, G_2022))+
  annotate(geom = "rect", ymin=0.5, ymax=1.3, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.15)+ #IQR solar
  annotate(geom = "rect", ymin=0.6, ymax=1.7, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.1)+  #IQR wind
  annotate(geom = "rect", ymin=1.3, ymax=6.0, xmin=-Inf, xmax=Inf, color="transparent", fill="black", alpha=0.05)+ #IQR nuclear
  geom_hline(yintercept = 1.42, color = "black", linewidth = 0.3)+ #nuclear (gen) G for the global market
  geom_hline(yintercept = 0.5, color = "black", linewidth = 0.3)+
  geom_hline(yintercept = 0.7, color = "black", linewidth = 0.3)+
  geom_boxplot(aes(colour=factor(temp)), outlier.alpha = 0.7, size = 0.1, outlier.size = 0.7) +
  geom_boxplot(aes(fill=factor(temp)),  outlier.colour = NA, size = 0.1)+
  scale_fill_manual("Temperature",labels = c("1.5°C", "2.0°C"), values = colss, guide = "none")+ 
  scale_colour_manual("Temperature",labels = c("1.5°C", "2.0°C"), values = colss, guide = "none")+ 
  scale_y_continuous("Maximum growth rate as share of the 2022 market", limits = c(0,7), breaks = seq(1,6,1), expand = c(0,0))

design <-
  "
ABCD           
GHEF           
"
figure4 <- (g_box_t0 & theme_bw(base_size = 7) & theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), panel.grid.minor.y = element_blank(), axis.text.y=element_blank(), 
                                              axis.ticks.y=element_blank(), axis.title.x = element_blank(),  legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()))+
  (g_box_tmax & theme_bw(base_size = 7) & theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), panel.grid.minor.y = element_blank(), axis.text.y=element_blank(), 
                                   axis.ticks.y=element_blank(), axis.title.x = element_blank(), legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()))+
  (dots15 & theme_bw(base_size = 7) & theme(axis.title.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(), legend.position = "none")) + 
  (dots20 & theme_bw(base_size = 7) & theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(), legend.position = "none")) + 
  (tmax_box15 & theme_bw(base_size = 7) & theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                   axis.title.y = element_blank(), legend.position = "none", panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank()))+
  (tmax_box20 & theme_bw(base_size = 7) & theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                                   axis.title.y = element_blank(), legend.position = "none", panel.grid.major.y = element_blank(), panel.grid.minor.x = element_blank()))+
  #guide_area()+
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect", design = design, widths=c(0.5,0.5,6,6), heights=c(6,0.25)) & theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal", panel.grid = element_blank())
ggsave("edf4.png", dpi = 500, width = 180, height = 120, unit = "mm", path = "./")

#saves a PNG file of Extended Data Figure 4 in the working directory (warnings regarding outliers are to be expected. change limits in 'scale_y_continuous' and 'scale_x_continuous' commands to expand the figure)
