#Code for the article Kazlou T., Cherp A., Jewell J.
#Feasible deployment of carbon capture and storage and the requirements of climate targets


#Fitting Gompertz and Logistic growth curves to CCS deployment projections in the IPCC AR6 scenarios
#Working directory should be set to the directory containing input files
#Necessary packages (should be installed)

library(tidyverse)
library(minpack.lm)
library(patchwork)
source("functions.R")     #use the path to "functions.R" file
filename <- "formatted_scenario_data.csv" #use the path to "formatted_scenario_data.csv"
`%nin%` = Negate(`%in%`)
mods <- c("S","G","G_new")
base_exp <- 2000
total <- F

###End of parameters
fout <- str_c(tools::file_path_sans_ext(filename), "_fit.csv")
df <- read.csv(str_c(filename), stringsAsFactors  = F)


result0 <- data.frame()

for (cn in unique(df$Country)) {
  print(cn)
  dt <- df %>% filter(Country == cn)
  res <- fit_curve(dt, fit = mods, t_exp = base_exp)
  res1 <- res %>% mutate(Country = cn)
  result0 <- res1 %>% rbind(result0)  
}

min.rss <- result0 %>%
  filter(Good == 1) %>%
  group_by(Country) %>%
  summarize(Min.RSS = min(RSS)) %>%
  ungroup
result1 <- result0 %>% 
  merge(min.rss) %>%
  mutate(RSS.Rel = RSS/Min.RSS) %>%
  arrange(Country, Fit)
rownames(result1) <- 1:nrow(result1)

if (!("Total" %in% colnames(df))) {
  total <- F  
}

df.max <- df %>% 
  group_by(Country) %>%
  summarize(Max.Year = max(Year)) %>%
  ungroup
result2 <- result1 %>% mutate(Year = round(TMax)) %>%
  merge(df.max) %>%
  mutate(Future = ifelse(Year > Max.Year & Good == 1 & Fit != "E", 1, 0),
         dT = ifelse(Fit == "S" & Good ==1, log(81)/K, 0),
         dT = ifelse(Fit %in% c("G", "G_new") & Good ==1, log(log(0.1)/log(0.9))/K, dT))

if (total) {
  result3 <- result2 %>% 
    mutate(Year = ifelse(Year > Max.Year | Good != 1 | Fit == "E", Max.Year, Year)) %>%
    merge(df) %>%
    mutate(G.Size = ifelse(Fit != "E", G/Total, 0),
           L.Size = ifelse(Fit != "E", L/Total, dT))
  result4 <- result3 %>% select(Country, Fit, L, L.Size, TMax, K,  dT, G, G.Size, Maturity, RSS.Rel, Good, Future)
} else {
  
  result4 <- result2 %>% select(Country, Fit, L, TMax, K, dT, G, Maturity, RSS.Rel, Good, Future)
}
failed <- result4 %>% filter(Fit == "G" & Good == 0)
corrected <- result4 %>% filter(Country %in% failed$Country) %>%
  filter(Fit %in% c("G_new", "S"))
result5 <- result4  %>%
  filter(Country %nin% failed$Country & Fit != "G_new") %>% rbind(.,corrected) %>% unique() %>%
  mutate(Fit = ifelse(Fit == "G_new", "G", Fit)) %>% 
  mutate(TMax_top = plyr::round_any(TMax, 10, ceiling), TMax_bot = plyr::round_any(TMax, 10, floor)) %>% filter(Future == 0)

write.csv(result5, fout, row.names = F)
#creates an output CSV file in the working directory