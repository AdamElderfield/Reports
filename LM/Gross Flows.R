

library(FA)
library(readabs)
library(patchwork)
library(gt)


LFS_M <- readabs::read_abs(cat_no = "6202.0", check_local = F)

gf <- read_lfs_grossflows(path = "C:\\Users\\aelde\\OneDrive\\Documents\\GitHub\\ABS")

gf <- readxl::read_xlsx(path = "C:\\Users\\aelde\\OneDrive\\Documents\\GitHub\\ABS\\GM1.xlsx", sheet= "Data 1", 
                        skip = 4, col_names = c("date", "sex", "age", "state", 
                                                "lfs_current", "lfs_previous", "persons"), col_types = c("date", 
                                                                                                         "text", "text", "text", "text", "text", "numeric"))

gf <- gf %>% mutate(date = as.Date(date, format = "%Y-%m-%d %H-%M-%S"), 
                    unit = "000s", weights = "current month")

gf_hist <- readxl::read_xlsx(path = "C:\\Users\\aelde\\OneDrive\\Documents\\GitHub\\ABS\\GM1_Historical.xlsx", sheet = "Data 1")


gf_hist <- gf_hist[-c(1:3),1:7]
colnames(gf_hist) <- colnames(gf)[1:7]
gf_hist$unit <- gf$unit[1]
gf_hist$weights <- gf$weights[1]
gf_hist$date <- as.Date(as.numeric(gf_hist$date), origin = "1899-12-30")
gf_hist$persons <- as.numeric(gf_hist$persons)

gf_total <- bind_rows(gf_hist %>% 
                        filter(date <= "2006-03-01"),gf)


FL_M <- LFS_M #readabs::read_abs_local(path = "C:\\Users\\aelde\\OneDrive\\Documents\\GitHub\\ABS",
#                        cat_no = "6202.0")


LM <- LFS_M %>% 
  filter(table_no %in% c("6202001","6202019")) %>% 
  filter(series_id %in% c("A84423043C","A84423047L","A84426277X","A84423050A", "A84423046K","A84423051C")) %>% # Employment heads, Part, Hours
  select(date, value, series) %>% 
  mutate(series = case_when(
    series == "Employed total ;  Persons ;" ~ "Employment",
    series == "Labour force total ;  Persons ;"  ~ "Labour Supply",
    series == "Monthly hours worked in all jobs ;  Persons ;" ~ "Total Hours (LFS)" ,
    series == "Unemployment rate ;  Persons ;" ~ "Unemployment rate",
    series == "Unemployed total ;  Persons ;" ~ "Unemployed",
    series == "Participation rate ;  Persons ;"~"Particpation rate")) %>% 
  rename(Var = series)

last_date <- last(LM$date)

latest_change <- LM %>% 
  group_by(Var) %>%
  mutate(value = difference(value)) %>%
  filter(date == last_date) %>%
  ungroup() %>% mutate(value = value*1000)


latest_change_q <- LM %>%
  group_by(Var) %>%
  mutate(value = difference(value)) %>% 
  filter(date == last_date) %>%
  ungroup() %>% mutate(value = value*1000)


latest_change_q <- LM %>%
  aggregate_m_to_q(agg = "SUM") %>%
  group_by(Var) %>%
  mutate(value = difference(value)) %>% 
  ungroup() %>% mutate(value = value*1000)

#--------------------------------------------------------------------
# Standard labour market charts
#--------------------------------------------------------------------

# Gross flows data and plots

Probs <- gf_total %>% 
  group_by(date, lfs_current, lfs_previous) %>% 
  summarise(value = sum(persons)) %>% 
  filter(!lfs_current %in% c("Incoming rotation group","Outgoing rotation group","Unmatched in common sample (responded in current month but not in previous)","Unmatched in common sample (responded in previous month but not in current)")) %>% 
  filter(!lfs_previous %in% c("Incoming rotation group","Outgoing rotation group","Unmatched in common sample (responded in current month but not in previous)","Unmatched in common sample (responded in previous month but not in current)")) %>%
  group_by(date, lfs_previous) %>%
  mutate(lfs_current = if_else(lfs_current == "Employed full-time"|lfs_current == "Employed part-time", "Employed",lfs_current)) %>% 
  mutate(lfs_previous = if_else(lfs_previous == "Employed full-time"|lfs_previous == "Employed part-time", "Employed",lfs_previous)) %>% 
  group_by(lfs_previous, date, lfs_current) %>% 
  summarise(value = sum(value)) %>% 
  
  mutate(Prob = value/sum(value)) 



U_2_E_prob <- Probs %>% 
  filter(lfs_current %in% c("Employed")) %>% 
  filter(lfs_previous %in% c("Unemployed")) %>%
  select(-value) %>% 
  group_by(date) %>% 
  mutate(value = (Prob)*100) %>% 
  select(-Prob) %>% 
  ungroup() %>% 
  mutate(value = predict(seasonal::seas((ts(value,start = c(1992,2),frequency = 12))))) %>% 
  mutate(Var = "Unemployment to employment") %>% 
   aggregate_m_to_q(agg = "AVE") 


P101 <- U_2_E_prob %>% 
  select(date, value, Var) %>% 
  spread(Var, value) %>% 
  gather(Var, value, -date) %>% 
  line_plot(axis_title_y = "% of Unemployed in previous period",title = "Job finding rate")+
  annotate("rect",xmin=as.Date("1991-03-01"), xmax=as.Date("1992-12-01"), ymin=15, ymax=Inf, alpha = 0.5)+
  annotate("rect",xmin=as.Date("2008-03-01"), xmax=as.Date("2009-12-01"), ymin=15, ymax=Inf, alpha = 0.5)+
  annotate("rect",xmin=as.Date("2020-03-01"), xmax=as.Date("2021-06-01"), ymin=15, ymax=Inf, alpha = 0.5)+
  theme(legend.position = "bottom")


E_2_U_prob <- Probs %>% 
  filter(lfs_previous %in% c("Employed")) %>% 
  filter(lfs_current %in% c("Unemployed")) %>%
  select(-value) %>% 
  group_by(date) %>% 
  mutate(value = (Prob)*100) %>% 
  select(-Prob) %>% 
  mutate(Var = "Employment to Unemployment") %>% 
  ungroup() %>% 
 aggregate_m_to_q(agg = "AVE") %>% 
  
  mutate(value = predict(seasonal::seas((ts(value,start = c(1992,2),frequency = 12)))))  


P100 <- E_2_U_prob %>%
  select(date, value, Var) %>% 
  spread(Var, value) %>% 
  gather(Var, value, -date) %>% 
  line_plot(axis_title_y = "% of Employed in previous period",title = "Job seperation rate")+
  annotate("rect",xmin=as.Date("1991-03-01"), xmax=as.Date("1992-12-01"), ymin=0.5, ymax=Inf, alpha = 0.5)+
  annotate("rect",xmin=as.Date("2008-03-01"), xmax=as.Date("2009-12-01"), ymin=0.5, ymax=Inf, alpha = 0.5)+
  annotate("rect",xmin=as.Date("2020-03-01"), xmax=as.Date("2021-06-01"), ymin=0.5, ymax=Inf, alpha = 0.5)+
  theme(legend.position = "bottom")




N_2_U_prob <- Probs %>% 
  filter(lfs_previous %in% c("Not in the labour force (NILF)")) %>% 
  filter(lfs_current %in% c("Unemployed")) %>%
  select(-value) %>% 
  group_by(date) %>% 
  summarise(value = sum(Prob)*100) %>%
  mutate(value =predict(seasonal::seas((ts(value,start = c(1992,2),frequency = 12))))) %>% 
  mutate(Var = "NILF to unemployement")  %>% 
   aggregate_m_to_q(agg = "AVE") 

P102 <- N_2_U_prob %>% 
  spread(Var, value) %>% 
  gather(Var, value, -date) %>% 
  line_plot(axis_title_y = "% of NILF in previous period",title = "Entry rate to unemployment")+
  annotate("rect",xmin=as.Date("1991-03-01"), xmax=as.Date("1992-12-01"), ymin=1.5, ymax=Inf, alpha = 0.5)+
  annotate("rect",xmin=as.Date("2008-03-01"), xmax=as.Date("2009-12-01"), ymin=1.5, ymax=Inf, alpha = 0.5)+
  annotate("rect",xmin=as.Date("2020-03-01"), xmax=as.Date("2021-06-01"), ymin=1.5, ymax=Inf, alpha = 0.5)+
  theme(legend.position = "bottom")


N_2_E_prob <- Probs %>% 
  filter(lfs_current %in% c("Not in the labour force (NILF)")) %>% 
  filter(lfs_previous %in% c("Employed")) %>% 
  select(-value) %>% 
  group_by(date) %>% 
  summarise(value = sum(Prob)*100) %>%
  mutate(value = predict(seasonal::seas((ts(value,start = c(1992,2),frequency = 12))))) %>% 
  mutate(Var = "NILF to employment")  %>% 
   aggregate_m_to_q(agg = "AVE") 

P103 <- N_2_E_prob %>% 
  spread(Var, value) %>% 
  gather(Var, value, -date) %>% 
  line_plot(axis_title_y = "% of NILF in previous period",title = "Entry rate to employment")+
  annotate("rect",xmin=as.Date("1991-03-01"), xmax=as.Date("1992-12-01"), ymin=1.5, ymax=Inf, alpha = 0.5)+
  annotate("rect",xmin=as.Date("2008-03-01"), xmax=as.Date("2009-12-01"), ymin=1.5, ymax=Inf, alpha = 0.5)+
  annotate("rect",xmin=as.Date("2020-03-01"), xmax=as.Date("2021-06-01"), ymin=1.5, ymax=Inf, alpha = 0.5)+
  theme(legend.position = "bottom")



P100+P101+P102+P103

