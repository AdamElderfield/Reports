library(readabs)
library(FA)
library(midasr)

search_catalogues("monthly")
a <- show_available_files("monthly-employee-earnings-indicator")

MEI <- download_abs_data_cube("monthly-employee-earnings-indicator",cube = "MEEIDC02.xlsx")

MEI <-  
  readxl::read_xlsx(path = paste0(MEI),
    sheet = "Table_2.1",
    skip = 5
    
    
  )

dlf <- read_abs(cat_no = "6291.0.55.001")


nowcast_date <- "2025-06-01"

#-----------------------------------------------------
# PREPARE DATA
#-----------------------------------------------------

colnames(MEI[,-c(1:4)]) <- as.Date(as.numeric(colnames(MEI[,-c(1:4)])), origin = "1899-12-30")

AUS_ALL <- MEI %>% 
  filter(`State or Territory` == "0. Australia",
#         `Industry division` == "0. All industries",
         `Employment size` == "0. All sizes",
         Sector == "0. All sectors"
          
          
          ) %>% 
  select(-`State or Territory`,-`Employment size`,-Sector)

AUS_ALL[1:20,1] <- c("ALL","AG","MIN","MAN","EGW","CONS","WT","RT","AFS","TPW","IMT","FIS","RHR","PST","ASS","PAS","ET","HCS","ARS","OTS")


AUS_ALL <-AUS_ALL %>% 
  gather(date,Val,-`Industry division`) %>% 
  #slice(-c(1:4)) %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
  mutate(value = as.numeric(Val)) %>%
  select(-Val) %>% 
#  mutate(Var = "MEI") %>% 
  filter(!is.na(value)) %>% 
  spread(`Industry division`,value)


write.csv(AUS_ALL,".\\Monthly Earnings Indicator\\MEI.csv")




hours <- dlf %>% 
  filter(table_no =="6291011") %>%
  filter(grepl("Number of hours actually worked in all jobs ;", series )) %>% 
  filter(!grepl(">>", series )) %>% 
  filter(!grepl("Worked", series ))

hours <- hours %>% 
  group_by(series) %>% 
  mutate(value_o = value) %>% 
  mutate(value = predict(seasonal::seas(ts(value,start = c(1991,1), frequency = 4 ) ))) %>% 
  select(date,value,series)

hours <- hours %>% 
  mutate(series = if_else(series == "Agriculture, Forestry and Fishing ;  Number of hours actually worked in all jobs ;","AG",series),
         series = if_else(series == "Mining ;  Number of hours actually worked in all jobs ;","MIN",series) ,
         series = if_else(series == "Manufacturing ;  Number of hours actually worked in all jobs ;","MAN",series) ,
         series = if_else(series == "Electricity, Gas, Water and Waste Services ;  Number of hours actually worked in all jobs ;","EGW",series) ,
         series = if_else(series == "Construction ;  Number of hours actually worked in all jobs ;","CONS",series) ,
         series = if_else(series == "Wholesale Trade ;  Number of hours actually worked in all jobs ;","WT",series) ,
         series = if_else(series == "Retail Trade ;  Number of hours actually worked in all jobs ;","RT",series) ,
         series = if_else(series == "Accommodation and Food Services ;  Number of hours actually worked in all jobs ;","AFS",series) ,
         series = if_else(series == "Transport, Postal and Warehousing ;  Number of hours actually worked in all jobs ;","TPW",series) ,
         series = if_else(series == "Information Media and Telecommunications ;  Number of hours actually worked in all jobs ;","IMT",series) ,
         series = if_else(series == "Financial and Insurance Services ;  Number of hours actually worked in all jobs ;","FIS",series) ,
         series = if_else(series == "Rental, Hiring and Real Estate Services ;  Number of hours actually worked in all jobs ;","RHR",series) ,
         series = if_else(series == "Professional, Scientific and Technical Services ;  Number of hours actually worked in all jobs ;","PST",series) ,
         series = if_else(series == "Administrative and Support Services ;  Number of hours actually worked in all jobs ;","ASS",series) ,
         series = if_else(series == "Public Administration and Safety ;  Number of hours actually worked in all jobs ;","PAS",series) ,
         series = if_else(series == "Education and Training ;  Number of hours actually worked in all jobs ;","ET",series) ,
         series = if_else(series == "Health Care and Social Assistance ;  Number of hours actually worked in all jobs ;","HCS",series) ,
         series = if_else(series == "Arts and Recreation Services ;  Number of hours actually worked in all jobs ;","ARS",series) ,
         series = if_else(series == "Other Services ;  Number of hours actually worked in all jobs ;","OS",series) ,
         series = if_else(series == "Number of hours actually worked in all jobs ;","Total",series) ,
         )

hours <- hours %>% 
  mutate(series = paste0(series, "_HW")) %>% 
  spread(series,value) %>% 
  mutate(MKTS_HW = EGW_HW+WT_HW+RT_HW+AFS_HW+TPW_HW+IMT_HW+FIS_HW+RHR_HW+PST_HW+ASS_HW+ARS_HW+OS_HW,
         NMKTS_HW = PAS_HW+ET_HW+HCS_HW )

hours <- hours %>% 
  gather(series, value, -date)

hours %>% 
  spread(series,value) %>% 
  write.csv(".\\Monthly Earnings Indicator\\IND_HRS.csv")

# READ IN DATABASE
MD <- read_rds("C:\\Users\\aelde\\OneDrive\\Documents\\GitHub\\MACRO_DATA\\MACRO_DATA\\MACRO_DATA.rds")
  
HW_LFS <- MD$history_monthly %>% 
  filter(MNEMONIC == "HW_LFS") %>% 
  rename(Var = MNEMONIC) %>% 
  aggregate_m_to_q(agg = "SUM") %>% 
  spread(Var,value)

write.csv(HW_LFS, ".\\Monthly Earnings Indicator\\Hours_LFS.csv")

md <-   MD$history_quarterly %>% 
  rename(Var = MNEMONIC)

MD$history_quarterly %>% 
  filter(MNEMONIC == "COE") %>%
  rename(Var = MNEMONIC) %>% 
  bind_rows(
    AUS_ALL %>% aggregate_m_to_q(agg = "SUM")
  ) %>% spread(Var,value) %>%
  filter(!is.na(MEI)) %>%
  gather(Var,value, -date) %>%
  group_by(Var) %>% 
  mutate(value = FA::grth_rate(value,L=4)) %>% 
  line_plot()


#-----------------------------------------------------
# MIDAS  - RUN EVIEWS 
#-----------------------------------------------------

Now_Casts <- read_csv(".\\Monthly Earnings Indicator\\coe_nowcast.csv")


#-----------------------------------------------------
# Plots 
#-----------------------------------------------------

Now_Casts$`_date_` <- gsub("-07-","-09-",Now_Casts$`_date_`)
Now_Casts$`_date_` <- gsub("-01-","-03-",Now_Casts$`_date_`)
Now_Casts$`_date_` <- gsub("-10-","-12-",Now_Casts$`_date_`)
Now_Casts$`_date_` <- gsub("-04-","-06-",Now_Casts$`_date_`)

Now_Casts <- Now_Casts %>% 
  rename(date = `_date_`) %>% 
  gather(Var,value, -date) %>% 
  filter(!is.na(value)) %>% 
  mutate(Var = toupper(Var))

#### Total COE and Now Cast

COE_TOTAL_QOQ <- md %>% 
  filter(Var == "COE") %>% 
  group_by(Var) %>% 
  mutate(value = grth_rate(value,1))%>% 
  filter(date >= "2022-03-01") %>% 
  line_plot(axis_title_y = "%", title = "Compensation of employees growth expected marginally pick up",subtitle = "Monthly employee earnings indicator signals an uptick, likely due to public sector strength observed in WPI")


NOW_CAST <- Now_Casts %>% 
  filter(Var %in% c("COE","COE_NCST")) %>%
  group_by(Var) %>% 
  mutate(value = grth_rate(value,1)) %>% 
  mutate(value = if_else(date ==nowcast_date,value,NA)) %>% 
  filter(date >= nowcast_date) %>% 
  mutate(date = as.Date(date))

COE_TOTAL_QOQ <- COE_TOTAL_QOQ +
  geom_point(data = NOW_CAST, 
             aes(x = date, y = value),
             color = FA::FA_colours$main[2], 
             size = 2, 
             shape = 19)+
  annotate("text", 
           x = NOW_CAST$date, 
           y = NOW_CAST$value + 1, 
           label = paste0("Nowcast:\n", round(NOW_CAST$value,2), "%"),
           color = FA::FA_colours$main[2],
           size = 5,
           hjust = 0.5)


COE_TOTAL <- md %>% 
  filter(Var == "COE") %>% 
  group_by(Var) %>% 
  mutate(value = grth_rate(value,4))%>% 
  filter(date >= "2022-03-01") %>% 
  line_plot(axis_title_y = "%TTY", title = "Base effects are keeping annual estimates elevated",subtitle = "Expect this to linger over the quarter")


NOW_CAST <- Now_Casts %>% 
  filter(Var %in% c("COE","COE_NCST")) %>%
  group_by(Var) %>% 
  mutate(value = grth_rate(value,4)) %>% 
  mutate(value = if_else(date ==nowcast_date,value,NA)) %>% 
  filter(date >= nowcast_date) %>% 
  mutate(date = as.Date(date))

COE_TOTAL <- COE_TOTAL+
  geom_point(data = NOW_CAST, 
             aes(x = date, y = value),
             color = FA::FA_colours$main[2], 
             size = 2, 
             shape = 19)+
  annotate("text", 
           x = NOW_CAST$date, 
           y = NOW_CAST$value + 1, 
           label = paste0("Nowcast:\n", round(NOW_CAST$value,2), "%"),
           color = FA::FA_colours$main[2],
           size = 5,
           hjust = 0.5)
  

#### Labour component

md %>% 
  filter(Var %in% c("WAENA","COE")) %>% 
  spread(Var, value) %>% 
  mutate(`National Accounts Hours` = COE/WAENA)  

HW <- HW_LFS %>% 
  left_join(md %>% 
              filter(Var %in% c("WAENA","COE")) %>% 
              spread(Var, value) %>% 
              mutate(`National Accounts Hours` = COE/WAENA)  
  ) %>% 
  mutate(HRS_NA_G = grth_rate(`National Accounts Hours`),
         HRS_LFS_G = grth_rate(`HW_LFS`))

HW_reg <- HW %>% 
  filter(date<="2019-12-01")

LA_eq <- lm(HW_reg$HRS_NA_G~HW_reg$HRS_LFS_G, na.rm = T)

HW_NCST <- HW %>% 
  mutate(HRS_NA_G = if_else(date == nowcast_date, LA_eq$coefficients[1]+LA_eq$coefficients[2]*HRS_LFS_G, HRS_NA_G)) %>% 
  mutate(`National Accounts Hours` = if_else(date == nowcast_date, lag(`National Accounts Hours`)*(1+HRS_NA_G/100),`National Accounts Hours` )) %>% 
  select(date,`National Accounts Hours`) %>% 
  gather(Var,value,-date) 



NA_HRS_TOTAL_QOQ <- HW %>%
  select(date,`National Accounts Hours`) %>% 
  gather(Var,value,-date) %>%  
  mutate(value = grth_rate(value,1))%>% 
  filter(date >= "2022-03-01") %>% 
  filter(date <= nowcast_date) %>% 
  line_plot(axis_title_y = "%", title = "Hours worked in the national accounts expected to be stable", subtitle = "Labour force survey points to similar growth in hours as last quarter")


NOW_CAST <- HW_NCST %>%  
  mutate(value = grth_rate(value,1)) %>% 
  mutate(value = if_else(date ==nowcast_date,value,NA)) %>% 
  filter(date == nowcast_date) %>% 
  mutate(date = as.Date(date))

NA_HRS_TOTAL_QOQ <- NA_HRS_TOTAL_QOQ +
  geom_point(data = NOW_CAST, 
             aes(x = date, y = value),
             color = FA::FA_colours$main[2], 
             size = 2, 
             shape = 19)+
  annotate("text", 
           x = NOW_CAST$date, 
           y = NOW_CAST$value + 1, 
           label = paste0("Nowcast:\n", round(NOW_CAST$value,2), "%"),
           color = FA::FA_colours$main[2],
           size = 5,
           hjust = 0.5)


#### Wage component
WAGES_QOQ <- md %>% 
  filter(Var %in% c("WAENA")) %>% 
  mutate(value = grth_rate(value,1))%>% 
  filter(date >= "2022-03-01") %>% 
  filter(date <= nowcast_date) %>% 
  line_plot(axis_title_y = "%", title = "Wages are driving the increase in COE",subtitle = "But are not expected to rebound sharply as seen previously")


NOW_CAST_W <- HW_NCST %>%
  #select(date,`National Accounts Hours`) %>% 
  #gather(Var,value,-date) %>%  
  spread(Var, value) %>%
  left_join(
      Now_Casts %>% 
              filter(Var %in% c("COE_NCST")) %>% 
     spread(Var, value) ) %>% 
  mutate(Wages = COE_NCST/`National Accounts Hours`) %>% 
  select(date, Wages) %>%
  gather(Var, value, -date) %>% 
  mutate(value = grth_rate(value,1)) %>% 
  mutate(value = if_else(date ==nowcast_date,value,NA)) %>% 
  filter(date == nowcast_date) %>% 
  mutate(date = as.Date(date))


WAGES_QOQ <- WAGES_QOQ +
  geom_point(data = NOW_CAST_W, 
             aes(x = date, y = value),
             color = FA::FA_colours$main[2], 
             size = 2, 
             shape = 19)+
  annotate("text", 
           x = NOW_CAST_W$date, 
           y = NOW_CAST_W$value + 1, 
           label = paste0("Nowcast:\n", round(NOW_CAST_W$value,2), "%"),
           color = FA::FA_colours$main[2],
           size = 5,
           hjust = 0.5)


md <- md %>% 
  mutate(value = as.numeric(value)) %>% 

  spread(Var, value) %>% 
  mutate(COE_MKTS = toupper("coe_EGW+coe_WT+coe_RT+coe_AFS+coe_TPW+coe_IMT+coe_FIS+coe_RHR+coe_PST+coe_ASS+coe_ARS+coe_OTS"),
         COE_NMKTS =  toupper("coe_PAS+coe_ET+coe_HCS")) %>% 
  gather(Var,value,-date) %>% 
  mutate(value = as.numeric(value))


grth_rate1 <- function(x, L=1){
  #if(is.numeric(x)){stop("DATA MUST BE NUMERIC")}  Warning is breaking in shiny app - will look into
  
  # Find the first non-NA value
  first_non_na <- which(!is.na(x))[1]
  
  # If all values are NA, return vector of same length with all NAs
  if(is.na(first_non_na)) {
    return(rep(NA, length(x)))
  }
  
  # Create result vector initialized with NAs
  result <- rep(NA, length(x))
  
  # Only calculate growth rates from first_non_na + L onwards
  if(first_non_na + L <= length(x)) {
    # Get the subset starting from first non-NA
    x_subset <- x[first_non_na:length(x)]
    
    # Calculate growth rates for the subset
    growth_subset <- c(rep(NA, L), base::diff(x_subset, L)) / lag(x_subset, n = L) * 100
    
    # Place results in correct positions
    result[first_non_na:length(x)] <- growth_subset
  }
  
  return(result)
}



hours$date <- gsub("-08-","-09-",hours$date)
hours$date <- gsub("-02-","-03-",hours$date)
hours$date <- gsub("-11-","-12-",hours$date)
hours$date <- gsub("-05-","-06-",hours$date)

ind_p <- list()

for(i in c("AG","MIN","MAN","CONS","MKTS","NMKTS")){
  
  COE_TOTAL_QOQ <-  Now_Casts %>% 
    filter(Var == paste0("COE_",i)) %>% 
    group_by(Var) %>% 
    mutate(value = grth_rate(value,1))%>% 
    filter(date >= "2022-03-01") %>% 
    line_plot(axis_title_y = "%", title = "Compensation of employees",subtitle = i)
  
  
  NOW_CAST <- Now_Casts %>% 
    filter(Var %in% paste0("COE_",i,"_NCST")) %>%
    group_by(Var) %>% 
    mutate(value = grth_rate(value,1)) %>% 
    mutate(value = if_else(date ==nowcast_date,value,NA)) %>% 
    filter(date >= nowcast_date) %>% 
    mutate(date = as.Date(date))
  
  COE_TOTAL_QOQ <- COE_TOTAL_QOQ +
    geom_point(data = NOW_CAST, 
               aes(x = date, y = value),
               color = FA::FA_colours$main[2], 
               size = 2, 
               shape = 19)+
    annotate("text", 
             x = NOW_CAST$date, 
             y = NOW_CAST$value + 1, 
             label = paste0("Nowcast:\n", round(NOW_CAST$value,2), "%"),
             color = FA::FA_colours$main[2],
             size = 5,
             hjust = 0.5)
  
  
  #### Labour component
  

  HW <- hours %>%
    filter(series == paste0(i,"_HW")) %>% 
    mutate(series = "value_LFS") %>% 
    spread(series,value) %>% 
    mutate(date = as.character(date)) %>% 
    
    left_join(md %>% 
                filter(Var %in% paste0("HWLA_",i)) %>% 
                mutate(Var = "value_LA") %>% 
                spread(Var, value)) %>%
     
    mutate(value_LFS =  grth_rate(value_LFS),
           value_LA =  grth_rate(as.numeric(value_LA))) 
    
  HW_reg <- HW %>% 
    filter(date<="2019-12-01")
  
  LA_eq <- lm(HW_reg[[paste0("HWLA_",i)]]~HW_reg[[paste0(i,"_HW")]], na.rm = T)
  
  HW_NCST <- HW %>% 
    mutate(HRS_NA_G = if_else(date == nowcast_date, LA_eq$coefficients[1]+LA_eq$coefficients[2]*HRS_LFS_G, HRS_NA_G)) %>% 
    mutate(`National Accounts Hours` = if_else(date == nowcast_date, lag(`National Accounts Hours`)*(1+HRS_NA_G/100),`National Accounts Hours` )) %>% 
    select(date,`National Accounts Hours`) %>% 
    gather(Var,value,-date) 
  
  
  
  NA_HRS_TOTAL_QOQ <- HW %>%
    select(date,`National Accounts Hours`) %>% 
    gather(Var,value,-date) %>%  
    mutate(value = grth_rate(value,1))%>% 
    filter(date >= "2022-03-01") %>% 
    filter(date <= nowcast_date) %>% 
    line_plot(axis_title_y = "%", title = "Hours worked in the national accounts expected to be stable",subtitle = "Labour force survey points to similar growth in hours as last quarter")
  
  
  NOW_CAST <- HW_NCST %>%  
    mutate(value = grth_rate(value,1)) %>% 
    mutate(value = if_else(date ==nowcast_date,value,NA)) %>% 
    filter(date == nowcast_date) %>% 
    mutate(date = as.Date(date))
  
  NA_HRS_TOTAL_QOQ <- NA_HRS_TOTAL_QOQ +
    geom_point(data = NOW_CAST, 
               aes(x = date, y = value),
               color = FA::FA_colours$main[2], 
               size = 2, 
               shape = 19)+
    annotate("text", 
             x = NOW_CAST$date, 
             y = NOW_CAST$value + 1, 
             label = paste0("Nowcast:\n", round(NOW_CAST$value,2), "%"),
             color = FA::FA_colours$main[2],
             size = 5,
             hjust = 0.5)
  
  
  #### Wage component
  WAGES_QOQ <- md %>% 
    filter(Var %in% c("WAENA")) %>% 
    mutate(value = grth_rate(value,1))%>% 
    filter(date >= "2022-03-01") %>% 
    filter(date <= nowcast_date) %>% 
    line_plot(axis_title_y = "%", title = "Wages are driving the increase in COE",subtitle = "But are expected to slow in the quarter, in line with other indicators")
  
  
  NOW_CAST_W <- HW_NCST %>%
    #select(date,`National Accounts Hours`) %>% 
    #gather(Var,value,-date) %>%  
    spread(Var, value) %>%
    left_join(
      Now_Casts %>% 
        filter(Var %in% c("COE_NCST")) %>% 
        spread(Var, value) ) %>% 
    mutate(Wages = COE_NCST/`National Accounts Hours`) %>% 
    select(date, Wages) %>%
    gather(Var, value, -date) %>% 
    mutate(value = grth_rate(value,1)) %>% 
    mutate(value = if_else(date ==nowcast_date,value,NA)) %>% 
    filter(date == nowcast_date) %>% 
    mutate(date = as.Date(date))
  
  
  WAGES_QOQ <- WAGES_QOQ +
    geom_point(data = NOW_CAST_W, 
               aes(x = date, y = value),
               color = FA::FA_colours$main[2], 
               size = 2, 
               shape = 19)+
    annotate("text", 
             x = NOW_CAST_W$date, 
             y = NOW_CAST_W$value + 1, 
             label = paste0("Nowcast:\n", round(NOW_CAST_W$value,2), "%"),
             color = FA::FA_colours$main[2],
             size = 5,
             hjust = 0.5)
  
  
  
  
  ind_p[[paste0("COE_",i)]] <- COE_TOTAL_QOQ
  
}

plots_2_save <- list(COE_TOTAL_QOQ,COE_TOTAL,NA_HRS_TOTAL_QOQ,WAGES_QOQ)

saveRDS(plots_2_save,".\\Monthly Earnings Indicator\\PLOTS.rds")



