
library(FA)
library(readabs)
library(patchwork)
library(gt)


CPIQ_data <- read_abs("6401.0",check_local = F)

CPIM_data <- read_abs("6484.0",check_local = F)


calc_xlim <- function(df, x_col, has_callouts = FALSE, callout_extension = 2) {
  min_x <- min(df[[x_col]], na.rm = TRUE)
  max_x <- max(df[[x_col]], na.rm = TRUE)
  
  if (has_callouts) {
    return(c(min_x, max_x + callout_extension))
  } else {
    return(c(min_x, max_x))
  }
}

# Function to add callout annotations for long-format data
add_callouts <- function(df, x_col, var_col, value_col, colors, x_offset = 0.5) {
  max_x <- max(df[[x_col]], na.rm = TRUE)
  vars <- unique(df[[var_col]])
  annotations <- list()
  
  for (i in seq_along(vars)) {
    var_name <- vars[i]
    color <- colors[i]
    
    last_value <- df %>%
      filter(.data[[var_col]] == var_name) %>%
      filter(.data[[x_col]] == max(.data[[x_col]])) %>%
      pull(.data[[value_col]])
    
    annotations[[length(annotations) + 1]] <- 
      annotate("segment", 
               x = max_x, xend = max_x + x_offset * 0.3, 
               y = last_value, yend = last_value,
               color = color, linewidth = 0.4)
    
    annotations[[length(annotations) + 1]] <- 
      annotate("label", 
               x = max_x + x_offset * 0.5, y = last_value, 
               label = paste0(round(last_value, 2), "%"), 
               hjust = 0, size = 2.5, fill = "white", 
               color = ifelse(i == 1, "black", color),
               label.padding = unit(0.1, "lines"))
  }
  
  return(annotations)
}




#-------------------------- Weights data for charts 

Weights <- readxl::read_xlsx("C:\\Users\\aelde\\OneDrive\\Documents\\GitHub\\MACRO_DATA\\MACRO_DATA\\Manual Download\\Consumer Price Index - 2025 Weighting Pattern.xlsx", sheet = "Table 1")

Weights_l <-  sapply(Weights,function(x){zoo::na.locf(x)})

Weights_Ag <- data.frame(Var = Weights_l$`Consumer Price Index, Weighting Pattern, 2025`[-c(1:5,138:140)],
                         Weight = as.numeric(Weights_l[["...4"]][-c(1:2,135:137)]))

Weights_2 <- data.frame(Var = Weights_l$...2,
                        Weight = as.numeric(Weights_l[["...5"]])) %>% 
  distinct()


Weights_3 <- data.frame(Var = Weights_l$...3,
                        Weight = as.numeric(Weights_l[["...6"]])) %>% 
  distinct()


Weights_Ag <- Weights_Ag %>% 
  distinct()


#--------------------------Contributions to growth 


CPI_Tb1_Aus <- CPIQ_data %>%
  filter(table_no == "640102") %>%
  separate_series() %>% 
  filter(series_3 == "Australia") %>% 
  filter(series_1 == "Index Numbers")


CPI <- CPI_Tb1_Aus %>%
  select(series_2,date,value) %>%
  rename(Var = series_2) %>% 
  mutate(Var = toupper(Var)) %>% 
  left_join(Weights_Ag) %>% 
  filter(!is.na(Weight)) %>% 
  mutate(Contribution = value^(Weight/100))

CPI_Diff_q <- CPI %>% 
  select(Var, date, Contribution) %>% 
  group_by(Var) %>% 
  mutate(value = log(Contribution/lag(Contribution,1))*100) %>% 
  select(Var, value, date)

CPI_Diff_a <- CPI %>% 
  select(Var, date, Contribution) %>% 
  group_by(Var) %>% 
  mutate(value = log(Contribution/lag(Contribution,4))*100) %>% 
  select(Var, value, date)


cont_plot_q <- CPI_Diff_q %>% 
  filter(date >="2023-06-01") %>% 
  stacked_plot(total_line = "ALL GROUPS CPI",title = "Quarterly contributions to CPI inflation", axis_title_y = "%")+
  geom_hline(yintercept = 0)

cont_plot_a <- CPI_Diff_a %>% 
  filter(date >="2016-06-01") %>% 
  stacked_plot(total_line = "ALL GROUPS CPI",title = "Annual contributions to CPI inflation", axis_title_y = "%TTY")+
  geom_hline(yintercept = 0)

combinded_cont_plot <- cont_plot_a+cont_plot_q+ plot_layout(guides = "collect")& theme(legend.position = 'bottom')&
  guides(fill=guide_legend(nrow=4))




#----------------------------------
#  Exluding Food and Energy / Food and Energy


CPI_Tb7_Aus <- CPIQ_data %>%
  filter(table_no == "640107") %>%
  separate_series() %>% 
  filter(series_3 == "Australia") %>% 
  filter(series_1 == "Index Numbers")


CPI_SUB <- CPI_Tb7_Aus %>%
  select(series_2,date,value) %>%
  rename(Var = series_2) %>% 
#  mutate(Var = toupper(Var)) %>% 
  left_join(Weights_3) %>% 
  filter(!is.na(Weight)) %>% 
  filter(duplicated(Var))

CPI_SUB_DAT <- CPI_SUB %>% 
  filter(Var %in% Weights_3[c(1:25,43,44,64),"Var"]) %>%
  group_by(date) %>% 
  mutate(value = value*(Weight/100)/sum(Weight/100)) %>% 
  select(-Weight) %>% 
  group_by(date) %>% 
  summarise(`Food and Energy` = sum(value)) %>% 
  gather(Var, value, -date) %>% 
  bind_rows(CPIQ_data %>% 
              filter(table_no =="640106") %>% 
              separate_series() %>% 
              filter(series_2 %in% c("All groups CPI excluding food and energy")) %>%
              filter(series_1 == "Index Numbers") %>% 
              filter(data_type == "INDEX") %>% 
              select(date, value, series_2) %>% 
              rename(Var = series_2)) %>% 
  group_by(Var) %>% 
  mutate(value = grth_rate(value,L=4)) %>% 
  filter(!is.na(value))


P3 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "%TTY",title = "All Groups excluding Food & Energy")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")




#----------------------------------
#  Administered prices



CPI_SUB_DAT <- CPI_SUB %>% 
  filter(Var %in% Weights_3[c(41:44,55,82,83,84,58:61,67,68,66),"Var"])  %>%
  group_by(date) %>% 
  mutate(value = value*(Weight/100)/sum(Weight/100)) %>% 
  select(-Weight) %>% 
  group_by(date) %>% 
  summarise(`Administered prices` = sum(value)) %>% 
  gather(Var, value, -date) %>% 
  bind_rows(CPIQ_data %>% 
              filter(table_no =="640106") %>% 
              separate_series() %>% 
              filter(series_2 %in% c("Market goods and services excluding 'volatile items' - Total")) %>%
              filter(series_1 == "Index Numbers") %>% 
              filter(data_type == "INDEX") %>% 
              select(date, value, series_2) %>% 
              rename(Var = series_2)) %>% 
  group_by(Var) %>% 
  mutate(value = grth_rate(value,L=4)) %>% 
  filter(!is.na(value))%>%
    filter(date >= "2010-06-01") 


P4 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "%TTY",title = "Market and Administered inflation")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")



#----------------------------------
#  Market G and S



CPI_SUB_DAT <- CPIQ_data %>% 
              filter(table_no =="640106") %>% 
              separate_series() %>% 
              filter(series_2 %in% c("Market goods and services excluding 'volatile items' - Goods",
                                     "Market goods and services excluding 'volatile items' - Services")) %>%
              filter(series_1 == "Index Numbers") %>% 
              filter(data_type == "INDEX") %>% 
              select(date, value, series_2) %>% 
              rename(Var = series_2) %>% 
  group_by(Var) %>% 
  mutate(value = grth_rate(value,L=4)) %>% 
  filter(!is.na(value))

P5 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "%TTY",title = "Market goods & services")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")




#----------------------------------
#  Diffusion index



CPI_SUB_DAT <- CPI_SUB %>% 
  distinct(Var,date, value) %>% 
  group_by(Var) %>% 
  mutate(value= grth_rate(value,L=4)) %>%
  group_by(date) %>% 
  mutate(`Diffusion Index` = if_else(value>2.5,1,0)) %>% 
  mutate(`Diffusion Index` = (sum(`Diffusion Index`, na.rm = T)/87*100 )) %>% 
  select(date, `Diffusion Index`) %>% 
  group_by(date) %>% 
  summarise(`Diffusion Index` = mean(`Diffusion Index`)) %>% 
  filter(date >= "1990-03-01") %>% 
  gather(Var, value,-date)


P6 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "% of categories",title = "Diffusion Index")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")



#

#-------------------------- Key points chart
LR_INF <- CPIQ_data %>% 
  filter(table_no =="640106") %>% 
  separate_series() %>% 
  filter(series_2 %in% c("All groups CPI")) %>%
  filter(series_1 == "Index Numbers") %>% 
  filter(data_type == "INDEX") %>% 
  select(date, value, series_2) %>% 
  rename(Var = series_2) %>%
  group_by(Var) %>% 
  mutate(value = grth_rate(value, L =4)) %>% 
  filter(!is.na(value))


  P1 <-line_plot(LR_INF, axis_title_y = "%TTY", title = "Long term headline inflation") +
  
  # Korean War boom
  annotate("text", x = as.Date("1955-06-01"), y = 24, label = "Korean War boom",
           size = 3.2, color = "black", hjust = 0) +
  annotate("segment", x = as.Date("1953-06-01"), xend = as.Date("1954-06-01"), 
           y = 22, yend = 24, colour = "black", size = 0.6, alpha = 0.7) +
  # 1st Oil price shock
  annotate("text", x = as.Date("1972-12-01"), y = 20, label = "1st Oil\nprice\nshock",
           size = 3.2, color = "black", hjust = 0.5, lineheight = 0.9) +
  annotate("segment", x = as.Date("1974-09-01"), xend = as.Date("1974-03-01"), 
           y = 17, yend = 19, colour = "black", size = 0.6, alpha = 0.7) +
  
  # 2nd Oil price shock
  annotate("text", x = as.Date("1979-06-01"), y = 15.5, label = "2nd Oil\nprice\nshock",
           size = 3.2, color = "black", hjust = 0.5, lineheight = 0.9) +
  annotate("segment", x = as.Date("1980-06-01"), xend = as.Date("1980-06-01"), 
           y = 13.5, yend = 12, colour = "black", size = 0.6, alpha = 0.7) +
  
  # Real wage reforms
  annotate("text", x = as.Date("1964-06-01"), y = 15, label = "Real wage reforms",
           size = 3.2, color = "black", hjust = 0) +
  annotate("segment", x = as.Date("1965-12-01"), xend = as.Date("1973-06-01"), 
           y = 14.5, yend = 11, colour = "black", size = 0.6, alpha = 0.7) +
  
  # GST introduced
  annotate("text", x = as.Date("1998-06-01"), y = 8, label = "GST introduced",
           size = 3.2, color = "black", hjust = 0) +
  annotate("segment", x = as.Date("2000-06-01"), xend = as.Date("2000-06-01"), 
           y = 7.5, yend = 6, colour = "black", size = 0.6, alpha = 0.7) +
  
  # AUD depreciation, commodity boom
  annotate("text", x = as.Date("2003-06-01"), y = 7, label = "AUD\ndepreciation,\ncommodity\nboom",
           size = 3.2, color = "black", hjust = 0, lineheight = 0.9) +
  annotate("segment", x = as.Date("2007-06-01"), xend = as.Date("2007-06-01"), 
           y = 6, yend = 4.5, colour = "black", size = 0.6, alpha = 0.7) +
  
  # First mining boom
  annotate("text", x = as.Date("2009-06-01"), y = 6, label = "First mining\nboom",
           size = 3.2, color = "black", hjust = 0, lineheight = 0.9) +
  annotate("segment", x = as.Date("2011-06-01"), xend = as.Date("2011-06-01"), 
           y = 5.5, yend = 4, colour = "black", size = 0.6, alpha = 0.7) +
  
  # Pandemic shock
  annotate("text", x = as.Date("2021-12-01"), y = 16, label = "Pandemic\nshock",
           size = 3.2, color = "black", hjust = 0, lineheight = 0.9) +
  annotate("segment", x = as.Date("2022-06-01"), xend = as.Date("2022-06-01"), 
           y = 15, yend = 8, colour = "black", size = 0.6, alpha = 0.7) +
  
  # Add horizontal dashed line at y = 2 (inflation target)
  geom_hline(yintercept = 2, linetype = "dashed", color = "grey50", size = 0.8) +
  
  # Adjust theme for better readability
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  
  # Set axis limits to match the original
  scale_x_date(date_breaks = "5 years", date_labels = "%y") +
  scale_y_continuous(breaks = seq(-5, 25, 5), limits = c(-5, 25))+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim = calc_xlim(LR_INF, "date", has_callouts = FALSE, callout_extension = 2), clip = "off")

# TM WM 

TM_WM <- CPIQ_data %>% 
  filter(table_no =="640106") %>% 
  separate_series() %>% 
  filter(series_2 %in% c("Weighted Median","Trimmed Mean")) %>%
  filter(series_1 == "Index Numbers") %>% 
  filter(data_type == "INDEX") %>% 
  select(date, value, series_2) %>% 
  rename(Var = series_2) %>%
  group_by(Var) %>% 
  mutate(value = grth_rate(value, L =4)) %>% 
  filter(date >= '2000-06-01')


TM_HLplot <- line_plot(TM_WM, axis_title_y = "%TTY",title = "Underlying inflation")

P2 <- TM_HLplot+annotate("rect",ymin=2, ymax=3, xmin=as.Date("2000-06-01"), xmax=as.Date(last(TM_WM$date)), alpha = 0.5)+
  theme(legend.position = "bottom")+
add_callouts(TM_WM,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(TM_WM, "date", has_callouts = TRUE, callout_extension = 2) 
                  , clip = "off")





#-------------------------- Inflation by spedning type

# Discretionary / non-discretionary
CPI_SUB_DAT <- CPIQ_data %>% 
  filter(table_no =="640106") %>% 
  separate_series() %>% 
  filter(series_2 %in% c("Discretionary excluding tobacco",
                         "Non-Discretionary")) %>%
  filter(series_1 == "Index Numbers") %>% 
  filter(data_type == "INDEX") %>% 
  select(date, value, series_2) %>% 
  rename(Var = series_2) %>% 
  group_by(Var) %>% 
  mutate(value = grth_rate(value,L=4)) %>% 
  filter(!is.na(value))


P2.1 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "%TTY",title = "Discretionary & non-disc. inflation")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")


# Durable / non-durable  (doesn't look correct) 

CPI_SUB_DAT <- CPI_SUB %>% 
  filter(Var %in% Weights_3[c(1:25,26,27,28),"Var"])  %>%
  group_by(date) %>% 
  mutate(value = value*(Weight/100)/sum(Weight/100)) %>% 
  select(-Weight) %>% 
  group_by(date) %>%  
  summarise(`Non-durable goods` = sum(value)) %>% 
  gather(Var, value, -date) %>%
  bind_rows(
    CPI_SUB %>% 
      filter(Var %in% Weights_3[c(30:37,45:54,62:66),"Var"])  %>%
      group_by(date) %>% 
      mutate(value = value*(Weight/100)/sum(Weight/100)) %>% 
      select(-Weight) %>% 
      group_by(date) %>%  
      summarise(`Durable goods` = sum(value)) %>% 
      gather(Var, value, -date)
  )%>% 
  mutate(value = grth_rate(value,L=4)) %>% 
  filter(!is.na(value))

P2.2 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "%TTY",title = "Durable and non-durable goods")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")




# Tradebable / non-tradeable
CPI_SUB_DAT <- CPIQ_data %>% 
  filter(table_no =="640106") %>% 
  separate_series() %>% 
  filter(series_2 %in% c("Tradables",
                         "Non-tradables")) %>%
  filter(series_1 == "Index Numbers") %>% 
  filter(data_type == "INDEX") %>% 
  select(date, value, series_2) %>% 
  rename(Var = series_2) %>% 
  group_by(Var) %>% 
  mutate(value = grth_rate(value,L=4)) %>% 
  filter(!is.na(value))


P2.3 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "%TTY",title = "Tradable & non-tradeable inflation")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")


# Sticky / non-sticky  (not correct)
flexible <- c(64,66,1:22,44,62,31,32,33,34,35,36,63,25)
sticky <- -c(flexible,22,86,87)

CPI_SUB_DAT <- CPI_SUB %>% 
  filter(Var %in% Weights_3[flexible,"Var"])  %>%
  group_by(date) %>% 
  mutate(value = value*(Weight/100)/sum(Weight/100)) %>% 
  select(-Weight) %>% 
  group_by(date) %>%  
  summarise(`Flexible` = sum(value)) %>% 
  gather(Var, value, -date) %>%
  bind_rows(
    CPI_SUB %>% 
      filter(Var %in% Weights_3[sticky,"Var"])  %>%
      group_by(date) %>% 
      mutate(value = value*(Weight/100)/sum(Weight/100, na.rm =T)) %>% 
      select(-Weight) %>% 
      group_by(date) %>%  
      summarise(`Sticky` = sum(value)) %>% 
      gather(Var, value, -date)
  )%>% 
  mutate(value = grth_rate(value,L=4)) %>% 
  filter(!is.na(value))



P2.4 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "%TTY",title = "Sticky & non-sticky inflation")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")




# HHDL groceries

CPI_SUB_DAT <- CPI_SUB %>% 
  filter(Var %in% Weights_3[c(1:13,16:23),"Var"])  %>%
  group_by(date) %>% 
  mutate(value = value*(Weight/100)/sum(Weight/100)) %>% 
  select(-Weight) %>% 
  group_by(date) %>%  
  summarise(`Groceries, exc. fruit and veg` = sum(value)) %>% 
  gather(Var, value, -date) %>%
  bind_rows(
    CPI_SUB %>% 
      filter(Var %in% Weights_3[c(1:23),"Var"])  %>%
      group_by(date) %>% 
      mutate(value = value*(Weight/100)/sum(Weight/100, na.rm =T)) %>% 
      select(-Weight) %>% 
      group_by(date) %>%  
      summarise(`Groceries` = sum(value)) %>% 
      gather(Var, value, -date)
  )%>% 
  mutate(value = grth_rate(value,L=4)) %>% 
  filter(!is.na(value))



P2.5 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "%TTY",title = "Household groceries")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")


# Life admin

CPI_SUB_DAT <- CPI_SUB %>% 
  filter(Var %in% Weights_3[c(38,41,42,43,44,64,67,85,55),"Var"])  %>%
  group_by(date) %>% 
  mutate(value = value*(Weight/100)/sum(Weight/100)) %>% 
  select(-Weight) %>% 
  group_by(date) %>%  
  summarise(`Life Admin` = sum(value)) %>% 
  gather(Var, value, -date) %>%
  bind_rows(
    CPIQ_data %>% 
      filter(table_no =="640106") %>% 
      separate_series() %>% 
      filter(series_2 %in% c("All groups CPI")) %>%
      filter(series_1 == "Index Numbers") %>% 
      filter(data_type == "INDEX") %>% 
      select(date, value, series_2) %>% 
      rename(Var = series_2)
  )%>% 
  mutate(value = grth_rate(value,L=4)) %>% 
  filter(date >= "2002-03-01")



P2.6 <- CPI_SUB_DAT %>% 
  line_plot( axis_title_y = "%TTY",title = "Life admin and headline CPI")+
  theme(legend.position = "bottom")+
  add_callouts(CPI_SUB_DAT,x_col = "date",var_col = "Var",value_col = "value",colors = generate_pal(my_pal = "main", 2))+
  coord_cartesian(xlim = calc_xlim(CPI_SUB_DAT, "date", has_callouts = TRUE, callout_extension = 2), clip = "off")


#--------------------------Contributions to growth 


layout <- "
ABC
DEF
"

combined <- P1 + P2 + P3 + P4 + P5 + P6 + 
  plot_layout(design = layout) +
  plot_annotation(
    title = "Headline inflation & key categories",
    subtitle = "Inflation has ",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 14, hjust = 0)
    )
  )

combined



#-------------------------- Goods and Services / tradeables non-tradeable
GnS <- CPIQ_data %>% 
  filter(table_no =="640106") %>% 
  separate_series() %>% 
  filter(series_2 %in% c("All groups, goods component","All groups, services component")) %>%
  filter(series_1 == "Index Numbers") %>% 
  filter(data_type == "INDEX") %>% 
  select(date, value, series_2) %>% 
  rename(Var = series_2) %>%
  mutate(Var = gsub("All groups, goods component","Goods",Var)) %>% 
  
  mutate(Var = gsub("All groups, services component","Services",Var)) %>% 
  
  group_by(Var) %>% 
  mutate(value = grth_rate(value, L =4)) %>% 
  filter(date >= '2000-06-01')

TnT <- CPIQ_data %>% 
  filter(table_no =="640106") %>% 
  separate_series() %>% 
  filter(series_2 %in% c("Tradables","Non-tradables")) %>%
  filter(series_1 == "Index Numbers") %>% 
  filter(data_type == "INDEX") %>% 
  select(date, value, series_2) %>% 
  rename(Var = series_2) %>%
  group_by(Var) %>% 
  mutate(value = grth_rate(value, L =4)) %>% 
  filter(date >= '2000-06-01')

GnSplot <- line_plot(GnS, axis_title_y = "%TTY",title = "")
TnTplot <- line_plot(TnT, axis_title_y = "%TTY",title = "")


#-------------------------- Goods and Services / tradeables non-tradeable
# Long-term headline inflation, annotated graph

CPIQ_data %>% 
  filter(table_no =="640106") %>% 
  separate_series() %>% 
  filter(series_2 %in% c("All groups, goods component","All groups, services component")) %>%
  filter(series_1 == "Index Numbers") %>% 
  filter(data_type == "INDEX") %>% 
  select(date, value, series_2) %>% 
  rename(Var = series_2) 


