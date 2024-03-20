---
title: "Recruitment Monitoring"
author: "A.Amstutz"
date: "2024-03-20"
output:
  html_document:
    keep_md: yes
    toc: TRUE
    toc_float: TRUE
    code_folding: hide
  pdf_document:
    toc: yes
# runtime: shiny
# css: style.css
---
# PREPARATION
### Load packages

```r
library(tidyverse)
library(readxl)
library(ggplot2)
library(DT) # shiny app style table
library(formattable) # shiny app style table
library(leaflet) # gps map
library(remotes)
# remotes::install_gitlab("dickoa/robotoolbox")
library(robotoolbox)
library(plotly)
```

### Load data directly via accessing the REST API of KoboToolbox


### Functions, order and prepare dataframes

```r
# Create a function to activate the URL links
link_activator <- function(x) {
  sprintf('<a href="%s" target="_blank">%s</a>', x, x)
}
# Create a function to mark the duplicates in red, bold
dupl_checker <- formatter("span",
                          style = function(x) style(
                            color = ifelse(duplicated(x) == TRUE, "white", "black"),
                            background.color = ifelse(duplicated(x) == TRUE, "red", "white"),
                            font.weight = ifelse(duplicated(x) == TRUE, "bold", "normal")
                          ))
# Create a function to mark a duplicate in red, bold - WITHIN fullname
dupl_checker_within_fullname <- function(fullname_col, source_col) {
  formatter("span",
            style = function(x) style(
              color = ifelse(duplicated(paste(fullname_col, x)) & x %in% unique(x), "white", "black"),
              background.color = ifelse(duplicated(paste(fullname_col, x)) & x %in% unique(x), "red", "white"),
              font.weight = ifelse(duplicated(paste(fullname_col, x)) & x %in% unique(x), "bold", "normal")
            ))
}
# Create a function to mark the duplicates in green, bold
dupl_checker_masterlist <- formatter("span",
                          style = function(x) style(
                            color = ifelse(duplicated(x) == TRUE, "white", "black"),
                            background.color = ifelse(duplicated(x) == TRUE, "green", "white"),
                            font.weight = ifelse(duplicated(x) == TRUE, "bold", "normal")
                          ))
# Create a function to mark the missing in red, bold
missing_checker <- formatter("span",
                          style = function(x) style(
                            color = ifelse(is.na(x) == TRUE, "white", "black"),
                            background.color = ifelse(is.na(x) == TRUE, "red", "white"),
                            font.weight = ifelse(is.na(x) == TRUE, "bold", "normal")
                          ))
# Create a function to mark the ages outside 15-35 in orange, bold
age_checker <- formatter("span",
                         style = function(x) style(
                           color = ifelse(is.na(x) | x < 15 | x > 35, "white", "black"),
                           background.color = ifelse(is.na(x) | x < 15 | x > 35, "orange", "white"),
                           font.weight = ifelse(is.na(x) | x < 15 | x > 35, "bold", "normal")
                           ))
# Create a function to marks the gender men in orange, bold
gender_checker <- formatter("span",
                            style = function(x) style(
                              color = ifelse(grepl("_man", x, ignore.case = TRUE), "white", "black"),
                              background.color = ifelse(grepl("_man", x, ignore.case = TRUE), "orange", "white"),
                              font.weight = ifelse(grepl("_man", x, ignore.case = TRUE), "bold", "normal")
                            ))

### reformat all 4 questionnaires

# stylist registration
df_sr <- df_sr %>% mutate(fullname_s = paste(firstname_s, lastname_s, sep = " ")) # create fullname
df_sr <- df_sr %>% rename(submission_time_s = `_submission_time`,
                          id = `_id`)
df_sr$source <- "registration"
attachments_list_sr <- df_sr[["_attachments"]] # Extract the _attachments column that contains the salon pictures
flattened_list_sr <- as.data.frame(do.call(cbind, lapply(attachments_list_sr, function(x) setNames(as.data.frame(x), paste0(names(x), seq_along(x)))))) # Flatten the list without purrr and make column names unique
flattened_list_sr <- setNames(flattened_list_sr, make.unique(names(flattened_list_sr))) # Again, rename to unique columns
filtered_data <- flattened_list_sr %>% # Extracting only columns with download_small_url4.x and instance7.x
  select(contains("instance"),
         contains("download_small_url"))
# Reshaping the data so that we have rows with unique "instance" and 2 columns with each download_small_url (outside pic and inside pic)
reshaped_data <- filtered_data %>%
  mutate(row_id = row_number()) %>%
  gather(key, value, -row_id) %>%
  separate(key, into = c("variable", "index"), sep = "\\.") %>%
  spread(variable, value)
reshaped_data_renamed <- reshaped_data %>% 
  select(instance7, download_small_url4) %>% 
  rename(instance = instance7,
         download_small_url = download_small_url4)
final_data <- reshaped_data_renamed %>%
  group_by(instance) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = download_small_url) %>%
  rename(
    salon_pic_outside_s_URL = `1`,
    salon_pic_inside_s_URL = `2`,
    id = instance
  ) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE)
final_data$id <- as.numeric(final_data$id)
df_sr <- left_join(df_sr, final_data, by = join_by(id == id)) # Merge back
df_sr$salon_pic_inside_s_URL <- sapply(df_sr$salon_pic_inside_s_URL, link_activator) # Activate URL
df_sr$salon_pic_outside_s_URL <- sapply(df_sr$salon_pic_outside_s_URL, link_activator) # Activate URL
df_sr <- df_sr[order(df_sr$submission_time_s, decreasing = TRUE), ] # sort by latest submission date/time on top


# stylist survey
df_ss <- df_ss %>% mutate(fullname_s = paste(firstname_s, lastname_s, sep = " ")) # create fullname
df_ss <- df_ss %>% rename(submission_time_s = `_submission_time`,
                          id = `_id`)
df_ss$source <- "survey"
attachments_list_ss <- df_ss[["_attachments"]] # Extract the _attachments column that contains the pictures
flattened_list_ss <- as.data.frame(do.call(cbind, lapply(attachments_list_ss, function(x) setNames(as.data.frame(x), paste0(names(x), seq_along(x)))))) # Flatten the list without purrr and make column names unique
flattened_list_ss <- setNames(flattened_list_ss, make.unique(names(flattened_list_ss))) # Again, rename to unique columns
filtered_data <- flattened_list_ss %>% # Extracting only columns with download_small_url4.x and instance7.x
  select(contains("instance"),
         contains("download_small_url"))
# Reshaping the data so that we have rows with unique "instance" and 2 columns with each download_small_url (outside pic and inside pic)
reshaped_data <- filtered_data %>%
  mutate(row_id = row_number()) %>%
  gather(key, value, -row_id) %>%
  separate(key, into = c("variable", "index"), sep = "\\.") %>%
  spread(variable, value)
reshaped_data_renamed <- reshaped_data %>% 
  select(instance7, download_small_url4) %>% 
  rename(instance = instance7,
         download_small_url = download_small_url4)
final_data <- reshaped_data_renamed %>%
  group_by(instance) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = download_small_url) %>%
  rename(
    sign_s_URL = `1`,
    id = instance
  ) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE)
final_data$id <- as.numeric(final_data$id)
df_ss <- left_join(df_ss, final_data, by = join_by(id == id)) # Merge back
df_ss$sign_s_URL <- sapply(df_ss$sign_s_URL, link_activator) # Activate URL
df_ss <- df_ss[order(df_ss$submission_time_s, decreasing = TRUE), ] # sort by latest submission date/time on top


# client registration
df_cr <- df_cr %>% mutate(fullname_c = paste(firstname_c, lastname_c, sep = " ")) # create fullname
df_cr <- df_cr %>% mutate(fullname_s = paste(stylist_firstname_c, stylist_lastname_c, sep = " ")) # create fullname of the clients' stylist
df_cr <- df_cr %>% rename(submission_time_c = `_submission_time`,
                          id = `_id`)
df_cr$source <- "registration"
attachments_list_cr <- df_cr[["_attachments"]] # Extract the _attachments column that contains the pictures
flattened_list_cr <- as.data.frame(do.call(cbind, lapply(attachments_list_cr, function(x) setNames(as.data.frame(x), paste0(names(x), seq_along(x)))))) # Flatten the list without purrr and make column names unique
flattened_list_cr <- setNames(flattened_list_cr, make.unique(names(flattened_list_cr))) # Again, rename to unique columns
filtered_data <- flattened_list_cr %>% # Extracting only columns with download_small_url4.x and instance7.x
  select(contains("instance"),
         contains("download_small_url"))
# Reshaping the data so that we have rows with unique "instance" and 2 columns with each download_small_url (outside pic and inside pic)
reshaped_data <- filtered_data %>%
  mutate(row_id = row_number()) %>%
  gather(key, value, -row_id) %>%
  separate(key, into = c("variable", "index"), sep = "\\.") %>%
  spread(variable, value)
reshaped_data_renamed <- reshaped_data %>% 
  select(instance7, download_small_url4) %>% 
  rename(instance = instance7,
         download_small_url = download_small_url4)
final_data <- reshaped_data_renamed %>%
  group_by(instance) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = download_small_url) %>%
  rename(
    hair_pic_c_URL = `1`,
    id = instance
  ) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE)
final_data$id <- as.numeric(final_data$id)
df_cr <- left_join(df_cr, final_data, by = join_by(id == id)) # Merge back
df_cr$hair_pic_c_URL <- sapply(df_cr$hair_pic_c_URL, link_activator) # Activate URL
df_cr <- df_cr[order(df_cr$submission_time_c, decreasing = TRUE), ] # sort by latest submission date/time on top


# client survey
df_cs <- df_cs %>% mutate(fullname_c = paste(firstname_c, lastname_c, sep = " ")) # create fullname
df_cs <- df_cs %>% mutate(fullname_s = paste(stylist_firstname_c, stylist_lastname_c, sep = " ")) # create fullname of the clients' stylist
df_cs <- df_cs %>% rename(submission_time_c = `_submission_time`,
                          id = `_id`)
df_cs$source <- "survey"
attachments_list_cs <- df_cs[["_attachments"]] # Extract the _attachments column that contains the pictures
flattened_list_cs <- as.data.frame(do.call(cbind, lapply(attachments_list_cs, function(x) setNames(as.data.frame(x), paste0(names(x), seq_along(x)))))) # Flatten the list without purrr and make column names unique
flattened_list_cs <- setNames(flattened_list_cs, make.unique(names(flattened_list_cs))) # Again, rename to unique columns
filtered_data <- flattened_list_cs %>% # Extracting only columns with download_small_url4.x and instance7.x
  select(contains("instance"),
         contains("download_small_url"))
# Reshaping the data so that we have rows with unique "instance" and 2 columns with each download_small_url (outside pic and inside pic)
reshaped_data <- filtered_data %>%
  mutate(row_id = row_number()) %>%
  gather(key, value, -row_id) %>%
  separate(key, into = c("variable", "index"), sep = "\\.") %>%
  spread(variable, value)
reshaped_data_renamed <- reshaped_data %>% 
  select(instance7, download_small_url4) %>% 
  rename(instance = instance7,
         download_small_url = download_small_url4)
final_data <- reshaped_data_renamed %>%
  group_by(instance) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = download_small_url) %>%
  rename(
    sign_c_URL = `1`,
    id = instance
  ) %>%
  ungroup() %>%
  distinct(id, .keep_all = TRUE)
final_data$id <- as.numeric(final_data$id)
df_cs <- left_join(df_cs, final_data, by = join_by(id == id)) # Merge back
df_cs$sign_c_URL <- sapply(df_cs$sign_c_URL, link_activator) # Activate URL
df_cs <- df_cs[order(df_cs$submission_time_c, decreasing = TRUE), ] # sort by latest submission date/time on top
```

### Data cleaning


# HAIR SALON GPS

```r
leaflet(data = df_sr) %>%
  addTiles() %>%
  addMarkers(~salon_gps_s_longitude, ~salon_gps_s_latitude)
```

```{=html}
<div class="leaflet html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-04ca0ebe21f7044b87bd" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-04ca0ebe21f7044b87bd">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addMarkers","args":[[-29.375649,-29.271304,-29.623198,-28.766502],[27.543793,27.52884,27.495843,28.252393],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[-29.623198,-28.766502],"lng":[27.495843,28.252393]}},"evals":[],"jsHooks":[]}</script>
```

# RECRUITMENT STYLISTS

```r
df_sr$submission_date_only_s <- as.Date(df_sr$submission_time_s) # Extract Dates only from Date/Time variable
df_sr <- df_sr %>% arrange(submission_date_only_s) # Sort
# Calculate cumulative counts over time
total_counts_sr <- df_sr %>%
  mutate(Cumulative_Count = cumsum(rep(1, nrow(.)))) %>%
  select(submission_date_only_s, Cumulative_Count)

# Static Plot
# ggplot(total_counts_sr, aes(x = submission_date_only_s, y = Cumulative_Count)) +
#   geom_ribbon(aes(ymin = 0, ymax = Cumulative_Count), fill = "lightcoral", alpha = 0.3) +
#   geom_line(color = "darkred", size = 1) +
#   geom_point(color = "darkred", size = 3) +  # Increased size for better visibility
#   labs(title = "Cumulative Recruitment of Stylists",
#        x = "Date",
#        y = "Number of Stylists") +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.text = element_text(size = 10),  # Adjust axis text size
#         axis.title = element_text(size = 12, face = "bold"),  # Adjust axis title size
#         plot.title = element_text(size = 14, face = "bold"),  # Adjust plot title size
#         legend.position = "bottom")  # Move legend to the bottom, if applicable

# Interactive Plot
earlier_date <- min(df_sr$submission_date_only_s) - 1  # start plot 1 day earlier
hover_text <- paste("Fantastic - another Stylist recruited!")
plot_ly() %>%
  add_trace(x = ~total_counts_sr$submission_date_only_s, y = ~total_counts_sr$Cumulative_Count, type = "scatter", mode = "lines+markers", line = list(color = "darkred"), marker = list(color = "darkred", size = 5), text = hover_text, name = "1 Stylist") %>%
  add_trace(x = c(min(total_counts_sr$submission_date_only_s), total_counts_sr$submission_date_only_s, max(total_counts_sr$submission_date_only_s)), y = c(0, total_counts_sr$Cumulative_Count, tail(total_counts_sr$Cumulative_Count, 1)), type = "scatter", mode = "lines", fill = "tozeroy", line = list(color = "transparent"), name = "Shaded Area") %>%
  layout(
    # title = list(text = "Cumulative Recruitment of Stylists", font = list(size = 20, color = "black", family = "Arial", weight = "bold")),
         xaxis = list(title = "Date", range = c(earlier_date, max(total_counts_sr$submission_date_only_s) + 1), visible = TRUE),
         yaxis = list(title = "Number of Stylists", tickmode = "linear", dtick = 1),
         showlegend = F)
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-96c035f68ed25f2731bc" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-96c035f68ed25f2731bc">{"x":{"visdat":{"10c7224bd924":["function () ","plotlyVisDat"]},"cur_data":"10c7224bd924","attrs":{"10c7224bd924":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines+markers","line":{"color":"darkred"},"marker":{"color":"darkred","size":5},"text":"Fantastic - another Stylist recruited!","name":"1 Stylist","inherit":true},"10c7224bd924.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2024-03-18","2024-03-18","2024-03-18","2024-03-18","2024-03-20","2024-03-20"],"y":[0,1,2,3,4,4],"type":"scatter","mode":"lines","fill":"tozeroy","line":{"color":"transparent"},"name":"Shaded Area","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"Date","range":["2024-03-17","2024-03-21"],"visible":true},"yaxis":{"domain":[0,1],"automargin":true,"title":"Number of Stylists","tickmode":"linear","dtick":1},"showlegend":false,"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["2024-03-18","2024-03-18","2024-03-18","2024-03-20"],"y":[1,2,3,4],"type":"scatter","mode":"lines+markers","line":{"color":"darkred"},"marker":{"color":"darkred","size":5,"line":{"color":"rgba(31,119,180,1)"}},"text":["Fantastic - another Stylist recruited!","Fantastic - another Stylist recruited!","Fantastic - another Stylist recruited!","Fantastic - another Stylist recruited!"],"name":"1 Stylist","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"fillcolor":"rgba(255,127,14,0.5)","x":["2024-03-18","2024-03-18","2024-03-18","2024-03-18","2024-03-20","2024-03-20"],"y":[0,1,2,3,4,4],"type":"scatter","mode":"lines","fill":"tozeroy","line":{"color":"transparent"},"name":"Shaded Area","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

# RECRUITMENT CLIENTS

```r
df_cr$submission_date_only_c <- as.Date(df_cr$submission_time_c) # Extract Dates only from Date/Time variable
df_cr <- df_cr %>% arrange(submission_date_only_c) # Sort
# Calculate cumulative counts over time
total_counts_cr <- df_cr %>%
  mutate(Cumulative_Count = cumsum(rep(1, nrow(.)))) %>%
  select(submission_date_only_c, Cumulative_Count)

# Interactive Plot
earlier_date <- min(df_sr$submission_date_only_c) - 1  # start plot 1 day earlier
hover_text <- paste("Fantastic - another Client recruited!")
plot_ly() %>%
  add_trace(x = ~total_counts_cr$submission_date_only_c, y = ~total_counts_cr$Cumulative_Count, type = "scatter", mode = "lines+markers", line = list(color = "darkred"), marker = list(color = "darkred", size = 5), text = hover_text, name = "1 Client") %>%
  add_trace(x = c(min(total_counts_cr$submission_date_only_c), total_counts_cr$submission_date_only_c, max(total_counts_cr$submission_date_only_c)), y = c(0, total_counts_cr$Cumulative_Count, tail(total_counts_cr$Cumulative_Count, 1)), type = "scatter", mode = "lines", fill = "tozeroy", line = list(color = "transparent"), name = "Shaded Area") %>%
  layout(
    # title = list(text = "Cumulative Recruitment of Stylists", font = list(size = 20, color = "black", family = "Arial", weight = "bold")),
         xaxis = list(title = "Date", range = c(earlier_date, max(total_counts_cr$submission_date_only_c) + 1), visible = TRUE),
         yaxis = list(title = "Number of Clients", tickmode = "linear", dtick = 1),
         showlegend = F)
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-61e9955b8915f90e357c" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-61e9955b8915f90e357c">{"x":{"visdat":{"10c721cfee5e6":["function () ","plotlyVisDat"]},"cur_data":"10c721cfee5e6","attrs":{"10c721cfee5e6":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines+markers","line":{"color":"darkred"},"marker":{"color":"darkred","size":5},"text":"Fantastic - another Client recruited!","name":"1 Client","inherit":true},"10c721cfee5e6.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2024-03-18","2024-03-18","2024-03-18","2024-03-19","2024-03-19","2024-03-19","2024-03-19","2024-03-19","2024-03-19","2024-03-20","2024-03-20","2024-03-20","2024-03-20","2024-03-20"],"y":[0,1,2,3,4,5,6,7,8,9,10,11,12,12],"type":"scatter","mode":"lines","fill":"tozeroy","line":{"color":"transparent"},"name":"Shaded Area","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"Date","range":[null,19803],"visible":true},"yaxis":{"domain":[0,1],"automargin":true,"title":"Number of Clients","tickmode":"linear","dtick":1},"showlegend":false,"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["2024-03-18","2024-03-18","2024-03-19","2024-03-19","2024-03-19","2024-03-19","2024-03-19","2024-03-19","2024-03-20","2024-03-20","2024-03-20","2024-03-20"],"y":[1,2,3,4,5,6,7,8,9,10,11,12],"type":"scatter","mode":"lines+markers","line":{"color":"darkred"},"marker":{"color":"darkred","size":5,"line":{"color":"rgba(31,119,180,1)"}},"text":["Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!","Fantastic - another Client recruited!"],"name":"1 Client","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"fillcolor":"rgba(255,127,14,0.5)","x":["2024-03-18","2024-03-18","2024-03-18","2024-03-19","2024-03-19","2024-03-19","2024-03-19","2024-03-19","2024-03-19","2024-03-20","2024-03-20","2024-03-20","2024-03-20","2024-03-20"],"y":[0,1,2,3,4,5,6,7,8,9,10,11,12,12],"type":"scatter","mode":"lines","fill":"tozeroy","line":{"color":"transparent"},"name":"Shaded Area","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```


