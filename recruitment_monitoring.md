---
title: "Recruitment Monitoring"
author: "A.Amstutz"
date: "2024-03-01"
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
library(scales) # date_function
```

### Load Data


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

# Create a function to mark the duplicates in green, bold
dupl_checker_masterlist <- formatter("span",
                          style = function(x) style(
                            color = ifelse(duplicated(x) == TRUE, "white", "black"),
                            background.color = ifelse(duplicated(x) == TRUE, "green", "white"),
                            font.weight = ifelse(duplicated(x) == TRUE, "bold", "normal")
                          ))
# stylist registration
df_sr <- df_sr %>% mutate(fullname_s = paste(firstname_s, lastname_s, sep = " "))
df_sr <- df_sr %>% rename(submission_time_s = `_submission_time`)
df_sr <- df_sr[order(df_sr$submission_time_s, decreasing = TRUE), ]
df_sr$strange <- NA
df_sr$sent <- NA
df_sr$salon_pic_inside_s_URL <- sapply(df_sr$salon_pic_inside_s_URL, link_activator)
df_sr$salon_pic_outside_s_URL <- sapply(df_sr$salon_pic_outside_s_URL, link_activator)
df_sr$source <- "registration"

# stylist survey
df_ss <- df_ss %>% mutate(fullname_s = paste(firstname_s, lastname_s, sep = " "))
df_ss <- df_ss %>% rename(submission_time_s = `_submission_time`)
df_ss <- df_ss[order(df_ss$submission_time_s, decreasing = TRUE), ]
df_ss$strange <- NA
df_ss$sign_s_001_URL <- sapply(df_ss$sign_s_001_URL, link_activator)
df_ss$source <- "survey"

# client registration
df_cr <- df_cr %>% mutate(fullname_c = paste(firstname_c, lastname_c, sep = " "))
df_cr <- df_cr %>% mutate(fullname_s = paste(stylist_firstname_c, stylist_lastname_c, sep = " "))
df_cr <- df_cr %>% rename(submission_time_c = `_submission_time`)
df_cr <- df_cr[order(df_cr$submission_time_c, decreasing = TRUE), ]
df_cr$strange <- NA
df_cr$sent <- NA
df_cr$hair_pic_c_URL <- sapply(df_cr$hair_pic_c_URL, link_activator)
df_cr$source <- "registration"

# client survey
df_cs <- df_cs %>% mutate(fullname_c = paste(firstname_c, lastname_c, sep = " "))
df_cs <- df_cs %>% mutate(fullname_s = paste(stylist_firstname_c, stylist_lastname_c, sep = " "))
df_cs <- df_cs %>% rename(submission_time_c = `_submission_time`)
df_cs <- df_cs[order(df_cs$submission_time_c, decreasing = TRUE), ]
df_cs$strange <- NA
df_cs$sign_c_URL <- sapply(df_cs$sign_c_URL, link_activator)
df_cs$source <- "survey"
```

### Data cleaning


# HAIR SALON GPS

```r
leaflet(data = df_sr) %>%
  addTiles() %>%
  addMarkers(~`_salon_gps_s_longitude`, ~`_salon_gps_s_latitude`, popup = ~fullname_s)
```

```{=html}
<div class="leaflet html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-7c0ff3cd332e77b73702" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-7c0ff3cd332e77b73702">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addMarkers","args":[[47.050172,47.05012,47.050119],[8.310062,8.31008,8.31008],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Alain Amstutz","Alain Amstutz","Katleho Thabane"],null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[47.050119,47.050172],"lng":[8.310062,8.31008]}},"evals":[],"jsHooks":[]}</script>
```

# RECRUITMENT STYLISTS

```r
## Plot 2: without names and including recruitment trajectory
# Extract Dates
df_sr$submission_date_only_s <- as.Date(df_sr$submission_time_s)
# Aggregate the total number of stylist registrations for each date
recruitment_counts_total <- table(df_sr$fullname_s, df_sr$submission_date_only_s)
# Convert to data frame and handle duplicates
recruitment_df_total <- as.data.frame(recruitment_counts_total) %>%
  group_by(Date = as.Date(Var2), add = TRUE) %>%
  summarise(Count = sum(Freq))
# Plotting
ggplot(recruitment_df_total, aes(x = Date, y = Count)) +
  geom_bar(stat = "identity", fill = "#1f78b4") +  
  geom_line(color = "#e31a1c", size = 1.5) +  
  labs(title = "Recruitment Plot",
       x = "Date",
       y = "Total Number of Stylists") +
  theme_minimal() +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "1 month")
```

![](recruitment_monitoring_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

# RECRUITMENT CLIENTS

```r
## Plot 2: without names and including recruitment trajectory
# Extract Dates
df_cr$submission_date_only_c <- as.Date(df_cr$submission_time_c)
# Aggregate the total number of Client registrations for each date
recruitment_counts_total <- table(df_cr$fullname_c, df_cr$submission_date_only_c)
# Convert to data frame and handle duplicates
recruitment_df_total <- as.data.frame(recruitment_counts_total) %>%
  group_by(Date = as.Date(Var2), add = TRUE) %>%
  summarise(Count = sum(Freq))
# Plotting
ggplot(recruitment_df_total, aes(x = Date, y = Count)) +
  geom_bar(stat = "identity", fill = "#1f78b4") +  
  geom_line(color = "#e31a1c", size = 1.5) +  
  labs(title = "Recruitment Plot",
       x = "Date",
       y = "Total Number of Clients") +
  theme_minimal() +
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "1 month")
```

![](recruitment_monitoring_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


