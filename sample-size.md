---
title: "hairsalon"
author: "A.Amstutz"
date: "2023-10-09"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: hide
  pdf_document:
    toc: yes
---

# Load packages

```r
library(tidyverse)
library(ggplot2)
```

# Sample Size calculation, according to "Calculation of sample size for a single cross-sectional cluster survey"
1) Determine the target population: All young women (= potential clients) in Lesotho.
2) Determine estimated proportion of the outcome: We hypothesize that at least 70% of participants are comfortable with being offered any SRH service at the hair salon. Remember, in Bassett et al, J Acquir Immune Defic Syndr, it was over 90%.
3) Determine the level of confidence: 95% confidence. The probability that our sample results will be within the margin of error of the true population estimate. I.e, if we were to repeat our survey 100 times, we would get the same mean result about 95 times.
4) Determine the margin of error (precision): The maximum acceptable difference between our sample estimate and the true population parameter, i.e., how sure we need to be that the results are accurate for the entire population. Usually at around 5% or less. If we discover that 85% of our participating clients find the offer of any SRH service acceptable, we can safely assume that 80-90% of our clients (the total population, i.e. all young women in Lesotho) will accept it.
5) Estimate the intracluster correlation coefficient (ICC): The ICC measures the similarity or correlation between responses within the same cluster. Between 0-1. A higher ICC (closer to 1) implies more similarity (i.e. more correlation). Based on the ICC, the design effect, or inflation factor, is calculated.
Based on: https://mnsurvey.nutritionintl.org/categories/13 


```r
# Parameters for the sample size calculation
confidence_level <- 0.95 # confidence level
z <- qnorm(1 - (1 - confidence_level) / 2) # z statistic
d <- 0.05 # margin of error
cluster_size <- 3 # Average cluster size (fixed in our case)
icc <- 0.05 # Intracluster correlation coefficient -> Clients from the same hair stylist might be correlated in terms of outcome (acceptability), so, 0.05 is the lowest acceptable.
prop <- 0.77 # Estimated proportion (= 77% acceptability) -> rather low! The higher, the lower the sample size needed.
deff <- 1 + (cluster_size - 1) * icc # Design Effect
sample_size <- ceiling((z^2 * prop * (1 - prop) * deff) / d^2)
# Print the calculated sample size
cat("Required sample size:", sample_size)
```

```
## Required sample size: 300
```

```r
# Number of clusters
n_clusters <- sample_size / cluster_size
cat("Required clusters:", round(n_clusters, 0))
```

```
## Required clusters: 100
```

```r
# Create.a plot varying the ICC
icc_values <- seq(0.01, 0.09, by = 0.01)  # range of realistic ICCs
data_frame <- data.frame(ICC = icc_values)
# Calculate required sample size for each ICC value
data_frame$SampleSize <- ceiling((z^2 * prop * (1 - prop) * (1 + (cluster_size - 1) * data_frame$ICC)) / d^2)
# Create the plot
ss_plot <- ggplot(data_frame, aes(x = ICC, y = SampleSize)) +
  geom_line() +
  labs(
    x = "ICC (Intracluster Correlation Coefficient)",
    y = "Sample Size"
  ) +
  theme_minimal()
print(ss_plot)
```

![](sample-size_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
# Save the ggplot as an image file (e.g., PNG)
ggsave("sample_size_plot.png", plot = ss_plot, width = 6, height = 4, units = "in")

# Print the current version of R
cat("R version:", R.version$version.string)
```

```
## R version: R version 4.2.3 (2023-03-15)
```

