#load necessary packages

library(tidyverse)
library(here)

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#load standards data

stds <- read_csv("Seawater_Standard_148045_table1.csv")

#Make boxplots of different line data

fill <- "#4271AE"
line <- "#1F3552"


D14C_box <- ggplot(stds, aes(x = as.factor(ws_line_num), y = fm_corr)) +
  geom_boxplot(fill = fill, color = line, notch = TRUE) +
  scale_x_discrete(name = "Line Number") +
  scale_y_continuous(name = "D14C, o/oo") +
  ggtitle("Boxplot of median DI14C for NOSAMS secondary standards") +
  theme_bw() + 
  geom_jitter(alpha = 0.4, size = 0.2)

D14C_box

D13C_box <- ggplot(stds, aes(x = as.factor(ws_line_num), y = dc13)) +
  geom_boxplot(fill = fill, color = line, notch = TRUE) +
  scale_x_discrete(name = "Line Number") +
  scale_y_continuous(name = "d13C, o/oo") +
  ggtitle("Boxplot of median DI13C for NOSAMS secondary standards") +
  theme_bw() + 
  geom_jitter(alpha = 0.4, size = 0.2)

D13C_box

#Make boxplots ocmparing different methods

#add a column that separates out by method

stds <- stds %>%
  mutate(method = ifelse(ws_line_num == 10, "REDICS", "WSL"))

D14C_box <- ggplot(stds, aes(x = as.factor(method), y = fm_corr)) +
  geom_boxplot(fill = fill, color = line, notch = TRUE) +
  scale_x_discrete(name = "Line Number") +
  scale_y_continuous(name = "D14C, o/oo") +
  ggtitle("Boxplot of median DI14C for NOSAMS secondary standards") +
  theme_bw() + 
  geom_jitter(alpha = 0.4, size = 0.2)

D14C_box

D13C_box <- ggplot(stds, aes(x = as.factor(method), y = dc13)) +
  geom_boxplot(fill = fill, color = line, notch = TRUE) +
  scale_x_discrete(name = "Line Number") +
  scale_y_continuous(name = "d13C, o/oo") +
  ggtitle("Boxplot of median DI13C for NOSAMS secondary standards") +
  theme_bw() + 
  geom_jitter(alpha = 0.4, size = 0.2) 

D13C_box

d13C_box_stats <- boxplot(dc13 ~ method, data = stds)
write.csv(d13C_box_stats, file = "d13C_box_stats.csv")
fm_box_stats <- boxplot(fm_corr ~ method, data = stds)
write.csv(fm_box_stats, file = "fm_box_stats.csv")

d13C_box_stats3 <- boxplot(dc13 ~ ws_line_num, data = stds)
d13C_box_stats3
write(d13C_box_stats, file = "d13C_box_stats.txt")
fm_box_stats3 <- boxplot(fm_corr ~ ws_line_num, data = stds)
fm_box_stats3

iso_stats <- stds %>%
  group_by(ws_line_num) %>%
  summarize(avg13 = mean(dc13, na.rm = TRUE), stand13 = sd(dc13, na.rm=TRUE), num13 = n(), 
              avg14 = mean(fm_corr, na.rm = TRUE), stand14 = sd(fm_corr, na.rm=TRUE), num14 = n()) 
  
  write.csv(iso_stats, file = "iso_stats.csv")

iso_stat_sort <- gather(iso_stats, '2', '3', '10', key = "ws_line_num")  
  
stds %>%
  group_by(method) %>%
  summarize(avg = mean(fm_corr, na.rm = TRUE), stand = sd(fm_corr, na.rm=TRUE), num = n()) 

Line10 <- filter(stds, ws_line_num == 10)

iso_stats




