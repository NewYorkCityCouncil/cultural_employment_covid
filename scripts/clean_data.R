library(ggplot2)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(hrbrthemes)
options(scipen = 999)

#copy and paste, read_excel is reading in wrong data
new_data <- read_excel("NYCcurr.xlsx", col_names = F)
new_data <- new_data[-1,]

names(new_data)<- c("Industry", 'July_2020', 'June_20', 'July_2019',
                    'Month_Change',	'Per_Month_Change',
                    'Year_Change', 'Per_Year_Change')

# first visual ------
# subset to arts & select yr cols & add 000s back to data
arts<- new_data %>% 
  filter(Industry=="Arts, Entertainment, and Recreation") %>% 
  select(Industry, July_2020, July_2019) %>% 
  pivot_longer(!Industry, names_to = "Year", values_to = "Employment") %>% 
  mutate(Employment = Employment*1000)

arts_dec<- scales::comma(arts[arts$Year=="July_2020",]$Employment - 
  arts[arts$Year=="July_2019",]$Employment)

arts_per<- scales::percent((arts[arts$Year=="July_2020",]$Employment - 
                              arts[arts$Year=="July_2019",]$Employment)/
                             arts[arts$Year=="July_2019",]$Employment)

# to bold text
st<- bquote(~bold(.(arts_dec))~or~bold(.(arts_per))~bold(Decrease)~"of Arts, Entertainment, and Recreation Jobs in July 2020 as compared to July 2019")

# plot ----
ggplot(data = arts, 
       aes(x= Year, y= Employment)) +
  geom_bar(stat = "identity", width = .6, fill = '#4E398F') +
  scale_y_continuous(labels = scales::comma(seq(0,100000,20000)),
                     breaks = seq(0,100000,20000)) +
  geom_text(label = scales::comma(arts$Employment), vjust=3,
            color = "#FFFFFF", size = 6) +
  xlab("") + ylab("Jobs") + 
  labs(caption = "NYS Department of Labor - Labor Statistics for the New York City Region") +
  ggtitle("Figure 3.\n\nArts Employment During Covid", 
          st) +
  theme_ipsum(axis_title_just = "mc", 
             base_size = 10,
             axis_title_size = 11) +
  theme(legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14)) 



# second visual ------
# subset to all NYC industries & select yr cols & add 000s back to data
all<- new_data %>% 
  filter(Industry=="Total Nonfarm") %>% 
  select(Industry, July_2020, July_2019) %>% 
  pivot_longer(!Industry, names_to = "Year", values_to = "Employment") %>% 
  mutate(Employment = Employment*1000)

all_dec<- scales::comma(all[all$Year=="July_2020",]$Employment - 
                           all[all$Year=="July_2019",]$Employment)

all_per<- scales::percent((all[all$Year=="July_2020",]$Employment - 
                              all[all$Year=="July_2019",]$Employment)/
                             all[all$Year=="July_2019",]$Employment)

# to bold text
st<- bquote(~bold(.(all_dec))~or~bold(.(all_per))~bold(Decrease)~"of Jobs in July 2020 as compared to July 2019")

# plot -----
ggplot(data = all, 
       aes(x= Year, y= Employment)) +
  geom_bar(stat = "identity", width = .6, fill = '#2F56A6') +
  scale_y_continuous(labels = scales::comma(seq(0,5500000,500000)),
                     breaks = seq(0,5500000,500000)) +
  geom_text(label = scales::comma(all$Employment), vjust=3,
            color = "#FFFFFF", size = 6) +
  xlab("") + ylab("Jobs") + 
  labs(caption = "NYS Department of Labor - Labor Statistics for the New York City Region") +
  ggtitle("Figure 2.\n\nNYC Employment During Covid", 
          st) +
  theme_ipsum(axis_title_just = "mc", 
              base_size = 10,
              axis_title_size = 11) +
  theme(legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14)) 


# third visual -----
s1<- c("Total Nonfarm", "Total Private", "Health Care and Social Assistance",
       "Professional and Business Services", "Information",
       "Personal and Laundry Services", "Food Services and Drinking Places",
       "Clothing Stores", "Arts, Entertainment, and Recreation")
share_diff<- new_data %>% 
  filter(Industry %in% s1) %>% 
  select(Industry, July_2020, July_2019) %>% 
  mutate(share_20 = July_2020/3916.8,
         share_19 = July_2019/4631.5,
         diff_share = as.double((share_20 - share_19) / share_19),
         positive = diff_share > 0) %>% 
  filter(Industry != "Total Nonfarm" & Industry != "Total Private") 

share_diff[share_diff$Industry=="Arts, Entertainment, and Recreation",]$positive <- '#4E398F'
share_diff[share_diff$positive==FALSE,]$positive <- '#666666'
share_diff[share_diff$positive==TRUE,]$positive <- '#CACACA'

# plot -------
ggplot(data = share_diff,
       aes(x = reorder(Industry, diff_share), y = diff_share,
           fill = positive))+
  geom_bar(stat = "identity", width = 0.7, fill = share_diff$positive) +
  scale_y_continuous(breaks = seq(-1,0.3,0.1),
                     labels = scales::percent(seq(-1,0.3,0.1))) +
  expand_limits(y=c(-1,0.3)) +
  coord_flip() +
  xlab("") + ylab('Percent Change') +
  ggtitle('Figure 1.\n\nShare Loss/Gain of Employment for Select Industries During Covid', 
          'July 2019 vs July 2020') +
  labs(caption = "NYS Department of Labor - Labor Statistics for the New York City Region") +
  geom_text(data = share_diff[share_diff$diff_share>0,],
            label = scales::percent(
              share_diff[share_diff$diff_share>0,]$diff_share,
              accuracy = 1),
            hjust = -0.3) +
  geom_text(data = share_diff[share_diff$diff_share<0,],
            label = scales::percent(
              share_diff[share_diff$diff_share<0,]$diff_share, 
              accuracy = 1),
            hjust = 1.3) +
  theme_ipsum(axis_title_just = "mc", 
              base_size = 10,
              axis_title_size = 11) +
  theme(legend.position="none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        text = element_text(family = "Open Sans"),
        plot.title = element_text(family = "Georgia",size = 14)) 