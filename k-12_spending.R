library(tidyverse)
library(gganimate)
library(readxl)
library(scales)
library(ggrepel)
library(ggtext)
library(hrbrthemes)

#data####
edu_spending <- read_excel("edu_spending_gganimate.xlsx") %>% 
  mutate(`Total Expenditures` = as.integer(round(`Total Expenditures`, 0) ) ) %>% 
  mutate(Year = round(as.integer(Year), 0 )) #%>% filter(Year >= 1960)

# gganimate ####
p <- ggplot(data, aes(Year, `Total Expenditures`)) +
  geom_area(alpha = 0.1) +
  geom_point(color = "#FF6C30", size = 3) +
  scale_x_continuous(breaks = c(1920, 1940, 1960, 1980, 2000, 2016)) +
  scale_y_continuous(expand = c(0,0), 
                     labels = scales:: dollar_format()) +
  labs(x = "", y ="",
       title = "Since 1920, inflation-adjusted spending per student in public elementary <br>and secondary schools has **increased by 1,671%**.<br>",
       caption = "Source: NCES, Digest of Education Statistics 2019\nVisual: Allan Wheeler") +
  #theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal() +
  theme(plot.title = element_markdown(),
        plot.title.position = "plot", 
        #plot.caption.position =  "plot",
        panel.grid.minor = element_blank()
  )

anim2 <- p +
  transition_reveal(as.numeric(Year)) +
  view_follow(fixed_y = TRUE)

animate(anim2, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)

anim_save("k-12 total per student expenditure since 1920.gif")

