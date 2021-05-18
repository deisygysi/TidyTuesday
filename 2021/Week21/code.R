require(tidyr)
require(igraph)
require(dplyr)
require(magrittr)
require(ggplot2)
`%ni%` <- Negate(`%in%`)

tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey = tuesdata$survey

US = survey %>% filter(tolower(country) %in% 
                         c("america", "usa")) %>%
  mutate(gender = ifelse(gender %ni% c("Woman", "Man"), "Other", gender))


data_summary = US %>% 
  select(how_old_are_you, 
         gender, 
         race,  
         years_of_experience_in_field,
         highest_level_of_education_completed, 
         industry, annual_salary) %>%
  mutate(gender = ifelse(gender %ni% c("Woman", "Man"), "Other", gender)) %>%
  na.exclude() %>% 
  group_by(
    gender, 
    highest_level_of_education_completed, 
  ) %>%
  summarise(min_salary = min(annual_salary), 
            max_salary = max(annual_salary), 
            median_salary = median(annual_salary), 
            mean_salary = mean(annual_salary))

library(ggplot2)
p = data_summary %>% 
  ungroup() %>%
  mutate(gender = factor(gender, levels = c(
    "Man", "Woman", "Other"))) %>% 
  mutate(highest_level_of_education_completed = 
           factor(highest_level_of_education_completed, 
                  levels = c("High School",                         
                             "Some college",                        
                             "College degree",                      
                             "Master's degree",
                             "PhD",
                             "Professional degree (MD, JD, etc.)" ))) %>%
  ggplot() +
  aes(x = highest_level_of_education_completed, fill = gender, colour = gender, 
      weight = median_salary) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = list(Man = "#AFD2E9", 
                                  Other = "#B1B695", Woman = "#A690A4")) +
  scale_color_manual(values = list(Man = "#AFD2E9", Other = "#B1B695", 
                                   Woman = "#A690A4")) +
  labs(x = "Education",
       y = "Median Salary in USA", 
       title = "Does Education Pay?", 
       subtitle = "It does, mainly if you are not male.\nIf you are a women you are paied less, independent of your education.", 
       fill = "Gender", 
       color = "Gender", 
       caption =  "@GysiDeisy | #TidyTuesday \nSource: Ask a Manager Salary Survey"
       ) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_number()) + 
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "italic"), 
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"), 
        plot.background = element_rect(fill = "#FCD0A1"), 
        panel.grid = element_line(color = NA)
  )

png(filename = "result.png",
    width = 40, 
    height = 30, 
    units = 'cm',
    res = 400)
p
dev.off()