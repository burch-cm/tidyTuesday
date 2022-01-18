library(tidyverse)
library(tidytuesdayR)


starbucks <- tidytuesdayR::tt_load('2021-12-21')
starbucks <- starbucks[["starbucks"]]

glimpse(starbucks)

# summary stats
library(corrr)
cor_df <- 
  starbucks |> 
  select(where(is.numeric)) |> 
  corrr::correlate() 
  
corrr::rplot(cor_df)
corrr::network_plot(cor_df)

starbucks |> 
  ggplot(aes(x = caffeine_mg)) +
  geom_density(fill = "#00704A", col = "#27251F") +
  theme_bw()

averages <- 
  starbucks |> 
  transmute(product_name,
            cal_ml = calories / serv_size_m_l,
            caff_mg_ml = caffeine_mg / serv_size_m_l) |> 
  group_by(product_name) |> 
  summarize(avg_cal_ml = mean(cal_ml, na.rm = TRUE),
            avg_mg_caf_ml = mean(caff_mg_ml, na.rm = TRUE))

averages |> 
  drop_na() |> 
  ggplot(aes(x = avg_cal_ml, y = avg_mg_caf_ml)) +
  geom_point()

library(plotly)
averages |>
  drop_na() |> 
  plot_ly(x = ~avg_cal_ml, y = ~avg_mg_caf_ml,
          text = ~product_name,
          type = 'scatter', 
          mode = 'markers')
  
# features

milk_fct <-
  data.frame(milk = c(0, 1, 2, 3, 4, 5),
             milk_type = c("none", "nonfat", "2%", "soy",
                           "coconut", "whole"))
sb <- 
  starbucks |> 
  left_join(milk_fct, by = "milk")
glimpse(sb)

sb |> 
  transmute(product_name,
            milk_type,
            cal_ml = calories / serv_size_m_l,
            caff_mg_ml = caffeine_mg / serv_size_m_l) |> 
  group_by(product_name, milk_type) |> 
  summarize(avg_cal_ml = mean(cal_ml, na.rm = TRUE),
            avg_mg_caf_ml = mean(caff_mg_ml, na.rm = TRUE)) |> 
  drop_na() |> 
  plot_ly(x = ~avg_cal_ml, y = ~avg_mg_caf_ml,
          text = ~paste("Product:", product_name, "<br>Milk:", milk_type),
          color = ~milk_type,
          type = 'scatter', 
          mode = 'markers')


##### analysis #####

# compare calories
cal_count <- 
  sb |> 
  filter(size == "grande",       # compare same size
         milk %in% c(0, 5),      # no milk and whole milk
         whip == 0) |>           # no whipped cream
  mutate(
    product_name = case_when(
      str_detect(product_name, "brewed coffee") ~ "brewed coffee",
      str_detect(product_name, "Clover Brewed Coffee") ~ "clover brewed coffee",
      TRUE ~ tolower(product_name)
    )
  ) 
# cal_count |> 
  
  dat |> 
    filter(milk == 5, whip == 0, size == "grande") |> 
  arrange(desc(calories)) |> 
  slice(1:10) |> 
  ggplot(aes(x = reorder(product_name, calories), y = calories)) +
  geom_bar(stat = "identity") +
  labs(x = "beverage", y = "calories",
       title = "Highest-calorie beverages at Starbucks") +
  coord_flip()
