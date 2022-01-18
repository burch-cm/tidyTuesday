##### Packages #####
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(arrow)

##### Setup #####
# load Starbucks menu data
# dat <- arrow::read_parquet("./starbucks-menu/menu_data.parquet") # interactive
dat <- arrow::read_parquet("./menu_data.parquet") # app-loaded
dat <- dat |> 
    mutate(
        product_name = case_when(
            str_detect(product_name, "brewed coffee") ~ "brewed coffee",
            str_detect(product_name, "Clover Brewed Coffee") ~ "clover brewed coffee",
            TRUE ~ tolower(product_name)
        )
    )

##### UI #####
ui <- fluidPage(

    # Application title
    titlePanel("TidyTuesday 2021-12-21: Starbucks Menu"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "drinkSize",
                        label = "size",
                        choices = c("short",
                                    "tall",
                                    "grande", 
                                    "venti",
                                    "trenta",
                                    "solo",
                                    "doppio",
                                    "triple",
                                    "quad",
                                    "1 scoop",
                                    "1 shot"),
                        selected = "short"),
            selectInput(inputId = "milkType", 
                        label = "Milk Type", 
                        choices = list("none" = 0,
                                       "nonfat" = 1,
                                       "2%" = 2,
                                       "soy" = 3,
                                       "coconut" = 4,
                                       "whole" = 5),
                        selected = "none")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("caloriePlot")
        )
    )
)

##### Server #####
server <- function(input, output) {
    
    output$caloriePlot <- renderPlot({
        selected_dat <-
            dat |> 
            filter(size == input$drinkSize,
                   milk == input$milkType) |>
            arrange(desc(calories)) |> 
            slice(1:10)
            
        selected_dat$product_name <- 
            reorder(selected_dat$product_name, selected_dat$calories)
        
        selected_dat |> 
            ggplot(aes(x = product_name, y = calories)) +
            geom_bar(stat = "identity") +
            labs(x = "beverage", y = "calories") +
            coord_flip() +
            theme_bw()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
