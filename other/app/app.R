library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(arrow)

# Read the Parquet file
data <- arrow::read_parquet("../../data/analysis_data/powerlifting_analysis_data.parquet")

# Define UI
ui <- fluidPage(
  titlePanel("Powerlifting Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("Average Total Weight Lifted by Age Class and Sex", "Average Total Weight Lifted by Weight Class and Sex", "Average Total Weight Lifted by Equipment and Sex"),
                  selected = NULL)
    ),
    mainPanel(
      plotOutput("powerliftingPlot"),
      DTOutput("powerliftingTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Function to create plots based on selected plot type
  generate_plot <- function(data, plot_type) {
    switch(plot_type,
           "Average Total Weight Lifted by Age Class and Sex" = {
             # Select relevant columns
             plot_data <- data %>% 
               select(sex, age_class, total_kg)
             # Convert age_class to a factor with desired level order
             plot_data$age_class <- factor(plot_data$age_class, levels = c("5-12", "13-15", "16-17", "18-19", "20-23", "24-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-999"))
             # Now sort the dataset
             plot_data <- plot_data[order(plot_data$age_class), ]
             # Calculate the average weight lifted by each age class and sex
             averages <- plot_data %>%
               group_by(age_class, sex) %>%
               summarise(
                 avg_total_kg = mean(total_kg)
               )
             # Melt the data into long format
             melted_data <- reshape2::melt(averages, id.vars = c("age_class", "sex"), variable.name = "exercise")
             # Create separate plots for each sex category
             plot <- melted_data %>%
               ggplot(aes(x = age_class, y = value, fill = sex)) +
               geom_bar(stat = "identity", position = "dodge", width = 0.7) +
               labs(x = "Age Class", y = "Average Total Weight Lifted (kg)", fill = "Sex") +
               theme_minimal() +
               theme(
                 axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
                 axis.ticks.x = element_blank()  # Remove x-axis ticks
               )
             return(plot)
           },
           "Average Total Weight Lifted by Weight Class and Sex" = {
             # Select relevant columns
             plot_data <- data %>% 
               select(sex, weight_class_kg, total_kg)
             # Convert weight_class_kg to a factor with desired level order
             plot_data$weight_class_kg <- factor(plot_data$weight_class_kg, levels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99", "100-109", "110-129", "130-145", "150+"))
             # Now sort the dataframe
             plot_data <- plot_data[order(plot_data$weight_class_kg), ]
             # Calculate the average weight lifted by each weight class and sex
             averages <- plot_data %>%
               group_by(sex, weight_class_kg) %>%
               summarise(
                 avg_total_kg = mean(total_kg)
               )
             # Melt the data into long format
             melted_data <- reshape2::melt(averages, id.vars = c("weight_class_kg", "sex"), variable.name = "exercise")
             melted_data <- na.omit(melted_data)
             # Create separate plots for each sex category
             plot <- melted_data %>%
               ggplot(aes(x = weight_class_kg, y = value, fill = sex)) +
               geom_bar(stat = "identity", position = "dodge", width = 0.7) +
               labs(x = "Weight Class", y = "Average Total Weight Lifted (kg)", fill = "Sex") +
               theme_minimal() +
               theme(
                 axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
                 axis.ticks.x = element_blank()  # Remove x-axis ticks
               )
             return(plot)
           },
           "Average Total Weight Lifted by Equipment and Sex" = {
             # Select relevant columns
             plot_data <- data %>% 
               select(equipment, total_kg, sex)
             # Calculate the average weight lifted by each equipment and sex
             averages <- plot_data %>%
               group_by(sex, equipment) %>%
               summarise(
                 avg_total_kg = mean(total_kg)
               )
             # Create separate plots for each sex category
             plot <- averages %>%
               ggplot(aes(x = equipment, y = avg_total_kg, fill = sex)) +
               geom_bar(stat = "identity", position = "dodge", width = 0.7) +
               labs(x = "Equipment", y = "Average Total Weight Lifted (kg)", fill = "Sex") +
               theme_minimal() +
               theme(
                 axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
                 axis.ticks.x = element_blank()  # Remove x-axis ticks
               )
             return(plot)
           }
    )
  }
  
  # Render interactive plot
  output$powerliftingPlot <- renderPlot({
    if (!is.null(input$plot_type)) {
      generate_plot(data, input$plot_type)
    }
  })
  
  # Render interactive table
  output$powerliftingTable <- renderDT({
    datatable(data, options = list(pageLength = 10))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
