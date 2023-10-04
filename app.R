# Load the necessary libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)

#UI
ui <- fluidPage(
  titlePanel("Global YouTube Statistics 2023"),
  theme = shinytheme('yeti'),
  selectInput(
    "visualization_type",
    "Choose Visualization Type",
    choices = c(
      "Select Plot Type",
      "Distribution of Categories",
      "Distribution of Subscribers",
      "Mean Subscribers by Channel Type",
      "Stacked Channel Types by Category",
      "Boxplot of Video Views by Category",
      "Top 10 YouTubers vs. Country",
      "Custom"
    )
  ),
  
  conditionalPanel(
    condition = "input.visualization_type == 'Custom'",
    selectInput(
      "plot_type",
      "Select Plot Type:",
      choices = c("Scatter Plot", "Bar Plot", "Line Plot")
    ),
    selectInput("x_axis", "Select X-Axis Variable:", choices = colnames(yt)),
    selectInput("y_axis", "Select Y-Axis Variable:", choices = colnames(yt))
    
  ),
  
  
  conditionalPanel(
    condition = "input.visualization_type == 'Distribution of Subscribers'",
    sliderInput(
      "binwidth",
      "Select Binwidth for Distribution of Subscribers:",
      min = 100000,
      max = 10000000,
      value = 1000000
    )
  ),
  
  conditionalPanel(condition = "input.visualization_type == 'Bar plot of Unemployment Rate by Country'",
                   plotOutput("unemployment_plot")),
  
  
  
  
  plotOutput("plot")
)

# server
server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$visualization_type == "Distribution of Categories") {
      yt %>%
        group_by(category) %>%
        summarise(count = n()) %>%
        ggplot(aes(
          x = reorder(category,-count),
          y = count,
          fill = category,
          label = count
        )) +
        geom_bar(stat = "identity") +
        geom_text(size = 3, vjust = -0.5) +
        theme_minimal() +
        labs(title = "Distribution of Categories", x = "Category", y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$visualization_type == "Distribution of Subscribers") {
      ggplot(yt, aes(x = subscribers)) +
        geom_histogram(
          binwidth = input$binwidth,
          fill = "blue",
          color = "black"
        ) +
        labs(title = "Distribution of Subscribers", x = "Subscribers")
    } else if (input$visualization_type == "Mean Subscribers by Channel Type") {
      channel_type_subscribers <- yt %>%
        group_by(channel_type) %>%
        summarize(mean_subscribers = mean(subscribers))
      
      ggplot(
        channel_type_subscribers,
        aes(
          x = channel_type,
          y = mean_subscribers,
          label = scales::comma(mean_subscribers)
        )
      )+
      
         geom_bar(stat = "identity", fill = "purple") +
        geom_text(size = 3,
                  hjust = 1,
                  vjust = 0.5)+
        coord_flip()+ 
       labs(title = "Mean Subscribers by Channel Type", x = "Channel Type", y = "Mean Subscribers")
    } else if (input$visualization_type == "Stacked Channel Types by Category") {
      # Stacked bar plot showing distribution of channel types within categories
      custom_palette <-
        scales::hue_pal()(n = 15)  # Custom color palette
      
      yt %>%
        ggplot(aes(x = category, fill = channel_type)) +
        geom_bar(position = "stack") +
        labs(title = "Distribution of Channel Types within Different Categories",
             x = "Category",
             y = "Count") +
        theme_minimal() +
        scale_fill_manual(values = custom_palette) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$visualization_type == "Boxplot of Video Views by Category") {
      # Boxplot of video views by category
      ggplot(yt, aes(x = category, y = video.views)) +
        geom_boxplot(fill = "lightgreen",
                     color = "black") +
        coord_flip() +
        labs(title = "Boxplot of Video Views by Category", x = "Category", y = "Video Views")
    } else if (input$visualization_type == "Top 10 YouTubers vs. Country") {
      # Create a subset of the top 10 YouTubers
      top_10_youtubers <- yt[1:10,]
      
      # Create the scatter plot
      ggplot(top_10_youtubers,
             aes(x = rank, y = Country, color = subscribers)) +
        geom_point(size = 3) +
        labs(
          title = "Top 10 YouTubers vs. Country",
          x = "Rank",
          y = "Country",
          color = "Subscribers"
        ) +
        theme_minimal() +
        scale_color_gradient(low = "blue", high = "red")  # Color scale from blue to red
      
    } else if (input$visualization_type == "Custom") {
      # Custom plot based on user-selected X and Y axes
      x_var <- input$x_axis
      y_var <- input$y_axis
      plot_type <- input$plot_type  # Retrieve plot_type correctly
      binwidth <- input$binwidth
      
      # Check if the selected variables are valid and plot 
      if (plot_type == "Scatter Plot") {
        ggplot(yt, aes(x = !!sym(x_var), y = !!sym(y_var))) +  # Use !!sym() for variable names (debugging wih the use of ChatGpt)
          geom_point() +
          labs(x = x_var, y = y_var) +
          theme_minimal()
      } else if (plot_type == "Bar Plot") {
        # Create a bar plot
        ggplot(yt, aes(x = !!sym(x_var), y = !!sym(y_var))) +  # Use !!sym() for variable names (debugging wih the use of ChatGpt)
          geom_bar(stat = "identity", fill = "blue") +
          labs(x = x_var, y = y_var) +
          theme_minimal()
      } else if (plot_type == "Line Plot") {
        # Create a line plot
        ggplot(yt, aes(x = !!sym(x_var), y = !!sym(y_var))) +  # Use !!sym() for variable names(debugging wih the use of ChatGpt)
          geom_line(color = "red") +
          labs(x = x_var, y = y_var) +
          theme_minimal()
      }
    
    
    }
  })
}

shinyApp(ui = ui, server = server)
