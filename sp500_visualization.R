# Install and load required packages
if (!require("quantmod")) install.packages("quantmod")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotly")) install.packages("plotly")
library(quantmod)
library(dplyr)
library(lubridate)
library(plotly)

# Custom color palette
custom_colors <- c( "#4CAF50", "#FF6B6B", "#5D9CEC", "#9C27B0") # Red, Green, Blue, Purple

# Download S&P 500 data using tomorrow's date to ensure we get today's data
tomorrow_date <- Sys.Date() + 1
sp500 <- getSymbols("^GSPC", from = "2007-01-01", to = tomorrow_date, auto.assign = FALSE)

# Convert to data frame
sp500_df <- data.frame(Date = index(sp500), coredata(sp500))
sp500_df <- sp500_df %>% select(Date, GSPC.Adjusted)
sp500_df$Date <- as.Date(sp500_df$Date)

# Function to create indexed series from a particular date until it turns positive
create_indexed_series <- function(data, start_date) {
  # Filter data from start date onwards
  filtered_data <- data %>% filter(Date >= start_date)
  
  # Get the starting price
  start_price <- filtered_data$GSPC.Adjusted[1]
  
  # Calculate the percentage change from start date (indexed to 0%)
  filtered_data$pct_change <- (filtered_data$GSPC.Adjusted / start_price - 1) * 100
  
  # Add "Days since peak" column
  filtered_data$days_since_peak <- 0:(nrow(filtered_data)-1)
  
  # Find the first date when the percentage change turns positive after being negative
  first_negative <- which(filtered_data$pct_change < 0)[1]
  
  # If it starts negative, find when it turns positive
  if (!is.na(first_negative)) {
    # Look for the first positive after the first negative
    negative_period <- filtered_data[first_negative:nrow(filtered_data), ]
    first_positive_after_negative <- which(negative_period$pct_change >= 0)[1]
    
    if (!is.na(first_positive_after_negative)) {
      end_row <- first_negative + first_positive_after_negative - 1
    } else {
      end_row <- nrow(filtered_data)  # Never turned positive, take all data
    }
  } else {
    end_row <- nrow(filtered_data)  # Never went negative, take all data
  }
  
  # Truncate the data
  result <- filtered_data[1:end_row, ]
  result$start_date <- as.character(start_date)
  
  # Add end date information
  if (end_row < nrow(filtered_data)) {
    result$end_date <- as.character(filtered_data$Date[end_row])
  } else {
    result$end_date <- "No recovery yet"
  }
  
  return(result)
}

# Create all four series
series1 <- create_indexed_series(sp500_df, as.Date("2007-10-09"))
series2 <- create_indexed_series(sp500_df, as.Date("2020-02-19"))
series3 <- create_indexed_series(sp500_df, as.Date("2022-01-03"))
series4 <- create_indexed_series(sp500_df, as.Date("2025-02-19"))

# Combine the series for plotting
all_series <- rbind(series1, series2, series3, series4)

# Create custom legend labels
all_series <- all_series %>%
  group_by(start_date) %>%
  mutate(
    label = case_when(
      start_date == "2007-10-09" ~ "2008/09 recession (2007-10-09 to 2013-03-28)",
      start_date == "2020-02-19" ~ "2020 pandemic (2020-02-19 to 2020-08-18)",
      start_date == "2022-01-03" ~ "2022/2023 wokecession (2022-01-03 to 2024-01-19)",
      start_date == "2025-02-19" ~ "2025 Bidencession (2025-02-19 to present)"
    )
  ) %>%
  ungroup()

# Order the factors to ensure consistent color assignment
all_series$label <- factor(all_series$label, 
                           levels = c("2008/09 recession (2007-10-09 to 2013-03-28)", 
                                      "2020 pandemic (2020-02-19 to 2020-08-18)", 
                                      "2022/2023 wokecession (2022-01-03 to 2024-01-19)",
                                      "2025 Bidencession (2025-02-19 to present)"))




# Create plotly plot with no title - optimized for Quarto embedding
sp500_plot <- plot_ly(data = all_series, x = ~days_since_peak, y = ~pct_change, 
                      color = ~label, 
                      colors = custom_colors,
                      type = 'scatter', 
                      mode = 'lines',
                      line=list(width=1.4)
                      ) %>%
  layout(
    # No title or annotations
    margin = list(
      l = 80,
      r = 50,
      t = 50,  # Reduced top margin since there's no title
      b = 100,
      pad = 10
    ),
    xaxis = list(
      title = list(
        text = "Days from peak",
        standoff = 10
      ),
      rangeslider = list(
        visible = TRUE,
        thickness = 0.08
      ),
      range = c(0, 300),
      showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(
        text = "Indexed change (%)",
        standoff = 10
      ),
      showgrid = TRUE,
      range = c(-61, 5),
      zeroline = TRUE
    ),
    legend = list(
      orientation = "v",
      xanchor = "left",
      yanchor = "top",
      x = 0,
      y = 1.5
    ),
    autosize = TRUE,
    config = list(
      displayModeBar = FALSE,
      responsive = TRUE
    )
  )
