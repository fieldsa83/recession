# Install and load required packages
# TTR is listed explicitly: quantmod depends on it, and a broken TTR install
# won't be repaired by reinstalling quantmod alone.
pkgs <- c("TTR", "quantmod", "dplyr", "lubridate", "plotly")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing, repos = "https://cloud.r-project.org/")
library(quantmod)
library(dplyr)
library(lubridate)
library(plotly)

# Custom color palette for the historical episodes (the current one is drawn black dashed)
custom_colors <- c("#4CAF50", "#FF6B6B", "#5D9CEC", "#9C27B0", "#fdc500")

# Download S&P 500 data using tomorrow's date to ensure we get today's data
tomorrow_date <- Sys.Date() + 1
sp500 <- getSymbols("^GSPC", from = "2007-01-01", to = tomorrow_date, auto.assign = FALSE)
index(sp500) <- as.Date(index(sp500))

# Keep only the last entry for any duplicate dates
sp500 <- sp500[!duplicated(index(sp500), fromLast = TRUE), ]

# Convert to data frame
sp500_df <- data.frame(Date = index(sp500), coredata(sp500))
sp500_df <- sp500_df %>% select(Date, GSPC.Adjusted)
sp500_df$Date <- as.Date(sp500_df$Date)

#sp500_df <- sp500_df %>%
#  add_row(Date = as.Date("2026-02-03"), GSPC.Adjusted = 6735.12)

# Function to create indexed series from a particular date until it turns positive.
# Optional cap_date: hard stop on this date (inclusive), applied before the recovery check.
create_indexed_series <- function(data, start_date, cap_date = NULL) {
  # Filter data from start date onwards (and up to cap_date if given)
  filtered_data <- data %>% filter(Date >= start_date)
  if (!is.null(cap_date)) {
    filtered_data <- filtered_data %>% filter(Date <= cap_date)
  }

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

# Create all series
series1 <- create_indexed_series(sp500_df, as.Date("2007-10-09"))
series2 <- create_indexed_series(sp500_df, as.Date("2020-02-19"))
series3 <- create_indexed_series(sp500_df, as.Date("2022-01-03"))
series4 <- create_indexed_series(sp500_df, as.Date("2025-02-19"))
series5 <- create_indexed_series(sp500_df, as.Date("2026-01-27"), cap_date = as.Date("2026-04-15"))
series6 <- create_indexed_series(sp500_df, as.Date("2026-10-06"))

# Historical episodes (coloured) and the current episode (black dashed) kept separate
hist_series <- rbind(series1, series2, series3, series4, series5)

hist_labels <- c("2008/09 recession (2007-10-09 to 2013-03-28)",
                 "2020 pandemic (2020-02-19 to 2020-08-18)",
                 "2022/2023 Russian war and interest rate hikes (2022-01-03 to 2024-01-19)",
                 "2025 tariff shocks (2025-02-19 to 2025-06-27)",
                 "2026 Iran war (2026-01-27 to 2026-04-15)")
current_label <- "2026 Hyperscaler descent (2026-10-06 to present)"

hist_series <- hist_series %>%
  mutate(
    label = case_when(
      start_date == "2007-10-09" ~ hist_labels[1],
      start_date == "2020-02-19" ~ hist_labels[2],
      start_date == "2022-01-03" ~ hist_labels[3],
      start_date == "2025-02-19" ~ hist_labels[4],
      start_date == "2026-01-27" ~ hist_labels[5]
    )
  )

# Order the factors to ensure consistent color assignment
hist_series$label <- factor(hist_series$label, levels = hist_labels)


# Create plotly plot with no title - optimized for Quarto embedding
sp500_plot <- plot_ly(data = hist_series, x = ~days_since_peak, y = ~pct_change,
        color = ~label,
        colors = custom_colors,
        type = 'scatter',
        mode = 'lines',
        line = list(width = 1.4)
) %>%
  add_trace(
    data = series6,
    x = ~days_since_peak, y = ~pct_change,
    name = current_label,
    type = 'scatter',
    mode = 'lines',
    color = I("black"),
    line = list(color = "black", dash = "dash", width = 2.2),
    inherit = FALSE
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
        text = "SP500 - Indexed change (%)",
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
