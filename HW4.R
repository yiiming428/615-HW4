
##a
library(data.table)
library(lubridate)
library(dplyr)

file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"

years <- 1985:2023

buoy_data_list <- list()

for (year in years) {
  path <- paste0(file_root, year, tail)
  header <- scan(path, what = 'character', nlines = 1)
  skip_lines <- ifelse(year >= 2007, 2, 1)
  buoy <- fread(path, header = FALSE, skip = skip_lines)
  num_cols <- ncol(buoy)
  if (length(header) > num_cols) {
    header <- header[1:num_cols]
  } else if (length(header) < num_cols) {
    header <- c(header, paste0("V", (length(header) + 1):num_cols))
  }
  
  colnames(buoy) <- header
  
  if ("YY" %in% colnames(buoy) & "MM" %in% colnames(buoy) & "DD" %in% colnames(buoy) & "hh" %in% colnames(buoy) & "mm" %in% colnames(buoy)) {
    buoy$Date <- ymd_hms(paste(buoy$YY, buoy$MM, buoy$DD, buoy$hh, buoy$mm))
  }
  
  buoy_data_list[[as.character(year)]] <- buoy
}

buoy_data_list <- rbindlist(buoy_data_list, fill = TRUE)

# Merge the YY, YYYY, and #YY columns into one (if any of them exist)
buoy_data_list <- buoy_data_list %>%
  mutate(Year = coalesce(as.numeric(YY), as.numeric(YYYY), as.numeric(`#YY`))) %>%
  select(-YY, -YYYY, -`#YY`) %>%
  select(Year, everything())

# Adjust year format for years between 85 and 98
buoy_data_list <- buoy_data_list %>%
  mutate(Year = ifelse(Year < 100, ifelse(Year >= 85, 1900 + Year, 2000 + Year), Year))

# Merge WD and WDIR columns into one
buoy_data_list <- buoy_data_list %>%
  mutate(Wind_Direction = coalesce(WD, WDIR)) %>%
  select(-WD, -WDIR)

# Merge BAR and PRES columns into one
buoy_data_list <- buoy_data_list %>%
  mutate(Pressure = coalesce(BAR, PRES)) %>%
  select(-BAR, -PRES)

# Create a proper date column if possible
if (all(c("Year", "MM", "DD", "hh") %in% colnames(buoy_data_list))) {
  buoy_data_list[, date := make_datetime(Year, MM, DD, hh)]
} else if (all(c("Year", "MM", "DD", "hh") %in% colnames(buoy_data_list))) {
  buoy_data_list[, date := make_datetime(Year, MM, DD, hh)]
}

buoy_data_list <- buoy_data_list %>% select(date, everything())

str(buoy_data_list)
