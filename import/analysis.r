library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(zoo)

dates <-
  read_csv(
    "dates.csv",
    col_types = cols(
      Date = col_date(format = "%Y-%m-%d"),
      Punch0 = col_time(format = "%H:%M"),
      Punch1 = col_time(format = "%H:%M"),
      Punch2 = col_time(format = "%H:%M"),
      Punch3 = col_time(format = "%H:%M"),
      Punch4 = col_time(format = "%H:%M"),
      Punch5 = col_time(format = "%H:%M"),
      Punch6 = col_time(format = "%H:%M"),
      Punch7 = col_time(format = "%H:%M")
    )
  )

df <- dates %>%
  filter(!is.na(Punch0)) %>%
  filter(Punch0 > as.difftime("05:00", "%H:%M")) %>%
  filter(format(Date, "%u") < 6) %>%
  group_by(Week = format(Date, "%Y-%U")) %>%
  summarise(
    Min = as.POSIXct(min(as.numeric(Punch0)), origin = "1970-01-01", tz = "GMT"),
    Max = as.POSIXct(max(as.numeric(Punch0)), origin = "1970-01-01", tz = "GMT"),
    Mean = as.POSIXct(mean(as.numeric(Punch0)), origin = "1970-01-01", tz = "GMT"),
    Date = Date[1],
    SD = as.POSIXct(sd(as.numeric(Punch0)), origin = "1970-01-01", tz = "GMT")
  ) %>%
  mutate(Late = as.numeric(Mean) > as.numeric(as.difftime("10:00", "%H:%M"), units = "secs")) %>%
  mutate(RollMean = rollmean(Mean, 4, fill = NA)) %>%
  mutate(Upper = as.POSIXct(as.numeric(RollMean) + as.numeric(SD), origin = "1970-01-01", tz = "GMT")) %>%
  mutate(Lower = as.POSIXct(as.numeric(RollMean) - as.numeric(SD), origin = "1970-01-01", tz = "GMT"))

ggplot(df, aes(x = Date)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "grey80") +
  geom_errorbar(aes(ymin = Min, ymax = Max)) +
  geom_point(aes(x = Date, y = Mean),
             color = ifelse(df$Late, 'red', 'blue')) +
  scale_y_datetime(labels = date_format("%H:%M"),
                   breaks = date_breaks("1 hour")) +
  scale_x_date(labels = date_format("%Y-%m"),
               breaks = date_breaks("6 months"),
               minor_breaks = date_breaks("1 month")) +
  geom_hline(yintercept = as.numeric(as.difftime("10:00", "%H:%M"), units = "secs"), color="green") +
  geom_smooth(aes(x = Date, y = Mean), method = "loess", span = 0.1, se = FALSE) +
  geom_line(aes(x = Date, y = SD)) +
  geom_smooth(aes(x = Date, y = SD), method = "loess", span = 0.1, se = FALSE)


