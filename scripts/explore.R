# Load packages.
library(XML)
library(cowplot)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

# List files.
dive_files <- paste("data/", list.files("data", pattern = glob2rx("*.xml"),  recursive=TRUE), sep = "")

# Load data.
dive_xml_list <- lapply(dive_files, xmlParse)

# Parse into a list of df.
dive_df_list <- lapply(dive_xml_list, xmlToList)

# Function pull out relevant information.
pull_fun <- function(x){
tibble(
    mode  = x[["Mode"]],
    date  = x[["StartTime"]],
    depth = as.numeric(lapply(x[["DiveSamples"]], function(y){y$Depth})),
    time  = as.numeric(lapply(x[["DiveSamples"]], function(y){y$Time})),
    max_depth   = as.numeric(x[["MaxDepth"]]),
    depth_minus = depth*-1,
    max_depth_minus = max_depth*-1,
    duration  = as.numeric(x[["Duration"]]),
    temp  = as.numeric(lapply(x[["DiveSamples"]], function(y)y$Temperature))
  ) %>% 
    mutate(date = str_replace_all(date, "T", " "),
           date_lub = ymd_hms(date),
           date_min = round_date(date_lub, "minute"),
           date_day = round_date(date_lub, "day"))
    # separate(col = date, into = c("date", "tod"), sep = "T", remove = TRUE) %>% 
    # mutate(date_lub = ymd(date),
    #        tod_lub  = hms(tod)) 
}

# Run function through list.
dive_info_list <- lapply(dive_df_list, pull_fun)

# Bind together.
dive_info_df <- bind_rows(dive_info_list, .id = "dive_id")

# Remove SCUBA dives, and likely safety dives.
dive_info_clean_df <- dive_info_df %>%
  filter(mode == 3, max_depth > 8) %>% 
  mutate(dive_id = as.numeric(dive_id))

# Plot dives profile.
dive_info_clean_df %>% 
  ggplot(data = .) +
  geom_line(mapping = aes(x = time, y = depth_minus, group = dive_id, colour = depth_minus),
            size = 1, alpha = 1) +
  scale_colour_viridis_c() +
  theme_bw() +
  geom_vline(xintercept = 60, linetype = "dotted") +
  annotate(geom = "text", x = 59, y = -20, label = "1 minute", angle = 90) +
  labs(y = "depth (metres)", x = "time (seconds)") +
  theme(legend.position = "none") 

# Create chronological id.
chrono_df <- dive_info_clean_df %>% 
  distinct(date_lub) %>% 
  mutate(chrono_id = 1:nrow(.)) 

# Join with data.
dive_info_clean_df <- left_join(dive_info_clean_df, chrono_df)

# Chronology plot.
dive_info_clean_df %>% 
  distinct(chrono_id, max_depth_minus, .keep_all = TRUE) %>%
  ggplot(data = .) +
  geom_segment(mapping = aes(x = chrono_id, xend = chrono_id,
                             y = 0, yend = max_depth_minus)) +
  geom_point  (mapping = aes(x = chrono_id, y = max_depth_minus))

dive_info_clean_df %>% 
  ggplot(data = .) +
  geom_line(mapping = aes(x = chrono_id, y = depth_minus, group = chrono_id,
                          colour = depth_minus), size = 2) +
  scale_colour_viridis_c() +
  theme_bw() +
  theme(legend.position = "none")


# Distribution handling.
dist_gg <- dive_info_clean_df %>% 
  select(dive_id, duration, max_depth, temp) %>% 
  distinct() %>% 
  rename(`Duration (mins)` = duration,
         `Max. depth (metres)`  = max_depth,
         `Temperature (C)` = temp) %>% 
  pivot_longer(cols = -dive_id, names_to = "stat") %>% 
  ggplot(data = .) +
  geom_density(mapping = aes(x = value), fill = "grey20", colour = "grey20") +
  facet_wrap(~stat, scales = "free") +
  theme_bw() +
  labs(x = NULL, y = NULL)

# Arrange.
ggdraw(dives_gg) +
  draw_plot(dist_gg, height = 0.25, width = 0.5, x = 0.48, y = 0.08)


  
