# Set Env. ----------------------------------------------------------------
library(tidyverse)
library(httr)
library(yaml)
library(jsonlite)
library(lubridate)
library(patchwork)

# FTP ---------------------------------------------------------------------
ftp <- 315

# Connect to API ----------------------------------------------------------
app <- oauth_app(appname = "strava", 
                 key = yaml.load_file("~/credentials.yaml")$client_id, 
                 secret = yaml.load_file("~/credentials.yaml")$secret)

endpoint <- oauth_endpoint(
  request = NULL,
  authorize = "https://www.strava.com/oauth/authorize",
  access = "https://www.strava.com/oauth/token"
)

token <- oauth2.0_token(endpoint = endpoint, 
                        app = app,
                        as_header = FALSE, 
                        scope = "activity:read_all")

# Fetch Activities --------------------------------------------------------
activity_list <- list()
i <- 1
done <- FALSE

while (!done) {
  
  req <- GET(
    url = "https://www.strava.com/api/v3/athlete/activities",
    config = token,
    query = list(per_page = 200, page = i))
  
  activity_list[[i]] <- fromJSON(content(req, as = "text"), flatten = TRUE)
  
  if (length(content(req)) < 200) {
    done <- TRUE
  } else {
    i <- i + 1
  }
  
}

activities <- rbind_pages(activity_list) %>% 
  filter(device_watts == TRUE) %>% 
  mutate(year = year(start_date),
         week = isoweek(start_date),
         year = ifelse(year == 2022 & week == 52, 2021, year), 
         session = paste(year, week, sep = ".")) %>% 
  group_by(session) %>% 
  arrange(id) %>%
  mutate(session_id = paste(session, 1:n(), sep = ".")) %>% 
  ungroup() %>%
  arrange(desc(id)) %>% 
  filter(str_detect(name, "Zwift|TT Tune-Up"),
         year %in% c(2021, 2022))

# Fetch Zones -------------------------------------------------------------
get_zones <- function(id){
  
  req <- GET(
    url = paste("https://www.strava.com/api/v3/activities/", id, "/streams?keys=watts&key_by_type=true", sep = ""),
    config = token,
    query = list(per_page = 200, page = 1))
  
  watts <- tibble(watts = fromJSON(content(req, as = "text"), flatten = TRUE) %>% 
                    pluck("watts", "data")) %>% 
    mutate(time = 1:n(),
           zone_6z = case_when(
             between(watts/ftp, 0, 0.55) ~ "Z1",
             between(watts/ftp, 0.55, 0.75) ~ "Z2",
             between(watts/ftp, 0.75, 0.90) ~ "Z3",
             between(watts/ftp, 0.90, 1.05) ~ "Z4",
             between(watts/ftp, 1.05, 1.12) ~ "Z5",
             between(watts/ftp, 1.12, 1.15) ~ "Z6",
             TRUE ~ "Z7"),
           zone_3z = case_when(
             zone_6z == "Z1" | zone_6z == "Z2" ~ "Z1",
             zone_6z == "Z3" | zone_6z == "Z4" ~ "Z2",
             TRUE ~ "Z3"))
  
  watts_summary <- tibble(
    id = id,
    time_s = max(watts$time),
    z1_time = (watts %>% filter(zone_3z == "Z1") %>% nrow()), 
    z2_time = (watts %>% filter(zone_3z == "Z2") %>% nrow()), 
    z3_time = (watts %>% filter(zone_3z == "Z3") %>% nrow()),
    z1_prop = z1_time / nrow(watts) * 100,
    z2_prop = z2_time / nrow(watts) * 100,
    z3_prop = z3_time / nrow(watts) * 100)
  
  cat(paste(id, sep = "\n"))
  
  return(watts_summary)
  
}

zone_data <- map_dfr(.x = activities$id,
                      .f = get_zones)

zone_data_summary <- activities %>% 
  left_join(zone_data, by = "id") %>% 
  group_by(session) %>% 
  summarise(z1_session_prop = sum(z1_time) / sum(time_s) * 100,
            z2_session_prop = sum(z2_time) / sum(time_s) * 100,
            z3_session_prop = sum(z3_time) / sum(time_s) * 100,
            total = sum(time_s),
            z1_session_total = sum(z1_time),
            z2_session_total = sum(z2_time),
            z3_session_total = sum(z3_time))

zone_graph <- zone_data_summary %>% 
  pivot_longer(cols = -c(session), names_to = "zone", values_to = "percent") %>% 
  filter(str_detect(zone, "_session_prop")) %>% 
  mutate(zone = str_remove(zone, "_session_prop"),
         percent = round(percent, 0)) %>% 
  ggplot(aes(x=session, y=percent, colour=zone, fill=zone, group = zone, label = percent)) + 
  geom_text(vjust = -0.75) + 
  geom_point(size = 3, shape = 21) + 
  geom_line(size = 1, alpha = 0.50) + 
  scale_fill_manual(values = c("blue", "orange", "red")) + 
  scale_colour_manual(values = c("darkblue", "darkorange", "darkred")) + 
  theme_bw() + 
  xlab("") + 
  ylab("Percent") + 
  ggtitle("Weekly Zone Distribution") + 
  ylim(0, 80) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

time_graph <- zone_data_summary %>% 
  pivot_longer(cols = -c(session), names_to = "zone", values_to = "percent") %>% 
  filter(zone == "total") %>%
  mutate(percent = round(percent / 3600, 2)) %>% 
  ggplot(aes(x=session, y=percent, label=percent)) + 
  geom_bar(stat = "identity", colour="black", fill="black", alpha = 0.75) + 
  geom_text(vjust = -0.35) + 
  xlab("") + 
  ylab("Hours / Week") + 
  ylim(0, 6) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

zone_graph / time_graph






