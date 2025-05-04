options(tidyverse.quiet = TRUE)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyverse)

DEFAULT_BUFFER = 3
MIN_CODERS = 1
START_DATE = "4/8/2025"
CODES_FILE = "Video Codes - Ingest.csv"
AGGREG_FILE = "Long-form Content Analysis - Aggregate.csv"
YOUTUBE_API = Sys.getenv("YOUTUBE_API")
PURGE_VIDS = c("O_gRPhCtJws", # per Rosie
               "1usZrJo2-UI", "eCeMivdcMAQ", "selSonjBqR4", "twCIayzFNC8" # 4/8 vids not in aggr.
               )

most_recent_codes <- data.table::fread(CODES_FILE) %>% 
  filter(Type != "point" & 
           Timestamp > START_DATE & 
           !(`Video ID` %in% PURGE_VIDS) & 
           Duration >= 0) %>% 
  select(!c(Type, Timestamp, Notes)) %>% 
  mutate(End = Timecode + Duration, Coder = glue::trim((Coder)))

merge_overlapping_intervals <- function(group_df, buffer = DEFAULT_BUFFER) {
  # Sort by start time
  group_df <- group_df %>% arrange(Timecode)
  
  # Initialize result container
  merged <- list()
  current <- group_df[1, ]
  current$Coders <- list(unique(current$Coder))  # track coders
  if (nrow(group_df) != 1) {
  
    for (i in 2:nrow(group_df)) {
      row <- group_df[i, ]
      
      # Check for overlap or within buffer
      if (is.na(row$Timecode) || is.na(current$End + buffer)) {
        print(row$Timecode)
        print(current$End)
      }
      if (row$Timecode <= (current$End + buffer)) {
        # Merge
        current$End <- mean(current$End, row$End)
        current$Timecode <- mean(current$Timecode, row$Timecode)
        current$Coders[[1]] <- unique(c(current$Coders[[1]], row$Coder))
      } else {
        # Push current and reset
        merged[[length(merged) + 1]] <- current
        current <- row
        current$Coders <- list(unique(current$Coder))
      }
    }
    merged[[length(merged) + 1]] <- current
    result <- bind_rows(merged)
  } else {
    result <- group_df %>% 
      mutate(Coders = list(unique(Coder))) %>%
      select(-Coder)
  }
  
  # Add final fields
  result %>%
    mutate(Duration = End - Timecode,
           Num_Coders = lengths(Coders)) #%>%
    #select(`Video ID`, Code, Timecode, End, Duration, Num_Coders, Coders)
}

# Apply per video and code
consensus_df <- most_recent_codes %>%
  group_by(`Video ID`, Code) %>%
  group_modify(~merge_overlapping_intervals(.x)) %>%
  ungroup() %>% select(!Coder) %>% filter(Num_Coders >= MIN_CODERS)


infoByID <- function(video_id, api_key = YOUTUBE_API) {
  # Construct API request URL
  url <- paste0("https://www.googleapis.com/youtube/v3/videos?",
                "part=snippet,statistics,contentDetails&id=", video_id,
                "&key=", api_key)
  
  # Send GET request
  response <- GET(url)
  
  # Check if the response is valid
  if (status_code(response) != 200) {
    stop("Failed to fetch data. Check your API key and video ID.")
  }
  
  # Parse JSON response
  data <- fromJSON(content(response, as = "text"))
  
  # Extract relevant information
  if (length(data$items) == 0) {
    stop("No video found with the provided ID.")
  }
  
  video_info <- data$items
  
  duration_iso <- video_info$contentDetails$duration
  duration_seconds <- as.numeric(period_to_seconds(as.period(duration_iso)))
  
  published_date <- as.Date(video_info$snippet$publishedAt)
  
  # Extract required details
  result <- list(
    title = video_info$snippet$title,
    channel = video_info$snippet$channelTitle,
    post_date = published_date,
    duration = duration_seconds,
    description = video_info$snippet$description,
    view_count = as.numeric(video_info$statistics$viewCount),
    like_count = ifelse(is.null(video_info$statistics$likeCount), NA, as.numeric(video_info$statistics$likeCount)),
    comment_count = ifelse(is.null(video_info$statistics$commentCount), NA, as.numeric(video_info$statistics$commentCount))
  )
  
  return(result)
}

aggregate_file <- data.table::fread(AGGREG_FILE) %>%
  filter(!is.na(Link) & Link != "#REF!" & !`Delete?` & !`Traditional News Broadcast?`) %>% 
  mutate(`Video ID` = sub(".*(?:v=|youtu\\.be/|embed/)([a-zA-Z0-9_-]+).*", "\\1", `Link`)) %>%
  select(Subject, Endorsement, `Video ID`) %>% 
  group_by(`Video ID`) %>% slice(1) %>% ungroup()

video_dictionary <- consensus_df %>% group_by(`Video ID`) %>% 
  summarise(AdTime = sum(Duration[Code == "ad"], na.rm = TRUE)) %>% 
  left_join(aggregate_file, by = "Video ID") %>%
  mutate(info = purrr::map(`Video ID`, ~ infoByID(.x))) %>% unnest_wider(info) %>% 
  mutate(activeDuration = duration - AdTime)

needInfo <- video_dictionary %>% filter(is.na(Subject))

if (needInfo %>% nrow() > 0) {
  warning(paste0("Missing partisanship info for:\n", 
                 paste0(needInfo$`Video ID`, collapse=", "), 
                 "\nRun View(needInfo) for more details."))
}

codes <- c("code_campaign", "code_climate", "code_electionsdemocracy", 
           "code_race", "code_abortion", "code_familypersonal", "code_gender", 
           "code_immigration", "code_international", "code_lgbtq", 
           "code_patriotism", "code_religion", "code_sportsculture", 
           "code_healthcare")

pivot <- consensus_df %>% group_by(`Video ID`, Code) %>% 
  summarise(TotalDuration = sum(Duration, na.rm = TRUE)) %>% ungroup() %>% 
  pivot_wider(names_from = Code, values_from = TotalDuration, names_prefix = "code_") %>% 
  left_join(video_dictionary, by = "Video ID") %>% 
  mutate(engagementIndex = (like_count * 1 + comment_count * 3) / view_count * 100) %>% 
  mutate(relativeEngagement = engagementIndex / mean(engagementIndex) * 100) %>% 
  mutate(across(starts_with("code_"), ~ replace(., is.na(.), 0)))

pivot_prop <- pivot %>% mutate(across(starts_with("code_"), ~ . / activeDuration))
pivot_sum <- pivot %>% group_by(Endorsement) %>% 
  summarise(across(starts_with("code_"), ~ sum(.x, na.rm = TRUE)), 
            total_view_count = sum(view_count, na.rm = TRUE),
            total_like_count = sum(like_count, na.rm = TRUE),
            total_comment_count = sum(comment_count, na.rm = TRUE),
            total_activeDuration = sum(activeDuration, na.rm = TRUE),
            ave_view_count = mean(view_count, na.rm = TRUE),
            ave_like_count = mean(like_count, na.rm = TRUE),
            ave_comment_count = mean(comment_count, na.rm = TRUE),
            ave_activeDuration = mean(activeDuration, na.rm = TRUE)
            ) %>% 
  # mutate(across(starts_with("code_"), ~ . / activeDuration)) %>% 
  mutate(engagementIndex = (total_like_count * 1 + total_comment_count * 3) / total_view_count * 100)


group_T <- pivot[pivot$Endorsement == "T", ]
group_H <- pivot[pivot$Endorsement == "H", ]
pivot$Endorsement = recode(pivot$Endorsement,
                           "H" = "Harris",
                           "T" = "Trump")
run_one_sided_t_test <- function(var) {
  t.test(group_T[[var]], group_H[[var]], alternative = "greater", var.equal = FALSE)
}

test_engagementIndex <- run_one_sided_t_test("engagementIndex")
test_activeDuration <- run_one_sided_t_test("activeDuration")
test_code_familypersonal <- run_one_sided_t_test("code_familypersonal")


library(ggplot2)
library(showtext)
library(patchwork)
font_add_google("IBM Plex Sans", "ibm")
showtext_auto()
theme_cunty <- function(base_size = 12, base_family = "ibm") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major.y = element_line(color = "#cccccc", size = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", size = 0.3),
      axis.ticks = element_line(color = "black", size = 0.3),
      plot.title = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(size = 13, color = "gray30", margin = margin(b = 10)),
      plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold", size = 12)
    )
}

ggplot(pivot, aes(x = Endorsement, y = engagementIndex, fill = Endorsement)) +
  geom_violin(alpha = 0.4, trim = FALSE, linetype = 0) +
  geom_boxplot(width = 0.1, outlier.shape = NA, notch = FALSE) +
  geom_jitter(aes(color = Endorsement), width = 0.15, alpha = 0.3) +
  scale_color_manual(values = c("Trump" = "#E45756", "Harris" = "#4C78A8")) +  
  scale_fill_manual(values = c("Trump" = "#E45756", "Harris" = "#4C78A8")) +
  labs(title = "Engagement Index by Endorsement Type",
       y = "Engagement Index", x = "Endorsement") +
  theme_cunty() + 
  
  ggplot(pivot, 
         aes(x = Endorsement, y = activeDuration, fill = Endorsement)) +
  geom_violin(alpha = 0.4, trim = FALSE, linetype = 0) +
  geom_boxplot(width = 0.1, outlier.shape = NA, notch = FALSE) +
  geom_jitter(aes(color = Endorsement), width = 0.15, alpha = 0.3) +
  scale_color_manual(values = c("Trump" = "#E45756", "Harris" = "#4C78A8")) +  
  scale_fill_manual(values = c("Trump" = "#E45756", "Harris" = "#4C78A8")) +
  labs(title = "Active Duration by Endorsement Type",
       y = "Active Duration", x = "Endorsement") +
  theme_cunty()


format_time <- function(x) {
  sprintf("%02d:%02d", x %/% 60, x %% 60)
}
all_codes <- c("healthcare", "climate", "ad", "sportsculture", 
               "patriotism", "immigration", "familypersonal", 
               "electionsdemocracy", "econtax", "campaign")
code_labels <- c("Healthcare", "Climate", "Ad Read/Irrelevant", "Sports/Pop Culture",
                 "Patriotism", "Immigration", "Family/Personal",
                 "Election Integrity/Democracy", "Economy/Taxes", "Campaign/Finance")
sand <- ggplot(consensus_df %>% 
                 filter(`Video ID` == "TwED_Znc9XQ") %>% 
                 mutate(Code = factor(Code, levels = all_codes, labels = code_labels)), 
               aes(x = Timecode, xend = End, y = Code, yend = Code, color = Code)) +
  geom_segment(size = 6, alpha = 0.7) +
  scale_x_continuous(labels = format_time) +
  labs(#title = paste("Code Timeline for Sen. Bernie Sanders"),
       subtitle = "Sen. Bernie Sanders | This Past Weekend w/ Theo Von #524", 
       x = "Time (mm:ss)",
       y = "Code") +
  scale_y_discrete(drop = FALSE) +
  theme_cunty() +
  theme(legend.position = "none")
vanc <- ggplot(consensus_df %>% 
                 filter(`Video ID` == "vd8mmTDDqAs") %>% 
                 mutate(Code = factor(Code, levels = all_codes, labels = code_labels)), 
               aes(x = Timecode, xend = End, y = Code, yend = Code, color = Code)) +
  geom_segment(size = 6, alpha = 0.7) +
  scale_x_continuous(labels = format_time) +
  labs(#title = paste("Code Timeline for VP Vance"),
       subtitle = "Sen. JD Vance | This Past Weekend w/ Theo Von #540",
       x = "Time (mm:ss)",
       y = "Code") +
  scale_y_discrete(drop = FALSE) +
  theme_cunty() +
  theme(legend.position = "none")

(sand | vanc) + plot_annotation(
    title = 'Code Timelines for Sen. Sanders & VP Vance',
    caption = 'NMPI GISP'
  )

{ggplot(consensus_df %>% filter(`Video ID` == "8JDMKyTpXWc"), 
       aes(xmin = Timecode, xmax = End, ymin = 0, ymax = 1, fill = Code)) +
  geom_rect(alpha = 0.4, color = NA) +
  scale_x_continuous(labels = format_time) +
  labs(title = paste("Code Timeline (Collapsed) – Video 8JDMKyTpXWc"),
       x = "Time (mm:ss)", y = NULL) +
  theme_cunty() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "right")} %>% 
  plotly::ggplotly() %>% 
  plotly::config(displaylogo = FALSE) %>% 
  plotly::layout(font = list(family = "sans serif"))

ggplot(pivot %>%  group_by(post_date, Endorsement) %>% 
         summarise(video_count = n(), .groups = "drop") %>% 
         arrange(Endorsement, post_date) %>%
         group_by(Endorsement) %>%
         mutate(cumulative_videos = cumsum(video_count)) %>%
         ungroup(), 
       aes(x = post_date, y = cumulative_videos, color = Endorsement)) +
  geom_line(size = 1.2) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = c("Trump" = "#E45756", "Harris" = "#4C78A8")) +
  labs(title = "Cumulative Number of Videos Over Time by Endorsement",
       subtitle = "Videos sampled from x/x/x to x/x/x",
       x = "Post Date", y = "Number of Videos", caption = "NMPI GISP, 2025") +
  theme_cunty()

library(huxtable)
summary_table <- pivot %>%
  group_by(Endorsement) %>%
  summarise(
    n_videos = n(),
    avg_engagement = mean(engagementIndex, na.rm = TRUE),
    avg_duration = mean(duration, na.rm = TRUE),
    avg_activeDuration = mean(activeDuration, na.rm = TRUE),
    avg_view_count = mean(view_count, na.rm = TRUE),
    avg_like_count = mean(like_count, na.rm = TRUE),
    avg_comment_count = mean(comment_count, na.rm = TRUE)
  ) %>%
  as_hux() %>%
  set_caption("Summary Statistics by Endorsement") %>%
  set_bold(1, everywhere, TRUE) %>%
  set_align(everywhere, everywhere, "center")

code_sums <- pivot %>%
  group_by(Endorsement) %>%
  summarise(across(all_of(codes), ~ sum(.x, na.rm = TRUE))) %>%
  as_hux() %>%
  set_caption("Sum of Topical Code Counts by Endorsement") %>%
  set_bold(1, everywhere, TRUE) %>%
  set_number_format(2, everywhere, 0) %>%
  set_align(everywhere, everywhere, "center")

df_monthly <- pivot %>%
  mutate(month = format(post_date, "%Y-%m")) %>%
  count(month, Endorsement) %>%
  tidyr::pivot_wider(names_from = Endorsement, values_from = n, values_fill = 0) %>%
  arrange(month) %>%
  as_hux() %>%
  set_caption("Number of Videos per Month by Endorsement") %>%
  set_bold(1, everywhere, TRUE) %>%
  set_align(everywhere, everywhere, "center")

engagement_outliers <- pivot %>%
  arrange(desc(relativeEngagement)) %>%
  select(`Video ID`, Endorsement, post_date, title, relativeEngagement, engagementIndex) %>%
  slice_head(n = 5) %>%
  as_hux() %>%
  set_caption("Top 5 Videos by Relative Engagement") %>%
  set_bold(1, everywhere, TRUE) %>%
  set_wrap(TRUE) %>%
  set_align(everywhere, everywhere, "center")

library(broom)
# Run t-tests
ttests <- list(
  engagementIndex = t.test(engagementIndex ~ Endorsement, data = pivot),
  activeDuration = t.test(activeDuration ~ Endorsement, data = pivot),
  code_familypersonal = t.test(code_familypersonal ~ Endorsement, data = pivot)
)

# Tidy results
ttest_df <- bind_rows(lapply(ttests, tidy), .id = "Variable") %>%
  select(Variable, estimate1, estimate2, statistic, p.value) %>%
  rename(
    Mean_T = estimate1,
    Mean_H = estimate2,
    T_statistic = statistic,
    P_value = p.value
  ) %>%
  as_hux() %>%
  set_caption("T-Test Results Comparing T vs H Endorsements") %>%
  set_bold(1, everywhere, TRUE) %>%
  set_number_format(2, everywhere, 3) %>%
  set_align(everywhere, everywhere, "center")

num_vars <- pivot %>%
  select(engagementIndex, activeDuration, duration, view_count, like_count, comment_count, starts_with("code_"))

# Compute correlation matrix
cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")

# Convert to huxtable


# Convert counts to binary (presence/absence)
binary_codes <- pivot %>%
  mutate(across(starts_with("code_"), ~ as.integer(. > 0))) %>%
  select(Endorsement, starts_with("code_"))

# Run chi-square tests for each code
chi_results <- lapply(names(binary_codes)[-1], function(code) {
  tbl <- table(binary_codes[[code]], binary_codes$Endorsement)
  test <- chisq.test(tbl)
  tibble(Code = code, ChiSq = test$statistic, df = test$parameter, P_value = test$p.value)
})

# Combine and format
chi_df <- bind_rows(chi_results) %>%
  arrange(P_value) %>%
  as_hux() %>%
  set_caption("Chi-Square Tests of Code Presence by Endorsement") %>%
  set_bold(1, everywhere, TRUE) %>%
  set_number_format(2, everywhere, 3) %>%
  set_align(everywhere, everywhere, "center")

# write.csv(needInfo, "Need Info.csv", row.names = FALSE, na = "")


