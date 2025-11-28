# Load required packages
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# List of teams with their IDs in ESPNcricinfo
teams <- list(
  "india-6", "afghanistan-40", "australia-2", "england-1",
  "ireland-29", "new-zealand-5", "pakistan-7", "south-africa-3",
  "sri-lanka-8", "west-indies-4", "zimbabwe-9", "bangladesh-25"
)

# We'll store the final data here
data <- data.frame(
  Name = character(),
  Country = character(),
  code = numeric(),
  link = character(),
  BattingAverage = character(),
  stringsAsFactors = FALSE
)

for (team in teams) {
  url  <- paste0("https://www.espncricinfo.com/records/team/averages-batting/", team, "/test-matches-1")
  page <- read_html(url)
  
  # Grab all rows from the "Batting Averages" table
  rows <- page %>%
    html_element("table.ds-table") %>%  # The main table has class "ds-table"
    html_elements("tbody tr")          # Each player is in a <tr> row
  
  for (r in rows) {
    # The first <td> has the anchor with player’s name + link
    anchor <- r %>%
      html_element("td a")
    
    # If no anchor found for some reason, skip
    if (is.na(anchor)) next
    
    player_link <- anchor %>% html_attr("href")
    player_name <- anchor %>% html_text(trim = TRUE)
    
    # Example link: /cricketers/ashton-agar-505120
    # We'll parse out the numeric code from the last chunk (e.g. "505120")
    splitted_link <- strsplit(player_link, split = "/")[[1]]
    last_part     <- splitted_link[length(splitted_link)]
    splitted_name <- strsplit(last_part, split = "-")[[1]]
    player_code   <- as.numeric(splitted_name[length(splitted_name)])
    
    # Get all cells in this row
    cells <- r %>% 
      html_elements("td") %>% 
      html_text(trim = TRUE)
    # Typically: [Player, Span, Mat, Inns, NO, Runs, HS, Ave, SR, 100, 50, 0]
    # So "Ave" is cells[8], if length is at least 8
    if (length(cells) < 8) next
    batting_avg <- cells[8]
    
    # Our "team" string is like "india-6" -> country is "india"
    country_str <- strsplit(team, split = "-")[[1]][1]
    
    # Build a StatsGuru link for that player’s Test matches
    player_stats_link <- paste0(
      "https://stats.espncricinfo.com/ci/engine/player/",
      player_code,
      ".html?class=1;template=results;type=allround;view=match"
    )
    
    # Create a row and append to 'data'
    new_row <- data.frame(
      Name = player_name,
      Country = country_str,
      code = player_code,
      link = player_stats_link,
      BattingAverage = batting_avg,
      stringsAsFactors = FALSE
    )
    data <- bind_rows(data, new_row)
  }
}

# View the final data frame
head(data)


data$link <- paste0('https://stats.espncricinfo.com/ci/engine/player/',data$code,'.html?class=1;template=results;type=allround;view=match')





###############################################################################
data$run_against <- 0
data$total_out <- 0
data$others_avg <- 0

for (i in 1:nrow(data)){
  name <- as.character(data$Name[i])
  name <- tools::toTitleCase(name)
  link <- data$link[i]
  run = 0
  out = 0
  
  # Read the HTML content
  page <- read_html(link)
  
  # Extract match links
  match_links <- page %>%
    html_nodes("td a") %>%
    html_attr("href") %>%
    unique() %>%
    .[grepl("/engine/match/", .)]
  
  # Convert to full URLs
  base_url <- "https://www.espncricinfo.com"
  full_match_links <- paste0(base_url, match_links)
  for (url in full_match_links){
    page <- read_html(url)
    match <- page %>%
      html_table()
    for(j in 1:length(match)){
      if (colnames(match[[j]])[1] == 'Batting'){
        target <- match[[j]]
        colnames(target)[2] <- 'NOT'
        target <- target[,c(1,2,3)]
        target <- target %>%
          filter(Batting != "")
        if (nrow(target) > 7){
          target <- target[c(1:7),]
        }
        
        
        target <- target %>%
          mutate(Batting = str_remove_all(Batting, "\\s*\\(c\\)")) %>%
          filter(Batting != name)
        out = out + nrow(target) - sum(grepl("^not out", target$NOT, ignore.case = TRUE))
        run = run + sum(suppressWarnings(as.numeric(unlist(target$R))), na.rm = TRUE)
      }
    }
  }
  data$run_against[i] <- run
  data$total_out[i] <- out
  data$others_avg[i] <-  run/out
  print(paste(i, data$Name[i], data$run_against[i], data$total_out[i], round(data$others_avg[i], 2)))
  
}

data <- data[,-4]


# A container (list) to hold data for each team
all_data <- list()

for (team in teams) {
  
  url <- paste0("https://www.espncricinfo.com/records/team/averages-batting/", 
                team, "/test-matches-1")
  
  # Read the page HTML
  page <- read_html(url)
  
  # Extract all rows in the batting averages table
  rows <- page %>%
    html_element("table.ds-table") %>%   # main batting averages table
    html_elements("tbody tr")            # each player's row in <tbody>
  
  # If the table doesn't exist (e.g., no data for that team), skip
  if (length(rows) == 0) {
    message("No table found for team: ", team)
    next
  }
  
  # The columns typically appear in this order:
  # 1) Player | 2) Span | 3) Mat | 4) Inns | 5) NO | 6) Runs | 7) HS | 8) Ave | ... 
  team_data <- map_df(rows, function(r) {
    cols <- r %>% html_elements("td")
    tibble(
      Player = cols[1] %>% html_text2(),  # Player name column
      Inns   = cols[4] %>% html_text2(),  # Inns column
      NO     = cols[5] %>% html_text2()   # NO column
    )
  })
  
  # Store the extracted data in our list, indexed by team name
  all_data[[team]] <- team_data
}

# Combine all data into a single data frame
df_all_teams <- bind_rows(all_data, .id = "team")

# View the final data
head(df_all_teams)

project_1_ds3 <- cbind(
  project_1_ds3,
  Inns    = df_all_teams$Inns,
  Not_out = df_all_teams$NO
)


project_1_ds3 <- project_1_ds3 %>%
  mutate(
    Inns     = as.numeric(as.character(Inns)),
    Not_out = as.numeric(as.character(Not_out))
  ) %>%
  mutate(
    Inns     = replace_na(Inns, 0),
    Not_out = replace_na(Not_out, 0)
  )

write.csv(project_1_ds3, "project_1_ds3.csv", row.names = FALSE)
