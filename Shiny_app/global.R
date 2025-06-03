# === ŁADOWANIE BIBLIOTEK ===
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(ggrepel)
library(plotly)
library(ggimage)
library(stringr)

# === WCZYTYWANIE DANYCH ===
nba_locations <- read.csv2("datasets/nba_team_locations.csv", sep = ",")
table1 <- read.csv2("datasets/play_off_box_scores_2010_2024.csv", sep = ",")
table2 <- read.csv2("datasets/play_off_totals_2010_2024.csv", sep = ",")
table3 <- read.csv2("datasets/regular_season_box_scores_2010_2024_part_1.csv", sep = ",")
table4 <- read.csv2("datasets/regular_season_box_scores_2010_2024_part_2.csv", sep = ",")
table5 <- read.csv2("datasets/regular_season_box_scores_2010_2024_part_3.csv", sep = ",")
table6 <- read.csv2("datasets/regular_season_totals_2010_2024.csv", sep = ",")

# === PRZETWARZANIE DANYCH REGULAR SEASON ===
table6 <- table6 %>%
  arrange(TEAM_NAME, GAME_DATE) %>%
  mutate(
    FG_PCT = as.numeric(FG_PCT),
    FG3_PCT = as.numeric(FG3_PCT),
    TOV = as.numeric(TOV),
    FT_PCT = as.numeric(FT_PCT),
    PLUS_MINUS = as.numeric(PLUS_MINUS),
    TEAM_NAME = case_when(
      TEAM_NAME == "Los Angeles Clippers" ~ "LA Clippers",
      TEAM_NAME == "New Orleans Hornets" ~ "New Orleans Pelicans",
      TEAM_NAME == "Charlotte Bobcats" ~ "Charlotte Hornets",
      TEAM_NAME == "New Jersey Nets" ~ "Brooklyn Nets",
      TRUE ~ TEAM_NAME
    ),
    Opponent = ifelse(grepl(" vs. ", MATCHUP),
                      sub(".* vs. ", "", MATCHUP),
                      sub(".* @ ", "", MATCHUP)),
    Opponent = case_when(
      Opponent == "NOH" ~ "NOP",
      Opponent == "NJN" ~ "BKN",
      TRUE ~ Opponent
    ),
    TEAM_ABBREVIATION = if_else(TEAM_ABBREVIATION == "NOH", "NOP", TEAM_ABBREVIATION),
    TEAM_ABBREVIATION = if_else(TEAM_ABBREVIATION == "NJN", "BKN", TEAM_ABBREVIATION)
  ) %>%
  mutate(Home_Away = case_when(
    str_detect(MATCHUP, "@") ~ "Away",
    str_detect(MATCHUP, "vs") ~ "Home",
    TRUE ~ NA_character_
  ))

# === MAPA LOKALIZACJI ===
nba_locations <- nba_locations %>%
  mutate(
    LON = as.numeric(LON),
    LAT = as.numeric(LAT)
  ) %>%
  group_by(LON, LAT) %>%
  mutate(n = n(), offset = row_number() - 1, LON_adj = LON + 0.5 * offset) %>%
  ungroup() %>%
  mutate(
    LOGO_PATH = paste0("logo/", TEAM_ABBREVIATION, ".png"),
    region = tolower(STATE)
  )

# Dane mapy USA
usa_states <- map_data("state")

# Połączenie lokalizacji z danymi
table6 <- table6 %>%
  left_join(nba_locations, by = c("TEAM_ABBREVIATION", "TEAM_NAME"))

# === TABELA 3: TOP 3 ZAWODNIKÓW (połączenie 3 części) ===
table3 <- table3 %>%
  bind_rows(
    table4 %>% mutate(TEAM_NAME = paste(teamCity, teamName)),
    table5 %>% mutate(TEAM_NAME = paste(teamCity, teamName))
  ) %>%
  mutate(TEAM_NAME = paste(teamCity, teamName)) %>%
  arrange(TEAM_NAME, game_date) %>%
  mutate(
    TEAM_NAME = case_when(
      TEAM_NAME == "Los Angeles Clippers" ~ "LA Clippers",
      TEAM_NAME == "New Orleans Hornets" ~ "New Orleans Pelicans",
      TEAM_NAME == "Charlotte Bobcats" ~ "Charlotte Hornets",
      TEAM_NAME == "New Jersey Nets" ~ "Brooklyn Nets",
      TRUE ~ TEAM_NAME
    ),
    Opponent = ifelse(grepl(" vs. ", matchup),
                      sub(".* vs. ", "", matchup),
                      sub(".* @ ", "", matchup)),
    Opponent = case_when(
      Opponent == "NOH" ~ "NOP",
      Opponent == "NJN" ~ "BKN",
      Opponent == "BOS" ~ "BCS",
      TRUE ~ Opponent
    )
  )

# === MAPA DRUŻYN I SKRÓTY ===
team_map <- table6 %>% select(TEAM_NAME, TEAM_ABBREVIATION) %>% distinct()

# === STATYSTYKI ===
team_wins <- table6 %>%
  group_by(TEAM_NAME) %>%
  summarise(Wins = sum(WL == "W"), .groups = "drop") %>%
  arrange(desc(Wins))

team_wins_home_away <- table6 %>%
  filter(WL == "W") %>%
  group_by(TEAM_NAME, Home_Away) %>%
  summarise(Wins = n(), .groups = "drop") %>%
  pivot_wider(names_from = Home_Away, values_from = Wins, values_fill = 0) %>%
  rename(Wins_Home = Home, Wins_Away = Away) %>%
  mutate(Wins_Total = Wins_Home + Wins_Away)

# === MACIERZ ZWYCIĘSTW ===
all_matches <- table6 %>% count(TEAM_NAME, TEAM_ABBREVIATION, Opponent, name = "total_games")
wins <- table6 %>% filter(WL == "W") %>% count(TEAM_ABBREVIATION, Opponent, name = "wins")
win_ratio <- left_join(all_matches, wins, by = c("TEAM_ABBREVIATION", "Opponent")) %>%
  mutate(wins = replace_na(wins, 0), win_ratio = wins / total_games)

win_ratio_named <- win_ratio %>%
  left_join(team_map, by = c("Opponent" = "TEAM_ABBREVIATION")) %>%
  rename(Opponent_full = TEAM_NAME.y, TEAM_NAME = TEAM_NAME.x)

wins_season_heatmap <- table6 %>%
  filter(WL == "W") %>%
  group_by(SEASON_YEAR, TEAM_NAME) %>%
  summarise(Wins = n(), .groups = "drop") %>%
  left_join(team_map, by = "TEAM_NAME")

# === KORELACJE ===
team_corr <- table6 %>%
  group_by(TEAM_NAME) %>%
  summarise(
    Wins = sum(WL == "W"),
    FGA = mean(as.numeric(FGA), na.rm = TRUE),
    FGM = mean(as.numeric(FGM), na.rm = TRUE),
    FG_PCT = mean(as.numeric(FG_PCT), na.rm = TRUE),
    FG3M = mean(as.numeric(FG3M), na.rm = TRUE),
    FG3A = mean(as.numeric(FG3A), na.rm = TRUE),
    FG3_PCT = mean(as.numeric(FG3_PCT), na.rm = TRUE),
    FTM = mean(as.numeric(FTM), na.rm = TRUE),
    FTA = mean(as.numeric(FTA), na.rm = TRUE),
    FT_PCT = mean(as.numeric(FT_PCT), na.rm = TRUE),
    DREB = mean(as.numeric(DREB), na.rm = TRUE),
    OREB = mean(OREB, na.rm = TRUE),
    REB = mean(REB, na.rm = TRUE),
    AST = mean(AST, na.rm = TRUE),
    STL = mean(STL, na.rm = TRUE),
    BLK = mean(BLK, na.rm = TRUE),
    TOV = mean(as.numeric(TOV), na.rm = TRUE),
    PF = mean(as.numeric(PF), na.rm = TRUE),
    PTS = mean(as.numeric(PTS), na.rm = TRUE),
    .groups = "drop"
  )

wins_corr <- cor(team_corr[-1], use = "complete.obs")[, "Wins"]
wins_df <- data.frame(Zmienna = names(wins_corr), Korelacja = wins_corr)

# === OPISY ZMIENNYCH ===
descriptions <- data.frame(
  Zmienna = c("FGA","FGM", "FG_PCT", "FG3M", "FG3A", "FG3_PCT", "FTM", "FTA", "FT_PCT",
              "OREB", "DREB", "REB", "AST", "STL", "BLK", "TOV", "PF", "PTS"),
  Opis = c("Próby rzutowe z gry","Trafione rzuty z gry", "Skuteczność rzutów z gry",
           "Trafione rzuty za 3 punkty", "Próby rzutów za 3 punkty",
           "Skuteczność rzutów za 3 punkty", "Trafione rzuty wolne",
           "Próby rzutów wolnych", "Skuteczność rzutów wolnych",
           "Zbiórki ofensywne", "Zbiórki defensywne", "Łączna liczba zbiórek",
           "Asysty", "Przechwyty", "Bloki", "Straty piłki", "Faule osobiste", "Punkty")
)
