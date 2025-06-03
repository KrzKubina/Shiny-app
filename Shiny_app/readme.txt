# Aplikacja Shiny – Analiza Zespołów NBA

Projekt stworzony w języku R przy użyciu pakietu **Shiny**. Aplikacja umożliwia interaktywną analizę danych dotyczących zespołów NBA oraz statystyk zawodników z wybranych sezonów.

## Funkcje aplikacji

- **Interaktywna mapa drużyn NBA** – kliknięcie na logo drużyny wyświetla szczegółowe informacje
- **Wykresy zwycięstw** – ogólne, domowe i wyjazdowe
- **TOP 3 zawodników drużyny pod względem średniej punktów** – analiza ich średnich statystyk sezon po sezonie
- **Analiza korelacji** – badanie zależności między wybranymi statystykami zawodników a liczbą zwycięstw drużyny
- **Dynamiczne wykresy** – wbudowane filtry, porównania między drużynami i sezony

##  Wykorzystane technologie

- **R** – język programowania
- **Shiny** – tworzenie aplikacji webowych
- **dplyr**, **ggplot2**, **plotly** – przetwarzanie danych i wizualizacja
- **DataTable**, **shinyWidgets**, **readr**, **tidyr**, **corrr**

##  Struktura katalogu


```
nba-shiny-app/
├── ui.R                 # Interfejs użytkownika aplikacji
├── server.R             # Logika serwera Shiny
├── global.R             # Wczytywanie danych i ustawienia globalne
├── datasets             # Folder z danymi
└── README.md            # Opis projektu
```

## Dane znajdują się w folderze `datasets/` i zawierają:

- `nba_team_locations.csv` – współrzędne geograficzne drużyn NBA (do mapy interaktywnej)
- `regular_season_box_scores_*.csv` – dane z meczów sezonu regularnego (2010–2024)
- `regular_season_totals_*.csv` – łączne statystyki zawodników w sezonach regularnych
- `play_off_box_scores_*.csv` – szczegółowe statystyki meczowe z fazy playoff
- `play_off_totals_*.csv` – sumaryczne wyniki zawodników w playoffach
- `2019-20_pbp.csv` – dane play-by-play z sezonu 2019/20
- `logo/` – folder z logotypami drużyn NBA używanymi na mapie


##  Uwagi

- Dane pochodzą z publicznych źródeł i zostały przetworzone wyłącznie do celów edukacyjno-prezentacyjnych.
- Aplikacja może być z łatwością rozbudowana o nowe metryki, sezony lub opcje analizy.


