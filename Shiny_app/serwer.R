# === SERVER ===
server <- function(input, output, session) {
  
  # Miejsce na ścieżkę do folderu z logo
  shiny::addResourcePath("logo", "datasets/logo")
  
  
  
  # === Reaktywna lista wybranych drużyn ===
  selected_teams <- reactiveVal(NULL)
  
  
  # === Mapa interaktywna z plotly ===
  output$map_plot <- renderPlotly({
    p <- ggplot() +
      geom_polygon(data = usa_states, aes(x = long, y = lat, group = group),
                   fill = "lightgray", color = "white") +
      geom_point(data = nba_locations,
                 aes(x = LON_adj, y = LAT,
                     color = Konferencja,
                     text = TEAM_NAME,
                     customdata = TEAM_ABBREVIATION),
                 size = 5) +
      geom_text(data = nba_locations,
                aes(x = LON_adj, y = LAT, label = TEAM_ABBREVIATION),
                color = "black", size = 3, vjust = -1) +
      scale_color_manual(values = c("Wschód" = "steelblue", "Zachód" = "orange")) +
      coord_fixed(1.3) +
      theme_minimal() +
      labs(title = "Kliknij drużynę na mapie", x = NULL, y = NULL)
    
    ggplotly(p, source = "map", tooltip = "text") %>%
      event_register("plotly_click") %>%
      layout(
        dragmode = "zoom",
        hovermode = "closest",
        xaxis = list(autorange = TRUE),
        yaxis = list(autorange = TRUE)
      )
  })
  
  # Obsługa kliknięć – zapisywanie wybranych drużyn
  observeEvent(event_data("plotly_click", source = "map"), {
    click <- event_data("plotly_click", source = "map")
    abbr <- click$customdata
    
    if (!is.null(abbr)) {
      selected_teams(abbr)
      
      # Scroll po kliknięciu
      session$sendCustomMessage("scrollToTarget", list())
    }
  })
  
  # Informacja o wybranych drużynach
  # output$map_info <- renderText({
  #   teams <- nba_locations %>% filter(TEAM_ABBREVIATION %in% selected_teams())
  #   paste("Wybrane drużyny:", paste(teams$TEAM_NAME, collapse = ", "))
  # })
  
  # Wykres słupkowy zwycięstw
  output$plot1 <- renderPlotly({
    selected_team_name <- first(team_map$TEAM_NAME[team_map$TEAM_ABBREVIATION == selected_teams()])
    
    plot_data <- team_wins %>%
      mutate(
        highlight = TEAM_NAME == selected_team_name,
        tooltip_text = paste0("Drużyna: ", TEAM_NAME, "<br>Zwycięstwa: ", Wins)
      )
    
    p <- ggplot(plot_data, aes(
      x = reorder(TEAM_NAME, Wins),
      y = Wins,
      fill = highlight,
      text = tooltip_text
    )) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "steelblue"), guide = "none") +  # <- już ukrywa w ggplot
      labs(title = paste("Liczba zwycięstw -", selected_team_name),
           x = NULL, y = "Zwycięstwa") +
      theme_minimal()+
      theme(axis.text.y = element_text(size = 6)) 
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE) %>%  # <- to ukrywa legendę w plotly
      config(displayModeBar = FALSE)
  })
  
  
  # Macierz zwycięstw
  output$plot2 <- renderPlot({
    x_levels <- sort(unique(win_ratio_named$Opponent))
    y_levels <- sort(unique(win_ratio_named$TEAM_ABBREVIATION))
    abbr <- selected_teams()
    if (is.null(abbr)) return(NULL)
    
    ggplot(win_ratio_named, aes(x = factor(Opponent, levels = x_levels),
                                y = factor(TEAM_ABBREVIATION, levels = y_levels),
                                fill = win_ratio)) +
      geom_tile(color = "white") +
      geom_rect(data = data.frame(TEAM_ABBREVIATION = abbr), 
                aes(xmin = -Inf, xmax = Inf, ymin = match(TEAM_ABBREVIATION, y_levels) - 0.5, ymax = match(TEAM_ABBREVIATION, y_levels) + 0.5),
                inherit.aes = FALSE, color = "red", fill = NA, linewidth = 1.2) +
      geom_rect(data = data.frame(Opponent = abbr), 
                aes(ymin = -Inf, ymax = Inf, xmin = match(Opponent, x_levels) - 0.5, xmax = match(Opponent, x_levels) + 0.5),
                inherit.aes = FALSE, color = "red", fill = NA, linewidth = 1.2) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(x = "Pokonana drużyna", y = "Zwycięska drużyna", fill = "Proporcja wygranych",
           title = "Macierz zwycięstw między drużynami") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Heatmapa zwycięstw w sezonach
  output$plot3 <- renderPlot({
    y_levels <- sort(unique(wins_season_heatmap$TEAM_ABBREVIATION))
    abbr <- selected_teams()
    if (is.null(abbr)) return(NULL)
    y_index <- match(abbr, y_levels)
    
    ggplot(wins_season_heatmap, aes(x = SEASON_YEAR,
                                    y = factor(TEAM_ABBREVIATION, levels = y_levels),
                                    fill = Wins)) +
      geom_tile(color = "white") +
      geom_rect(data = data.frame(index = y_index), 
                aes(xmin = 0.5, xmax = length(unique(wins_season_heatmap$SEASON_YEAR)) + 0.5,
                    ymin = index - 0.5, ymax = index + 0.5),
                inherit.aes = FALSE, color = "red", fill = NA, linewidth = 1.2) +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(x = NULL, y = NULL, fill = "Zwycięstwa", title = "Zwycięstwa drużyn w danym sezonie") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  # === DODATKOWE INFORMACJE PO KLIKNIĘCIU W WYKRESY ===
  
  
  
  # 2. Info do wykresu 2 (macierz zwycięstw)
  output$table_info2 <- renderTable({
    req(input$click_plot2)
    
    x_click <- round(input$click_plot2$x)
    y_click <- round(input$click_plot2$y)
    
    x_levels <- sort(unique(win_ratio_named$Opponent))
    y_levels <- sort(unique(win_ratio_named$TEAM_ABBREVIATION))
    
    if (x_click < 1 || x_click > length(x_levels) ||
        y_click < 1 || y_click > length(y_levels)) {
      return(data.frame(Informacja = "Kliknij w pole macierzy, aby zobaczyć szczegóły."))
    }
    
    opponent_abbr <- x_levels[x_click]
    winner_abbr <- y_levels[y_click]
    
    opponent_name <- team_map$TEAM_NAME[team_map$TEAM_ABBREVIATION == opponent_abbr]
    winner_name <- team_map$TEAM_NAME[team_map$TEAM_ABBREVIATION == winner_abbr]
    
    selected_ratio <- win_ratio_named %>%
      filter(TEAM_ABBREVIATION == winner_abbr, Opponent == opponent_abbr)
    
    if (nrow(selected_ratio) == 0) {
      return(data.frame(Informacja = "Brak danych o meczu."))
    }
    
    data.frame(
      "Zwycięska drużyna" = winner_name,
      "Pokonana drużyna" = opponent_name,
      "Wygrane mecze" = selected_ratio$wins,
      "Rozegrane mecze" = selected_ratio$total_games,
      "Proporcja wygranych" = round(selected_ratio$win_ratio, 2)
    )
  })
  
  # 3. Info do wykresu 3 (heatmap sezonowa)
  output$table_info3 <- renderTable({
    req(input$click_plot3)
    
    x_click <- round(input$click_plot3$x)
    y_click <- round(input$click_plot3$y)
    
    season_years <- sort(unique(wins_season_heatmap$SEASON_YEAR))
    y_levels <- sort(unique(wins_season_heatmap$TEAM_ABBREVIATION))
    
    if (x_click < 1 || x_click > length(season_years) ||
        y_click < 1 || y_click > length(y_levels)) {
      return(data.frame(Informacja = "Kliknij w pole, aby zobaczyć sezonowe zwycięstwa."))
    }
    
    team_abbr <- y_levels[y_click]
    season <- season_years[x_click]
    team_name <- team_map$TEAM_NAME[team_map$TEAM_ABBREVIATION == team_abbr]
    
    win_row <- wins_season_heatmap %>%
      filter(SEASON_YEAR == season, TEAM_ABBREVIATION == team_abbr)
    
    if (nrow(win_row) == 0) {
      return(data.frame(Informacja = "Brak zwycięstw w tym sezonie."))
    }
    
    data.frame(
      "Drużyna" = team_name,
      "Sezon" = season,
      "Liczba zwycięstw" = win_row$Wins
    )
  })
  
  output$team_info <- renderUI({
    abbr <- selected_teams()
    if (is.null(abbr)) return(NULL)
    
    team_data <- nba_locations %>% filter(TEAM_ABBREVIATION == abbr)
    if (nrow(team_data) == 0) return(NULL)
    
    LOGO_PATH <- paste0("logo/", abbr, ".png")
    
    tagList(
      tags$div(style = "text-align: center; margin-top: 10px;",
               tags$img(src = LOGO_PATH, height = "100px"),
               tags$p(style = "font-size: 18px; font-weight: bold;", team_data$TEAM_NAME),
               tags$p(paste("Stan:", team_data$STATE)),
               tags$p(paste("Konferencja:", team_data$Konferencja))
      )
    )
  })
  
  observeEvent(input$scroll_up, {
    session$sendCustomMessage("scrollToTop", list())
  })
  # === ZAKŁADKA 2 ===
  
  # UI do przełączania między wykresem a tabelą
  output$plot_or_table_ui <- renderUI({
    if (input$plot_or_table == "Wykres") {
      plotlyOutput("plot4", height = "300px")
    } else {
      tableOutput("summary_table")
    }
  })
  
  # Boxplot: rozkład wybranej zmiennej w podziale na 3 grupy drużyn
  output$plot4 <- renderPlotly({
    var <- input$selected_var
    
    top_bottom_teams <- team_corr %>%
      mutate(Grupa = case_when(
        Wins >= quantile(Wins, 0.9) ~ "Najlepsze 10",
        Wins <= quantile(Wins, 0.1) ~ "Najsłabsze 10",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Grupa)) %>%
      select(all_of(var), Grupa) %>%
      rename(Wartość = all_of(var))
    
    wszystkie_druzyny <- team_corr %>%
      select(all_of(var)) %>%
      mutate(Grupa = "Wszystkie drużyny") %>%
      rename(Wartość = all_of(var))
    
    dane_box <- bind_rows(top_bottom_teams, wszystkie_druzyny) %>%
      mutate(Grupa = factor(Grupa, levels = c("Najsłabsze 10", "Wszystkie drużyny", "Najlepsze 10")))
    
    # Test Wilcoxona: tylko Najlepsze vs Najsłabsze
    test_data <- dane_box %>% filter(Grupa != "Wszystkie drużyny")
    test_result <- wilcox.test(Wartość ~ Grupa, data = test_data)
    p_val <- formatC(test_result$p.value, format = "f", digits = 2)
    test_label <- paste0("Test Wilcoxona: p = ", p_val)
    
    p <- ggplot(dane_box, aes(x = Grupa, y = Wartość, fill = Grupa, text = paste0("Grupa: ", Grupa, "<br>Wartość: ", round(Wartość, 3)))) +
      geom_boxplot() +
      labs(title = paste("Rozkład zmiennej:", var),
           subtitle = test_label,
           x = "Grupa drużyn", y = var) +
      scale_fill_manual(values = c("Najsłabsze 10" = "#d95f02",
                                   "Wszystkie drużyny" = "#7570b3",
                                   "Najlepsze 10" = "#1b9e77")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Wykres korelacji zmiennych ze zwycięstwami z podświetleniem wybranej zmiennej
  output$plot5 <- renderPlotly({
    selected <- input$selected_var
    
    df <- wins_df %>%
      filter(Zmienna != "Wins") %>%
      mutate(KorelacjaAbs = abs(Korelacja),
             Highlight = ifelse(Zmienna == selected, "Tak", "Nie"),
             Tooltip = paste0("Zmienna: ", Zmienna, "<br>Korelacja: ", round(Korelacja, 2)))
    
    p <- ggplot(df, aes(x = reorder(Zmienna, KorelacjaAbs),
                        y = Korelacja,
                        fill = Highlight,
                        text = Tooltip)) +
      geom_col() +
      coord_flip() +
      labs(title = "Korelacja zmiennych ze zwycięstwami",
           x = "Zmienna", y = "Współczynnik korelacji") +
      scale_fill_manual(values = c("Tak" = "orange", "Nie" = "steelblue"), guide = "none") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  # Reaktywna zmienna na klikniętą drużynę
  selected_team <- reactiveVal(NULL)
  
  # Scatter plot: średnia wartość wybranej zmiennej vs liczba zwycięstw
  output$scatter_plot <- renderPlotly({
    var <- input$selected_var
    
    team_stats <- table6 %>%
      group_by(TEAM_NAME) %>%
      summarise(
        Wins = sum(WL == "W"),
        Avg_Stat = mean(.data[[var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tooltip = paste0("Drużyna: ", TEAM_NAME, "<br>Średnia: ", round(Avg_Stat, 2), "<br>Zwycięstwa: ", Wins))
    
    # Dopasuj model liniowy
    lm_model <- lm(Wins ~ Avg_Stat, data = team_stats)
    r_squared <- summary(lm_model)$r.squared
    reg_line <- data.frame(
      Avg_Stat = seq(min(team_stats$Avg_Stat), max(team_stats$Avg_Stat), length.out = 100)
    )
    reg_line$Wins <- predict(lm_model, newdata = reg_line)
    reg_line$Tooltip <- paste0("Regresja<br>R² = ", round(r_squared, 3))
    
    # Rysowanie
    p <- ggplot() +
      geom_point(data = team_stats, aes(x = Avg_Stat, y = Wins, text = Tooltip), color = "red", size = 3) +
      geom_line(data = reg_line, aes(x = Avg_Stat, y = Wins, text = Tooltip), color = "blue", size = 1) +
      labs(title = paste("Wpływ", var, "na liczbę zwycięstw"),
           x = paste("Średnia wartość", var, "na mecz"),
           y = "Liczba zwycięstw") +
      theme_minimal() 
    
    ggplotly(p, tooltip = "text", source = "scatter_click") %>%
      config(displayModeBar = FALSE)
  })
  
  observeEvent(event_data("plotly_click", source = "scatter_click"), {
    click <- event_data("plotly_click", source = "scatter_click")
    var <- input$selected_var
    
    team_stats <- table6 %>%
      group_by(TEAM_NAME) %>%
      summarise(
        Wins = sum(WL == "W"),
        Avg_Stat = mean(.data[[var]], na.rm = TRUE),
        .groups = "drop"
      )
    
    # Szukamy najbliższego punktu kliknięcia
    if (!is.null(click)) {
      x_click <- click$x
      y_click <- click$y
      
      clicked_team <- team_stats %>%
        filter(abs(Avg_Stat - x_click) < 0.5 & abs(Wins - y_click) < 20) %>%
        pull(TEAM_NAME)
      
      if (length(clicked_team) > 0) {
        selected_team(clicked_team[1])
      }
    }
  })
  
  
  # Wykres słupkowy ze statystyką wybranej drużyny
  output$highlight_bar <- renderPlotly({
    req(selected_team())
    var <- input$selected_var
    
    team_stats <- table6 %>%
      group_by(TEAM_ABBREVIATION) %>%
      summarise(Stat = mean(.data[[var]], na.rm = TRUE),
                .groups = "drop") %>%
      mutate(Group = ifelse(TEAM_ABBREVIATION == team_map$TEAM_ABBREVIATION[team_map$TEAM_NAME == selected_team()], "Wybrana", "Inne"),
             Tooltip = paste0("Drużyna: ", TEAM_ABBREVIATION, "<br>Średnia: ", round(Stat, 2)))
    
    p <- ggplot(team_stats, aes(x = reorder(TEAM_ABBREVIATION, Stat), y = Stat, fill = Group, text = Tooltip)) +
      geom_col(fill = "gray80") +
      geom_col(data = filter(team_stats, Group == "Wybrana"), fill = "gold", color = "black") +
      coord_flip() +
      labs(title = paste("Porównanie drużyn dla zmiennej:", var),
           x = "Drużyna", y = var) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 6))
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Tekst z nazwą klikniętej drużyny pod wykresem
  output$selected_label <- renderUI({
    req(selected_team())
    HTML(paste0("<b>Drużyna:</b> ", selected_team()))
  })
  
  
  # === DODATKOWE INFORMACJE PO KLIKNIĘCIU ===
  
  output$summary_table <- renderTable({
    var <- input$selected_var
    
    
    dane <- team_corr %>%
      mutate(Grupa = case_when(
        Wins >= quantile(Wins, 0.9) ~ "Najlepsze 10",
        Wins <= quantile(Wins, 0.1) ~ "Najsłabsze 10",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Grupa)) %>%
      select(all_of(var), Grupa) %>%
      rename(Wartość = all_of(var)) %>%
      bind_rows(
        team_corr %>% select(all_of(var)) %>%
          mutate(Grupa = "Wszystkie drużyny") %>%
          rename(Wartość = all_of(var))
      ) %>%
      group_by(Grupa) %>%
      summarise(
        Min = round(min(Wartość, na.rm = TRUE), 2),
        Q1 = round(quantile(Wartość, 0.25, na.rm = TRUE), 2),
        Mediana = round(median(Wartość, na.rm = TRUE), 2),
        Q3 = round(quantile(Wartość, 0.75, na.rm = TRUE), 2),
        Max = round(max(Wartość, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      relocate(Grupa)  # Grupa jako pierwsza kolumna
    
    dane
  })
  
  output$variable_description <- renderUI({
    var <- input$selected_var
    opis <- descriptions %>% filter(Zmienna == var) %>% pull(Opis)
    if (length(opis) == 0) return(NULL)
    HTML(paste0("<b>Opis zmiennej:</b> ", opis))
  })
  
  observeEvent(input$selected_var, {
    session$sendCustomMessage("scrollToTargetTab2", list())
  }, ignoreInit = TRUE)
  
  observeEvent(input$plot_or_table, {
    session$sendCustomMessage("scrollToTargetTab2", list())
  }, ignoreInit = TRUE)
  
  # === ZAKŁADKA 3 ===
  output$top3plot <- renderPlotly({
    req(input$stat, input$team)
    
    stat <- input$stat
    team <- input$team
    
    top3players <- table3 %>%
      filter(comment == "", TEAM_NAME == team) %>%
      group_by(season_year, TEAM_NAME, personName, personId) %>%
      summarise(
        total_points = sum(points, na.rm = TRUE),
        sum_match = n(),
        across(
          .cols = c(points, assists, steals, blocks, turnovers, reboundsTotal,
                    freeThrowsMade, threePointersMade),
          .fns = ~ mean(.x, na.rm = TRUE),
          .names = "avg_{.col}"
        ),
        .groups = "drop"
      ) %>%
      mutate(średnia_punktów = total_points / sum_match) %>%
      group_by(season_year, TEAM_NAME) %>%
      slice_max(order_by = średnia_punktów, n = 3, with_ties = FALSE) %>%
      ungroup()
    
    top3_ranked <- top3players %>%
      group_by(season_year, TEAM_NAME) %>%
      arrange(desc(średnia_punktów)) %>%
      mutate(Pozycja = paste0("Gracz ", row_number())) %>%
      ungroup() %>%
      mutate(
        wartość = if (stat == "sum_match") sum_match else .data[[paste0("avg_", stat)]],
        tooltip_text = paste0("Zawodnik: ", personName,
                              "<br>Sezon: ", season_year,
                              "<br>Wartość: ", round(wartość, 2))
      )
    
    p <- ggplot(top3_ranked, aes(x = season_year, y = wartość,
                                 group = Pozycja, color = Pozycja,
                                 text = tooltip_text)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = paste("Średnia (", stat, ") –", team, "(Top 3 zawodników według punktów)"),
        x = "Sezon",
        y = paste( stat),
        color = "Pozycja"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
  output$season_wins_plot <- renderPlot({
    req(input$team)
    
    team <- input$team
    
    season_wins <- table6 %>%
      filter(TEAM_NAME == team, WL == "W") %>%
      group_by(SEASON_YEAR) %>%
      summarise(Wins = n(), .groups = "drop")
    
    ggplot(season_wins, aes(x = SEASON_YEAR, y = Wins, group = 1)) +
      geom_line(color = "darkgreen", size = 1.2) +
      geom_point(color = "darkgreen", size = 2) +
      labs(
        title = paste("Liczba zwycięstw w sezonie –", team),
        x = "Sezon",
        y = "Liczba zwycięstw"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none"
      )
  })
  
  output$team_corr_plot <- renderPlotly({
    req(input$team_scope)
    
    zmienne <- c("avg_points", "sum_match", "assists", "steals", "blocks", 
                 "turnovers", "reboundsTotal", "freeThrowsMade", "threePointersMade")
    
    # Przygotuj dane
    filtered_table3 <- table3 %>%
      filter(comment == "") %>%
      group_by(season_year, TEAM_NAME, personId, personName) %>%
      summarise(
        sum_match = n(),
        total_points = sum(points, na.rm = TRUE),
        across(all_of(zmienne[-1:-2]), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(avg_points = total_points / sum_match) %>%
      group_by(season_year, TEAM_NAME) %>%
      slice_max(order_by = avg_points, n = 3, with_ties = FALSE) %>%
      summarise(
        avg_points = mean(avg_points),
        sum_match = mean(sum_match),
        across(all_of(zmienne[-1:-2]), mean, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Zwycięstwa
    team_wins_season <- table6 %>%
      filter(WL == "W") %>%
      group_by(SEASON_YEAR, TEAM_NAME) %>%
      summarise(Wins = n(), .groups = "drop")
    
    # Łączenie
    merged <- left_join(filtered_table3, team_wins_season, by = c("season_year" = "SEASON_YEAR", "TEAM_NAME"))
    
    # Jeśli wybrano tylko jedną drużynę
    if (input$team_scope == "selected") {
      merged <- merged %>% filter(TEAM_NAME == input$team)
    }
    
    # Korelacje
    corr <- cor(merged %>% select(all_of(zmienne), Wins), use = "complete.obs")
    corr_df <- data.frame(
      Zmienna = zmienne,
      Korelacja = corr[zmienne, "Wins"]
    ) %>%
      mutate(
        KorelacjaAbs = abs(Korelacja),
        Tooltip = paste0("Zmienna: ", Zmienna, "<br>Korelacja: ", round(Korelacja, 2))
      )
    
    title_text <- ifelse(input$team_scope == "selected",
                         paste("Korelacja zmiennych –", input$team),
                         "Korelacja zmiennych – wszystkie drużyny")
    
    p <- ggplot(corr_df, aes(x = reorder(Zmienna, KorelacjaAbs), y = Korelacja,
                             text = Tooltip)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = title_text,
           x = "", y = "Współczynnik korelacji") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = FALSE)
  })
  
}