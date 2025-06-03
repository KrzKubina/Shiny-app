library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .plotly .cursor-crosshair {
        cursor: pointer !important;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollToTarget', function(message) {
        document.getElementById('scrollTarget').scrollIntoView({ behavior: 'smooth' });
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollToTop', function(message) {
        window.scrollTo({ top: 0, behavior: 'smooth' });
      });
    ")),
    tags$script(HTML("
  Shiny.addCustomMessageHandler('scrollToTargetTab2', function(message) {
    var el = document.getElementById('scrollTargetTab2');
    if (el) {
      el.scrollIntoView({ behavior: 'smooth' });
    }
  });
"))
  ),
  
  titlePanel("NBA – Liczby, które wygrywają mecz"),
  
  tabsetPanel(
    
    ## === ZAKŁADKA 1 ===
    tabPanel("Zespoły i ich wyniki",
             fluidRow(
               column(10, plotlyOutput("map_plot", height = "500px"))
             ),
             fluidRow(
               column(12, div(style = "margin-top: 5px; font-size: 16px; font-weight: bold; text-align: center;",
                              textOutput("map_info")))
             ),
             div(style = "height: 100px;"),
             tags$div(id = "scrollTarget"),
             fluidRow(
               column(8, plotlyOutput("plot1", height = "300px")),
               column(4, div(style = "margin-top: 20px;", uiOutput("team_info")))
             ),
             fluidRow(
               column(6, plotOutput("plot2", height = "350px", click = "click_plot2"),
                      tableOutput("table_info2")),
               column(6, plotOutput("plot3", height = "350px", click = "click_plot3"),
                      tableOutput("table_info3"))
             ),
             div(style = "height: 200px;")
    ),
    
    ## === ZAKŁADKA 2 ===
    
      tabPanel("Wpływ statystyk drużynowych",
             
             # Wybór zmiennej i widoku
             fluidRow(
               column(12,
                      selectInput("selected_var", "Wybierz zmienną do analizy:",
                                  choices = setdiff(names(team_corr)[-1], "Wins"),
                                  selected = "FG3_PCT"),
                      radioButtons("plot_or_table", "Wybierz widok:",
                                   choices = c("Wykres", "Tabela"),
                                   selected = "Wykres",
                                   inline = TRUE),
                      div(
                        style = "background-color: #d4edda; border-left: 5px solid #28a745; padding: 10px; margin-bottom: 15px; color: #155724;",
                        uiOutput("variable_description")
                      )
               )
             ),
             
             # Boxplot + korelacje (scroll target)
             tags$div(id = "scrollTargetTab2",
                      fluidRow(
                        column(6, uiOutput("plot_or_table_ui")),
                        column(6, plotlyOutput("plot5", height = "300px"))
                      )
             ),
             
             tags$hr(),
             
             fluidRow(
               column(12,
                      helpText("Kliknij punkt na wykresie, aby podświetlić wybraną drużynę.")
               )
             ),
             
             fluidRow(
               column(6,
                      plotlyOutput("scatter_plot", height = "300px"),
                      uiOutput("selected_label")
               ),
               column(6,
                      plotlyOutput("highlight_bar", height = "300px")
               )
             )
    ),
  
  # Pływający przycisk (strzałka w górę)
  tags$div(
    style = "
      position: fixed;
      bottom: 30px;
      right: 30px;
      z-index: 9999;
    ",
    actionButton("scroll_up", label = icon("arrow-up"), 
                 style = "background-color: #007bff; color: white; border-radius: 50%; height: 40px; width: 40px; padding: 0; font-size: 20px; box-shadow: 0px 2px 5px rgba(0,0,0,0.3);")
  ),
    
    ## === ZAKŁADKA 3 ===
             tabPanel("Wpływ statystyk indywidualnych",
                      fluidRow(
               column(3,
                      selectInput("stat", "Wybierz zmienną:",
                                  choices = c("points", "assists", "steals", "blocks", "turnovers", 
                                              "reboundsTotal", "freeThrowsMade", "threePointersMade", "sum_match")),
                      selectInput("team", "Wybierz drużynę:",
                                  choices = unique(table3$TEAM_NAME))
               )
             ),
             
             # WYKRESY
             div(id = "plot_anchor",
                 fluidRow(
                   column(6, plotlyOutput("top3plot", height = "350px")),
                   column(6, plotOutput("season_wins_plot", height = "350px"))
                 )
             ),
             
             # TRZECI wykres + przyciski
             fluidRow(
               column(12,
                      radioButtons("team_scope", "Zakres analizy korelacji:",
                                   choices = c("Wybrana drużyna" = "selected", "Wszystkie drużyny" = "all"),
                                   inline = TRUE),
                      plotlyOutput("team_corr_plot")
               )
             )),
             
             
             tags$script(HTML("
    $(document).ready(function() {
      $('#stat, #team').on('change', function() {
        $('html, body').animate({
          scrollTop: $('#plot_anchor').offset().top
        }, 600);
      });

      $('#scroll_up').on('click', function() {
        $('html, body').animate({ scrollTop: 0 }, 600);
      });
    });
  ")),
  
  ## === STRZAŁKA DO GÓRY ===
  tags$div(
    style = "
      position: fixed;
      bottom: 30px;
      right: 30px;
      z-index: 9999;
    ",
    actionButton("scroll_up", label = icon("arrow-up"), 
                 style = "background-color: #007bff; color: white; border-radius: 50%; height: 40px; width: 40px; padding: 0; font-size: 20px; box-shadow: 0px 2px 5px rgba(0,0,0,0.3);")
  )
))