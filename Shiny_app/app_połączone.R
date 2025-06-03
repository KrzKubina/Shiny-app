source("global.R")
source("ui.R")
source("serwer.R")

# === URUCHOMIENIE APLIKACJI ===
shinyApp(ui = ui, server = server)

#rsconnect::deployApp()