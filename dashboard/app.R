#---------------------------------------------------------------------------------------------------------------------------
#
# WD - Projekt 2
#
#---------------------------------------------------------------------------------------------------------------------------

### Wczytanie pakietow

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
source("dane_do_wykresów.R")

#---------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------

### Dashboard

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Football App"),
    dashboardSidebar(
        sidebarUserPanel(Sys.info()[["effective_user"]],
                         subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
        ),
        sidebarMenu(
            id = "tabs",
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("O aplikacji", icon = icon("info-circle"), tabName = "about", badgeLabel = "new", badgeColor = "green"),
            menuItem("Wykresy", icon = icon("bar-chart-o"), startExpanded = TRUE,
                     menuSubItem("Elastyczne sondaże", tabName = "subitem1"),
                     menuSubItem("Jak po metaamfetaminie", tabName = "subitem2"),
                     menuSubItem("Niech się kręci", tabName = "subitem3"),
                     menuSubItem("Kobiety giganty", tabName = "subitem4"),
                     menuSubItem("Nie wiem, nie znam się", tabName = "subitem5"),
                     menuSubItem("Kinowe hity", tabName = "subitem6"),
                     menuSubItem("Sub-item 1", tabName = "subitem7"),
                     menuSubItem("Sub-item 2", tabName = "subitem8"),
                     menuSubItem("Sub-item 2", tabName = "subitem9")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("subitem1",
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img1"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot1"), width = 6))
            ),
            tabItem("subitem2",
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img2"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot2"), width = 6))
            ),
            tabItem("subitem3",
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img3"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotlyOutput("plot3"), width = 6, height = 500))
            ),
            tabItem("subitem4",
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img4"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot4"), width = 6, height = 500))
            ),
            tabItem("subitem5",
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img5"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot5"), width = 6, height = 500))
            ),
            tabItem("subitem6",
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img6"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot6"), width = 6, height = 500))
            ),
            tabItem("about",
                    "Aplikacja shiny dashboard, pozwalająca testować wpływ rozmaitych błędów wizualizacyjnych na 
                    percepcję danych przedstawionych na wykresie. Aplikcja zawiera osiem wykresów, które przedstawiają 
                    częste błędy wizualizacyjne, a także pola, w których użytkownik może zapisywać wartości odczytane 
                    z wykresu. Ponadto użytkownik ma możliwość przejścia w tryb sprawdzania, gdzie porówna wprowadzone 
                    przez siebie wartości z rzeczywistymi oraz zobaczy porównanie poprawnego wykresu z niepoprawnym."
            )
        )
    )
)

server <- function(input, output) {
    
    output[["img1"]] <- renderImage({
      
      filename <- normalizePath(file.path('./images', paste('elastyczne_sondaze', '.png', sep='')))
      list(src = filename, height = 400)
    
      }, deleteFile = FALSE)
    
    
    output[["plot1"]] <- renderPlot(
        plot_1()
    )
    
    output[["img2"]] <- renderImage({
      
      filename <- normalizePath(file.path('./images', paste('jak_po_metaamfetaminie', '.png', sep='')))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot2"]] <- renderPlot(
        plot_2()
    )
    
    output[["img3"]] <- renderImage({
      
      filename <- normalizePath(file.path('./images', paste('niech_sie_kreci', '.png', sep='')))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot3"]] <- renderPlotly(
        plot_3()
    )
    
    output[["img4"]] <- renderImage({
      
      filename <- normalizePath(file.path('./images', paste('kobiety', '.png', sep='')))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot4"]] <- renderPlot(
      plot_4()
    )
    
    output[["img5"]] <- renderImage({
      
      filename <- normalizePath(file.path('./images', paste('niewiem', '.png', sep='')))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot5"]] <- renderPlot(
      plot_5()
    )
    
    output[["img6"]] <- renderImage({
      
      filename <- normalizePath(file.path('./images', paste('kino', '.png', sep='')))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot6"]] <- renderPlot(
      plot_6()
    )
    
}

shinyApp(ui, server)
