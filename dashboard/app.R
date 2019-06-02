#---------------------------------------------------------------------------------------------------------------------------
#
# WD - Projekt 2
#
#---------------------------------------------------------------------------------------------------------------------------

### Wczytanie pakietow

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
source("data_for_charts.R")

#---------------------------------------------------------------------------------------------------------------------------

### Dashboard

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Skutki niewlasciwej wizualizacji danych", titleWidth = 380),
    dashboardSidebar(
        sidebarUserPanel(Sys.info()[["effective_user"]],
                         subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
        ),
        sidebarMenu(
            id = "tabs",
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("O aplikacji", icon = icon("info-circle"), tabName = "about", badgeLabel = "new", badgeColor = "green"),
            menuItem("Wykresy", icon = icon("bar-chart-o"), startExpanded = TRUE,
                     menuSubItem("Elastyczne sondaze", tabName = "subitem1"),
                     menuSubItem("Jak po metaamfetaminie", tabName = "subitem2"),
                     menuSubItem("Niech sie kreci", tabName = "subitem3"),
                     menuSubItem("Kobiety giganty", tabName = "subitem4"),
                     menuSubItem("Nie wiem, nie znam sie", tabName = "subitem5"),
                     menuSubItem("Kinowe hity", tabName = "subitem6"),
                     menuSubItem("Opinie Polakow", tabName = "subitem7"),
                     menuSubItem("Wysokie emerytury", tabName = "subitem8"),
                     menuSubItem("Sluby", tabName = "subitem9")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("subitem1",
                    fluidRow(box(title = "Pytanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, 
                                 textInput("text_in1", label = h3("Ile glosow zdobyla partia Nowoczesna?"), value = ""),
                                 actionButton("runRF1", "Sprawdz"),
                                 tags$p(textOutput("text_out1"), style = "font-size: 200%;"),
                                 width = 6, height = 283),
                             box(title = "Podsumowanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                 htmlOutput("summary1"),
                                 width = 6)
                    ),
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img1"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot1"), width = 6))
            ),
            tabItem("subitem2",
                    fluidRow(box(title = "Pytanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, 
                                 textInput("text_in2", label = h3("Ile kosztuje jedna tabletka metaamfetaminy w Wietnamie?"), value = ""),
                                 actionButton("runRF2", "Sprawdz"),
                                 tags$p(textOutput("text_out2"), style = "font-size: 200%;"),
                                 width = 6, height = 243),
                             box(title = "Podsumowanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                 htmlOutput("summary2"),
                                 width = 6)
                    ),
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img2"), width = 5),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot2"), width = 7))
            ),
            tabItem("subitem3",
                    fluidRow(box(title = "Pytanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, 
                                 textInput("text_in3", label = h3("Ile bylo skazan ludzi bialych (wynik podaj w %)?"), value = ""),
                                 actionButton("runRF3", "Sprawdz"),
                                 tags$p(textOutput("text_out3"), style = "font-size: 200%;"),
                                 width = 6, height = 243),
                             box(title = "Podsumowanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                 htmlOutput("summary3"),
                                 width = 6)
                    ),
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img3"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotlyOutput("plot3"), width = 6))
            ),
            tabItem("subitem4",
                    fluidRow(box(title = "Pytanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, 
                                 textInput("text_in4", label = h3("?"), value = ""),
                                 actionButton("runRF4", "Sprawdz"),
                                 tags$p(textOutput("text_out4"), style = "font-size: 200%;"),
                                 width = 6),
                             box(title = "Podsumowanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                 htmlOutput("summary4"),
                                 width = 6)
                    ),
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img4"), width = 4),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot4"), width = 8))
            ),
            tabItem("subitem5",
                    fluidRow(box(title = "Pytanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, 
                                 textInput("text_in5", label = h3("?"), value = ""),
                                 actionButton("runRF5", "Sprawdz"),
                                 tags$p(textOutput("text_out5"), style = "font-size: 200%;"),
                                 width = 6),
                             box(title = "Podsumowanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                 htmlOutput("summary5"),
                                 width = 6)
                    ),
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img5"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot5"), width = 6))
            ),
            tabItem("subitem6",
                    fluidRow(box(title = "Pytanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, 
                                 textInput("text_in6", label = h3("?"), value = ""),
                                 actionButton("runRF6", "Sprawdz"),
                                 tags$p(textOutput("text_out6"), style = "font-size: 200%;"),
                                 width = 6),
                             box(title = "Podsumowanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                 htmlOutput("summary6"),
                                 width = 6)
                    ),
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img6"), width = 5),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot6"), width = 7))
            ),
            tabItem("subitem7",
                    fluidRow(box(title = "Pytanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, 
                                 textInput("text_in7", label = h3("?"), value = ""),
                                 actionButton("runRF7", "Sprawdz"),
                                 tags$p(textOutput("text_out7"), style = "font-size: 200%;"),
                                 width = 6),
                             box(title = "Podsumowanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                 htmlOutput("summary7"),
                                 width = 6)
                    ),
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img7"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot7"), width = 6))
            ),
            tabItem("subitem8",
                    fluidRow(box(title = "Pytanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, 
                                 textInput("text_in8", label = h3("?"), value = ""),
                                 actionButton("runRF8", "Sprawdz"),
                                 tags$p(textOutput("text_out8"), style = "font-size: 200%;"),
                                 width = 6),
                             box(title = "Podsumowanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                 htmlOutput("summary8"),
                                 width = 6)
                    ),
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img8"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot8"), width = 6))
            ),
            tabItem("subitem9",
                    fluidRow(box(title = "Pytanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, 
                                 textInput("text_in9", label = h3("Ile slubow zawarto w 2009"), value = ""),
                                 actionButton("runRF9", "Sprawdz"),
                                 tags$p(textOutput("text_out9"), style = "font-size: 200%;"),
                                 width = 6),
                             box(title = "Podsumowanie", status = "primary", 
                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                 htmlOutput("summary9"),
                                 width = 6)
                    ),
                    fluidRow(box(title = "Niepoprawny wykres", status = "danger", 
                                 solidHeader = TRUE, collapsible = TRUE, imageOutput("img9"), width = 6),
                             box(title = "Poprawny wykres", status = "success", solidHeader = TRUE, 
                                 collapsible = TRUE, collapsed = TRUE, plotOutput("plot9"), width = 6))
            ),
            tabItem("about",
                    "Aplikacja shiny dashboard, pozwalajaca testowac wplyw rozmaitych bledow wizualizacyjnych na 
                    percepcje danych przedstawionych na wykresie. Aplikcja zawiera dziewiec wykresow, ktore przedstawiaja
                    czeste bledy wizualizacyjne, a takze pola, w ktorych uzytkownik moze zapisywac wartosci odczytane 
                    z wykresu. Ponadto uzytkownik ma mozliwosc przejscia w tryb sprawdzania, gdzie porowna wprowadzone 
                    przez siebie wartosci z rzeczywistymi oraz zobaczy porownanie poprawnego wykresu z niepoprawnym."
            )
        )
    )
)

server <- function(input, output) {
    
    text_to_display1 <- reactiveVal("Wcisnij przycisk 'Sprawdz', aby poznac poprawna odpowiedz")
    
    observeEvent(input$runRF1, {
      ifelse(input$text_in1 == "", text_to_display1("Najpierw wpisz swoja odpowiedz."), text_to_display1("Poprawna odpowiedz: 20."))
    })
    
    output$text_out1 <- renderPrint({  text_to_display1() })
    
    output$summary1 <- renderUI({ 
      HTML(paste("Co bylo zle:",
                 "- rozciagniecie slupkow poprzez dodanie podpisow w ramkach w kolorze slupkow",
                 "- zly podpis slupka dla partii Nowoczesna", 
                 "- zbedne liczby pod slupkami, nie wiadomo co znacza",
                 "- brak podpisow osi, brak tytulu",
                 " ",
                 "Co zostalo poprawione:",
                 "- podpisy pod slupkami bez ramek",
                 "- poprawny podpis slupka dla partii Nowoczesna", 
                 "- pozbyto sie zbednych liczb pod slupkami",
                 "- dodano podpis osi pionowej i tytul",
                 sep="<br/>"))
    })
    
    output[["img1"]] <- renderImage({
      
      filename <- normalizePath(file.path("./images", paste("elastyczne_sondaze", ".png", sep = "")))
      list(src = filename, height = 400)
    
      }, deleteFile = FALSE)
    
    
    output[["plot1"]] <- renderPlot(
        plot_1()
    )
    
    
    text_to_display2 <- reactiveVal("Wcisnij przycisk 'Sprawdz', aby poznac poprawna odpowiedz")
    
    observeEvent(input$runRF2, {
      ifelse(input$text_in2 == "", text_to_display2("Najpierw wpisz swoja odpowiedz."), text_to_display2("Poprawna odpowiedz: 2.5 USD."))
    })
    
    output$text_out2 <- renderPrint({  text_to_display2() })
    
    output$summary2 <- renderUI({ 
      HTML(paste("Co bylo zle:",
                 "- wykres slupkowy i trojwymiarowy jednoczesnie",
                 "- brak podpisow osi",
                 "- nieposortowane dane",
                 " ",
                 "Co zostalo poprawione:",
                 "- wykres slupkowy, dwuwymiarowy",
                 "- dodano podpis osi pionowej",
                 "- posortowano dane",
                 sep="<br/>"))
    })
    
    output[["img2"]] <- renderImage({
      
      filename <- normalizePath(file.path("./images", paste("jak_po_metaamfetaminie", ".png", sep = "")))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot2"]] <- renderPlot(
        plot_2()
    )
    
    
    text_to_display3 <- reactiveVal("Wcisnij przycisk 'Sprawdz', aby poznac poprawna odpowiedz")
    
    observeEvent(input$runRF3, {
      ifelse(input$text_in3 == "", text_to_display3("Najpierw wpisz swoja odpowiedz."), text_to_display3("Poprawna odpowiedz: 72.3%."))
    })
    
    output$text_out3 <- renderPrint({  text_to_display3() })
    
    output$summary3 <- renderUI({ 
      HTML(paste("Co bylo zle:",
                 "- wykres kolowy i trojwymiarowy jednoczesnie",
                 "- powtorzenie tego samego wykresu 6 razy, pod roznymi katami", 
                 "- brak podpisow kolejnych czesci wykresu",
                 " ",
                 "Co zostalo poprawione:",
                 "- jeden wykres kolowy, dwuwymiarowy",
                 "- dodano podpisy kolejnych czesci wykresu", 
                 " ",
                 " ",
                 sep="<br/>"))
    })
    
    output[["img3"]] <- renderImage({
      
      filename <- normalizePath(file.path("./images", paste("niech_sie_kreci", ".png", sep = "")))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot3"]] <- renderPlotly(
        plot_3()
    )
    
    
    text_to_display4 <- reactiveVal("Wcisnij przycisk 'Sprawdz', aby poznac poprawna odpowiedz")
    
    observeEvent(input$runRF4, {
      ifelse(input$text_in4 == "", text_to_display4("Najpierw wpisz swoja odpowiedz."), text_to_display4("Poprawna odpowiedz: ."))
    })
    
    output$text_out4 <- renderPrint({  text_to_display4() })
    
    output$summary4 <- renderUI({ 
      HTML(paste("Co bylo zle:",
                 "- ",
                 "- ", 
                 " ",
                 "Co zostalo poprawione:",
                 "- ",
                 "- ", 
                 sep="<br/>"))
    })
    
    output[["img4"]] <- renderImage({
      
      filename <- normalizePath(file.path("./images", paste("kobiety", ".png", sep = "")))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot4"]] <- renderPlot(
      plot_4()
    )
    
    
    text_to_display5 <- reactiveVal("Wcisnij przycisk 'Sprawdz', aby poznac poprawna odpowiedz")
    
    observeEvent(input$runRF5, {
      ifelse(input$text_in5 == "", text_to_display5("Najpierw wpisz swoja odpowiedz."), text_to_display5("Poprawna odpowiedz: ."))
    })
    
    output$text_out5 <- renderPrint({  text_to_display5() })
    
    output$summary5 <- renderUI({ 
      HTML(paste("Co bylo zle:",
                 "- ",
                 "- ", 
                 " ",
                 "Co zostalo poprawione:",
                 "- ",
                 "- ", 
                 sep="<br/>"))
    })
    
    output[["img5"]] <- renderImage({
      
      filename <- normalizePath(file.path("./images", paste("niewiem", ".png", sep = "")))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot5"]] <- renderPlot(
      plot_5()
    )
    
    
    text_to_display6 <- reactiveVal("Wcisnij przycisk 'Sprawdz', aby poznac poprawna odpowiedz")
    
    observeEvent(input$runRF6, {
      ifelse(input$text_in6 == "", text_to_display6("Najpierw wpisz swoja odpowiedz."), text_to_display6("Poprawna odpowiedz: ."))
    })
    
    output$text_out6 <- renderPrint({  text_to_display6() })
    
    output$summary6 <- renderUI({ 
      HTML(paste("Co bylo zle:",
                 "- ",
                 "- ", 
                 " ",
                 "Co zostalo poprawione:",
                 "- ",
                 "- ", 
                 sep="<br/>"))
    })
    
    output[["img6"]] <- renderImage({
      
      filename <- normalizePath(file.path("./images", paste("kino", ".png", sep = "")))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    output[["plot6"]] <- renderPlot(
      plot_6()
    )
    
    text_to_display7 <- reactiveVal("Wcisnij przycisk 'Sprawdz', aby poznac poprawna odpowiedz")
    
    observeEvent(input$runRF7, {
      ifelse(input$text_in7 == "", text_to_display7("Najpierw wpisz swoja odpowiedz."), text_to_display7("Poprawna odpowiedz: 53."))
    })
    
    output$text_out7 <- renderPrint({  text_to_display7() })
    
    output$summary7 <- renderUI({ 
      HTML(paste("Co bylo zle:",
                 "- rozciagniecie slupkow poprzez dodanie podpisow w ramkach w kolorze slupkow",
                 "- zly podpis slupka dla partii Nowoczesna", 
                 "- zbedne liczby pod slupkami, nie wiadomo co znacza",
                 "- brak podpisow osi, brak tytulu",
                 " ",
                 "Co zostalo poprawione:",
                 "- podpisy pod slupkami bez ramek",
                 "- poprawny podpis slupka dla partii Nowoczesna", 
                 "- pozbyto sie zbednych liczb pod slupkami",
                 "- dodano podpis osi pionowej i tytul",
                 sep="<br/>"))
    })
    
    output[["img7"]] <- renderImage({
      
      filename <- normalizePath(file.path("./images", paste("matka", ".jpg", sep = "")))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    
    output[["plot7"]] <- renderPlot(
      plot_7()
    )
    text_to_display8 <- reactiveVal("Wcisnij przycisk 'Sprawdz', aby poznac poprawna odpowiedz")
    
    observeEvent(input$runRF8, {
      ifelse(input$text_in8 == "", text_to_display8("Najpierw wpisz swoja odpowiedz."), text_to_display8("Poprawna odpowiedz: 53."))
    })
    
    output$text_out8 <- renderPrint({  text_to_display8() })
    
    output$summary8 <- renderUI({ 
      HTML(paste("Co bylo zle:",
                 "- rozciagniecie slupkow poprzez dodanie podpisow w ramkach w kolorze slupkow",
                 "- zly podpis slupka dla partii Nowoczesna", 
                 "- zbedne liczby pod slupkami, nie wiadomo co znacza",
                 "- brak podpisow osi, brak tytulu",
                 " ",
                 "Co zostalo poprawione:",
                 "- podpisy pod slupkami bez ramek",
                 "- poprawny podpis slupka dla partii Nowoczesna", 
                 "- pozbyto sie zbednych liczb pod slupkami",
                 "- dodano podpis osi pionowej i tytul",
                 sep="<br/>"))
    })
    
    output[["img8"]] <- renderImage({
      
      filename <- normalizePath(file.path("./images", paste("emerytury", ".png", sep = "")))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    
    output[["plot8"]] <- renderPlot(
      plot_8()
    )
    text_to_display9 <- reactiveVal("Wcisnij przycisk 'Sprawdz', aby poznac poprawna odpowiedz")
    
    observeEvent(input$runRF9, {
      ifelse(input$text_in9 == "", text_to_display9("Najpierw wpisz swoja odpowiedz."), text_to_display9("Poprawna odpowiedz: 53."))
    })
    
    output$text_out9 <- renderPrint({  text_to_display9() })
    
    output$summary9 <- renderUI({ 
      HTML(paste("Co bylo zle:",
                 "- rozciagniecie slupkow poprzez dodanie podpisow w ramkach w kolorze slupkow",
                 "- zly podpis slupka dla partii Nowoczesna", 
                 "- zbedne liczby pod slupkami, nie wiadomo co znacza",
                 "- brak podpisow osi, brak tytulu",
                 " ",
                 "Co zostalo poprawione:",
                 "- podpisy pod slupkami bez ramek",
                 "- poprawny podpis slupka dla partii Nowoczesna", 
                 "- pozbyto sie zbednych liczb pod slupkami",
                 "- dodano podpis osi pionowej i tytul",
                 sep="<br/>"))
    })
    
    output[["img9"]] <- renderImage({
      
      filename <- normalizePath(file.path("./images", paste("sluby", ".png", sep = "")))
      list(src = filename, height = 400)
      
    }, deleteFile = FALSE)
    
    
    output[["plot9"]] <- renderPlot(
      plot_9()
    )
}

shinyApp(ui, server)
