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

#---------------------------------------------------------------------------------------------------------------------------

### Dane do wykresow

## 1) "elastyczne sondaze"
partie <- c("PiS", "Nowoczesna", "PO", "Kukiz'15", "PSL", "SLD", "Razem", "Wolność", "Kongres Nowej Prawicy", "Prawica RP", "Nie wiem")
procenty <- c(30.1, 20.0, 15.2, 8.6, 5.4, 5.2, 4.0, 2.8, 0.9, 0.0, 7.9)
df1 <- data.frame(partie, procenty)

## 2) "jak po metaamfetaminie"
kraje <- c("Myanmar", "Laos", "Wietnam", "Kambodża", "Tajlandia\n(najtaniej)", "Tajlandia\n(najdrożej)", "Malezja", "Chiny")
ceny <- c(2, 2, 2.5, 4, 3, 10, 11, 25)
df2 <- data.frame(kraje, ceny)

## 3) "niech sie kreci"
Race <- c("White", "Black", "Asian", "Mixed Race", "NS (Not Stated)", "Other")
convictions <- c(6743, 885, 711, 322, 553, 110)
percents <- 100 * convictions / sum(convictions)
df3 <- data.frame(Race, convictions)

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
                     menuSubItem("Sub-item 2", tabName = "subitem4"),
                     menuSubItem("Sub-item 1", tabName = "subitem5"),
                     menuSubItem("Sub-item 2", tabName = "subitem6"),
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
    
    #output[["img1"]] <- renderImage(img(src="elastyczne_sondaze.PNG", width = 190))
    
    output[["plot1"]] <- renderPlot(
        ggplot(df1, aes(x = reorder(partie, -procenty), y = procenty, fill = partie)) +
            geom_bar(stat = "identity") +
            scale_fill_manual("legend", values = c("PiS" = "navyblue", "Nowoczesna" = "dodgerblue", 
                                                   "PO" = "darkorange1", "Kukiz'15" = "black", 
                                                   "PSL" = "green3", "SLD" = "red", 
                                                   "Razem" = "hotpink4", "Wolność" = "goldenrod1", 
                                                   "Kongres Nowej Prawicy" = "cornflowerblue", 
                                                   "Prawica RP" = "firebrick4", "Nie wiem" = "plum")) +
            ylim(0, 32) +
            geom_label(aes(label = paste0(sprintf("%0.1f", round(df1$procenty, digits = 1)), "%")), fill = "white", vjust = -0.15) +
            ggtitle("Wyniki sondażu") +
            xlab("") + ylab("Wynik procentowy") +
            theme(plot.title = element_text(size = 16, face = "bold"),
                  axis.title.y = element_text(size = 14, face = "bold", vjust = 2),
                  axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                  axis.text.y = element_text(size = 12),
                  legend.position = "none")
    )
    
    # img2
    
    output[["plot2"]] <- renderPlot(
        ggplot(df2, aes(x = reorder(kraje, -ceny), y = ceny)) +
            geom_bar(stat = "identity", fill = "hotpink4") +
            ylim(0, 26) +
            geom_label(aes(label = ceny), vjust = -0.15) +
            ggtitle("Cena jednej tabletki metaamfetaminy w wybranych krajach Azji") +
            xlab("") + ylab("Cena [USD]") +
            theme(plot.title = element_text(size = 16, face = "bold"),
                  axis.title.y = element_text(size = 14, face = "bold", vjust = 2),
                  axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                  axis.text.y = element_text(size = 12),
                  legend.position = "none")
    )
    
    # img3
    
    output[["plot3"]] <- renderPlotly(
        plot_ly(df3, labels = ~Race, values = ~convictions, type = 'pie', textposition = 'outside') %>%
            add_annotations(
                yref = "paper", 
                xref = "paper", 
                y = 1, 
                x = 0.5,
                text = "Convictions in England and Wales for class B drug supply", 
                showarrow = F, 
                font = list(size = 20)
            ) %>% 
            layout(title = FALSE,
                   font = list(size = 16),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   legend = list(x = 100, y = 0.5))
    )
    
}

shinyApp(ui, server)
