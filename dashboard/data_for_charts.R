#---------------------------------------------------------------------------------------------------------------------------
#
# WD - Projekt 2
#
#---------------------------------------------------------------------------------------------------------------------------
#
# Dane do wykresow i wykresy
#
#---------------------------------------------------------------------------------------------------------------------------

### Dane

## 1) "elastyczne sondaze"
partie <- c("PiS", "Nowoczesna", "PO", "Kukiz'15", "PSL", "SLD", "Razem", "Wolnosc", "Kongres Nowej Prawicy", "Prawica RP", "Nie wiem")
procenty <- c(30.1, 20.0, 15.2, 8.6, 5.4, 5.2, 4.0, 2.8, 0.9, 0.0, 7.9)
df1 <- data.frame(partie, procenty)


## 2) "jak po metaamfetaminie"
kraje <- c("Myanmar", "Laos", "Wietnam", "Kambodza", "Tajlandia\n(najtaniej)", "Tajlandia\n(najdrozej)", "Malezja", "Chiny")
ceny <- c(2, 2, 2.5, 4, 3, 10, 11, 25)
df2 <- data.frame(kraje, ceny)


## 3) "niech sie kreci"
Race <- c("White", "Black", "Asian", "Mixed Race", "NS (Not Stated)", "Other")
convictions <- c(6743, 885, 711, 322, 553, 110)
percents <- 100 * convictions / sum(convictions)
df3 <- data.frame(Race, convictions)


## 4) "kobiety giganty"
woman <- data.frame(Country = c("Lativa", "Australia", "Scotland", "Peru", "South Africa", "India"),
                    Height = c(1.68, 1.65, 1.65, 1.65, 1.58, 1.52))


## 5) Nie znam sie
nazwa <- c("Swiatowe Dni Mlodziezy", "Realizacja programu Rodzina 500+", "Wizyta papieza w Polsce", 
          "Wybory 2015 - parlamentarne/prezydenckie", "Rzady PIS, nowa sytuacja polityczna", "Szczyt NATO",
          "Konflikt wokol‚ TK", "Inne rzadowe przedsiewziecia, reformy", "1050 rocznica Chrztu Polski",
          "Protesty spoleczne", "Reforma edukacji", "Brexit", "Wydarzenia z dziedziny gospodarki", 
          "Obnizenie wieku emerytalnego", "Intronizacja Chrystusa na krola Polski", "Wybory prezydenckie w USA",
          "Smierc gornikow w kopalni miedzi", "Reformy podatkowe", "Ekshumacje ofiar katastrofy smolenskiej",
          "Wydarzenia sportowe")

procenty <- c(14, 10.3, 3.4, 3.1, 2.9, 2.7, 2.1, 1.2, 1.1, 1, 1, 0.9, 0.9, 0.8, 0.4, 0.3, 0.3, 0.2, 0.2, 0.2)

wydarzenia_df <- data.frame(Wydarzenie = nazwa, Glosy = procenty) 


## 6) Kinowe hity
movies <- data.frame(Title = c("Avengers: \nKoniec Gry","Avengers: \nWojna bez konca","Szybcy i wsciekli \n8",
                               "Gwiezdne Wojny: \nPrzebudzenie Mocy","Jurassic World","Harry Potter \ni Insygnia smierci II","Kapitan Marvel"),
                     Income = c(1209, 640.5, 541.9, 529, 525.5, 483.20, 456.7))


## 7) 


## 8) 


## 9)


#---------------------------------------------------------------------------------------------------------------------------

### Wykresy


## 1) "elastyczne sondaze"
plot_1 <- function() {
  
  ggplot(df1, aes(x = reorder(partie, -procenty), y = procenty, fill = partie)) +
    geom_bar(stat = "identity") +
    scale_fill_manual("legend", values = c("PiS" = "navyblue", "Nowoczesna" = "dodgerblue", 
                                           "PO" = "darkorange1", "Kukiz'15" = "black", 
                                           "PSL" = "green3", "SLD" = "red", 
                                           "Razem" = "hotpink4", "Wolnosc" = "goldenrod1", 
                                           "Kongres Nowej Prawicy" = "cornflowerblue", 
                                           "Prawica RP" = "firebrick4", "Nie wiem" = "plum")) +
    ylim(0, 32) +
    geom_label(aes(label = paste0(sprintf("%0.1f", round(df1$procenty, digits = 1)), "%")), fill = "white", vjust = -0.15) +
    ggtitle("Wyniki sondazu") +
    xlab("") + ylab("Wynik procentowy") +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold", vjust = 2),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          legend.position = "none")
  
}


## 2) "jak po metaamfetaminie"
plot_2 <- function () {
  
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
}


## 3) "niech sie kreci"
plot_3 <- function() {
  
  plot_ly(df3, labels = ~Race, values = ~convictions, type = "pie", textposition = "outside") %>%
    layout(title = "Convictions in England and Wales for class B drug supply",
           font = list(size = 16),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           legend = list(x = 100, y = 0.4),
           margin = 0)
  
}


## 4) "kobiety giganty"
plot_4 <- function() {
  
  ggplot(woman) + 
    geom_bar(aes(x = reorder(Country, -Height), y=Height),stat="identity",fill='darkred',width = 0.3) +
    labs(title = "Average female height", x = "Country", y = "Height") +
    geom_label(aes(x = reorder(Country, -Height), y=Height, label = Height), 
               label.padding = unit(0.1, "lines"), vjust = -0.2, label.size = 0.05) +
    scale_y_continuous(breaks = seq(0, max(woman[["Height"]]+0.02), by = 0.1)) + 
    theme_bw()
  
}


## 5) Nie znam sie
plot_5 <- function() {
  
}


## 6) Kinowe hity
plot_6 <- function() {
  
  ggplot(movies) + 
    geom_bar(aes(x = reorder(Title, -Income), y=Income),stat="identity",fill='darkred') +
    labs(title = "Kinowe rekordy otwarcia (w mln $)", x = "Tytul", y = "Zysk") +
    theme_bw()
  
}


## 7) 


## 8) 


## 9)

