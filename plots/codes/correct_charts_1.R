#---------------------------------------------------------------------------------------------------------------------------
#
# WD - Projekt 2 - Poprawne wykresy
#
#---------------------------------------------------------------------------------------------------------------------------

### Wczytanie pakietow

library(dplyr)
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

### Wykresy

## 1) "elastyczne sondaze"
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

## 2) "jak po metaamfetaminie"
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


## 3) "niech sie kreci"

# ggplot(df3, aes(x = "", y = percents, fill = Race)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y", start=0) + 
#   geom_text(aes(label = paste0(sprintf("%0.2f", round(df3$percents, digits = 2)), "%")), 
#             position = position_stack(vjust = 0.5), size = 5) +
#   scale_fill_manual(values = c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) +
#   ggtitle("Convictions in England and Wales for class B drug supply") +
#   xlab("") + ylab("") +
#   theme_void() + 
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#         axis.text.x = element_blank(),
#         legend.position = "right",
#         legend.title = element_text(size = 14, face = "bold"),
#         legend.text = element_text(size = 12))

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
