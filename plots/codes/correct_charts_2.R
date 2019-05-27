library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# 1. Kobiety giganty

# http://morethanmyheight.com/2018/04/10/global-perspective-of-female-height/?fbclid=IwAR0tzQaRW89uQfx50bXNwoeu2_DuRE0lxz4nutqRq-PMvDN7mMvf_jnq4Do

woman <- data.frame(Country = c("Lativa","Australia","Scotland","Peru","South Africa","India"),
                    Height = c(5.5,5.4,5.4,5.4,5.2,5.0))


ggplot(woman, aes(x = reorder(Country, -Height),Height, group = 1)) + 
  geom_point(color='darkred',size=3) +
  ylim(0,6) +
  labs(title = "Average female height", x = "Country", y = "Height") +
  theme_bw()


ggplot(woman) + 
  geom_bar(aes(x = reorder(Country, -Height), y=Height),stat="identity",fill='darkred',width = 0.3) +
  labs(title = "Average female height", x = "Country", y = "Height") +
  ylim(0,6) +
  theme_bw()



# 2. Kinowe hity

# http://next.gazeta.pl/next/7,151003,24710613,avengers-endgame-bije-wszelkie-rekordy-film-marvela-zarobil.html

movies <- data.frame(Title = c("Avengers: \nKoniec Gry","Avengers: \nWojna bez końca","Szybcy i wściekli \n8",
                               "Gwiezdne Wojny: \nPrzebudzenie Mocy","Jurassic World","Harry Potter \ni Insygnia Śmierci II","Kapitan Marvel"),
                    Income = c(1209,640.5,541.9,529,525.5,483.20,456.7))

ggplot(movies) + 
  geom_bar(aes(x = reorder(Title, -Income), y=Income),stat="identity",fill='darkred') +
  labs(title = "Kinowe rekordy otwarcia (w mln $)", x = "Tytuł", y = "Zysk") +
  theme_bw()



# 3. Ważne wydarzenia

# https://oko.press/bylo-najwazniejsze-2016-wedlug-polakow-sdm-500/
  
nazwa = c("Światowe Dni Młodzieży", "Realizacja programu Rodzina 500+", "Wizyta papieża w Polsce", 
               "Wybory 2015 - parlamentarne/prezydenckie", "Rządy PIS, nowa sytuacja polityczna", "Szczyt NATO",
               "Konflikt wokół TK", "Inne rządowe przedsięwzięcia, reformy", "1050 rocznica Chrztu Polski",
               "Protesty społeczne", "Reforma edukacji", "Brexit", "Wydarzenia z dziedziny gospodarki", 
               "Obniżenie wieku emerytalnego", "Intronizacja Chrystusa na króla Polski", "Wybory prezydenckie w USA",
               "Śmierć górników w kopalni miedzi", "Reformy podatkowe", "Ekshumacje ofiar katastrofy smoleńskiej",
               "Wydarzenia sportowe"
               )
  

procenty = c(14,10.3,3.4,3.1,2.9,2.7,2.1,1.2,1.1,1,1,0.9,0.9,0.8,0.4,0.3,0.3,0.2,0.2,0.2)


wydarzenia_df <- data.frame(Wydarzenie=nazwa,Głosy=procenty) 

plot <- ggplot(wydarzenia_df) + 
  geom_bar(aes(x = reorder(Wydarzenie, Głosy), y=Głosy),stat="identity",fill='darkred') +
  geom_label(aes(x = reorder(Wydarzenie, Głosy), y=Głosy,
                 label = paste0(wydarzenia_df$Głosy, "%")), 
             fill = "white", label.size = 0.1, vjust = 3, nudge_x =1.2,
             nudge_y = 0.3, label.padding = unit(0.15, "lines")) +
  scale_y_continuous(expand = c(0, 0.2)) +
  labs(x = "", y = "Procent odpowiedzi") +
  coord_flip() +
  theme_bw()

title_text <- textGrob("Które z wydarzeń mijającego roku można Pana(i) zdaniem uznać za najważniejsze dla Polski?", 
                       gp=gpar(fontsize=18, fontface="bold",lineheight=1),
                       just=c("left"), x=unit(0.05, "npc"), y=unit(0.05, "npc"))
subtitle_text <- textGrob("Dane w procentach, pytanie miało charakter otwarty", 
                          gp=gpar(fontsize=13), just=c("left"), x=unit(0.05, "npc"))
source_text <- textGrob("Najwięcej procent głosów: 42.5% uzyskała odpowiedź 'Nie wiem, nie zastanawiałem się, nie interesuje się'.\nPoniżej pozostałe odpowiedzi:",
                        gp=gpar(fontsize=14), just=c("left", "center"), x=unit(0.05, "npc"))
grid.arrange(title_text, subtitle_text, source_text, plot, ncol = 1, heights = c(1, 0.8, 1, 10))



