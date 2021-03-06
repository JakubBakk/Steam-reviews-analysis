library(dplyr)
library(tidyverse)
library(tidytext)
library(stopwords)
library(hunspell)
library(textstem)
library(stringi)
library(openxlsx)
library(janitor)
library(ggplot2)
library(ggpubr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)

data <- read.xlsx('Dane_steam_To_the_Moon.xlsx', sheet = 1, skipEmptyRows = TRUE)

df <- subset(data, select = -c(product_id, page, page_order, user_id, date))
# Usuwamy te kolumny, poniewa� nie dostarczaj� nam �adnej warto�ci w dalszej cz�ci analizy
head(df)


# Dla compensation, gdzie 0-> normalnie kupiona gra, 1-> gra otrzymana za darmo
for(i in 1:nrow(df)){
  if(is.na(df$compensation[i])){
    df$compensation[i] <- 0
  }
  else{
    df$compensation[i] <- 1
  }
}

#W przypadku data rozbijam j� na rok oraz miesi�c(dzie� odrzucam, poniewa� nie jest potrzebny w analizie)
# Zosta�o to wykonane w programie Excel ju� przy wstepnym czyszczeniu tam danych. Pozosta�o mi jednak zmiana miesi�cy
# z warto�ci numerycznych na znaki, dla lepszego efektu wizualnego na wykresach i wi�kszej jego czytelno�ci

month.abb
df$month <- month.abb[df$month]

#Ostatnim krokiem we wst�pnej obr�bce danych jest zastapienie danych Na dla kolumny 'found_funny' na 0 dla
# �atwiejszej p�niejszej analizy

for (i in 1:nrow(df)){
  if(is.na(df$found_funny[i])){
    df$found_funny[i] <- 0
  }
}

# Na samym ko�cu tej cz�ci zmieniam cz�� klas zmiennych na  typ numerical i podsumowuje wszystkie zebrane dane
df$found_funny <- as.numeric(df$found_funny)
df$compensation <- as.numeric(df$compensation)
df$hours <- as.numeric(df$hours)

summary(df)

################################################################################################################

df %>%
  mutate(each_year=year) %>%
  group_by(each_year) %>%
  count(each_year)

#Jak mo�emy zobaczy�, najwi�cej recenzji pojawi�o si� w 2021 roku pomimo tego, �e sama gra zosta�a dodana na steama
# w 2011 roku. Mo�e to jednak wynika� z filtr�w zastosowanych w web scraperze(kt�ry by�y biased w stosunku do p�niejszych lat), dlatego najbardziej prawdopodobny
# rok z najwi�ksz� ilo�ci� recenzji to rok 2014, co nadal jest mocno zaskakuj�ce. Prawdopodobnie dopiero wtedy gra
# sta�a si� naprawd� popularna. Wynika to te� z tego, �e druga gra studia "A bird story" ukaza�a si� w�a�nie w 2014 roku
# co mo�e by� najbardziej prawdopodobnym wynikiem tego
Each_year <- df$year
hist(Each_year,
     main = 'Ilo�� napisanych recenzji w ka�dym roku',
     xlab = 'Rok',
     col = 'darkmagenta',
     breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021))


Each_month <- as.data.frame(table(df$month))
Each_month

months <- c('Stycze�', 'Luty', 'Marzec', 'Kwiecie�', 'Maj', 'Czerwiec', 'Lipiec', 'Sierpie�', 'Wrzesie�', 'Pa�dziernik', 'Listopad', 'Grudzie�')
values <- c(271, 277, 201, 193, 211, 223, 159, 124, 92, 116, 230, 233)
Each_month_to_plot <- data.frame(months, values)

ggplot(Each_month_to_plot) + geom_point(aes(x = months, y = values, color = 'red'), size = 3.0) +
  xlab('Warto�ci liczbowe') +
  ylab('Miesi�ce') +
  ggtitle('Ilo�� napisanych recenzji w ka�dym miesi�cu')

mean_hours <- mean(df$hours, na.rm = TRUE)
mean_hours
mean_hours_without_extremes <- mean(df$hours[df$hours < 270], na.rm = TRUE)
mean_hours_without_extremes
# Tutaj licz� �redni� ilo�� godzin na gr� nie uwzgl�dniaj�c skrajnych przypadk�w, tj. 4100 godzin oraz 1153 godzin
# kt�re mocno zaburzaj� warto�� �redniej, o ponad 2 godziny. Policz� dlatego jeszcze b��d wzgl�dny
percentage_error <- ((mean_hours - mean_hours_without_extremes)/mean_hours) * 100
percentage_error
# W tym wypadku wynosi on ponad 25%, co jest b. wysokim wynikiem. Nast�pnie policz� �redni� ilo�� produkt�w
mean_products <- mean(df$products, na.rm = TRUE)
mean_products
# Wynosi ona ponad 625 gier na jedn� osob�. Nast�pnie policz� % os�b, kt�re poleci�oby t� gr�
percentage_recommended <- (sum(df$recommended, na.rm = TRUE) / nrow(df)) * 100
percentage_recommended
# Wychodzi on na prawie 95% co jest bardzo dobrym wynikiem jak na gr� z tak du�� ilo�ci� recenzji. Ostatni� rzecz� jest
# policzenie ile �rednio os�b uwa�a�o dan� recenzj� za zabawn�, pomijaj�c recenzje, kt�re takich warto�ci nie mia�y
mean_funny_reviews <- mean(df$found_funny[df$found_funny > 0], na.rm = TRUE)
mean_funny_reviews


# Na samym ko�cu tej cz�ci zlicz� ilo�� wyraz�w w ka�dej recenzji, i oblicz� �redni� ilo�� s��w w recenzji, a tak�e sprawdz�,
# czy jest zwi�zek pomi�dzy d�ugo�ci� recenzji, a d�ugo�� grania w t� gr� + d�ugo�� grania, a polecenie gry


for(i in 1:nrow(df)){
  df$word_count[i] <- sapply(strsplit(df$text[i], " "), length)
}

mean_word_count <- mean(df$word_count, na.rm = TRUE)
mean_word_count

# Na sam koniec po��cz� te wszystkie dane w jeden data frame
variable <- c(mean_hours, mean_hours_without_extremes, percentage_error, mean_products, percentage_recommended, mean_funny_reviews, mean_word_count)
names <- c('mean_hours', 'mean_hours_without_extremes', 'percentage_error', 'mean_products', 'percentage_recommended', 'mean_funny_reviews', 'mean_word_count')

every_fact <- data.frame(names, variable)
every_fact

#Nast�pny wykres to wykres zale�no�ci mi�dzy d�ugo�ci� recenzji, a ilo�ci� godzin sp�dzonych nad gr�
df$word_count <- as.numeric(df$word_count)

# Jak mo�emy zobaczy�, godziny grania, a d�ugo�� recenzji nie jest w �aden spos�b skorelowana ze sob�
ggplot(df) +
  aes(x = hours, y = word_count) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

#Tym oto ko�czy si� cz�� pierwsza raportu. W nast�pnej cz�ci zajm� si� dok�adniejsz� analiz� zebranych recenzji
#################################################################################################################
# W tej cz�ci zajm� si� analiz� semantyczn� recenzji, ale zaczn� najpierw od ich wst�pnego wyczyszczenia
# jak tokenizacji, usuni�cia znak�w stop oraz lematyzacji

tidied_text <- df %>%
  unnest_tokens(word, text)

tidied_text_2 <- tidied_text %>%
  anti_join(stop_words)

tidied_test_ultimate <- tidied_text_2 %>%
  mutate(word2 = hunspell_stem(word))

#Nast�pnym krokiem b�dzie podliczenie najbardziej popularnych s��W wyst�puj�cych w recenzjach. Przedstawi� je na
# wykresie s�upkowym. Wcze�niej jednak musz� usun�� te wiersze, w kt�rych wyst�puj� nieznane znaki w celu lepszej
# przejrzysto�ci


ultimate_most_popular_Words <- tidied_test_ultimate %>%
  count(word2) %>%
  arrange(desc(n))

ultimate_most_popular_Words <- ultimate_most_popular_Words[-c(1),]
head(ultimate_most_popular_Words)


# Jak mo�emy zobaczy�, najbardziej popluarnym slowem jest puste pole. Wynika to z tego, ze zamiast tego pola wystepuja
# inne znaki niewystepujace w angielskim, dlatego bedzie to potem pominiete w analizie oraz wykresach

barplot(ultimate_most_popular_Words[1:6,]$n, las = 2, names.arg = ultimate_most_popular_Words[2:7,]$word2, col = 'darkgreen', main="Top 6 najcz�ciej wyst�puj�cych s��w", ylab="Cz�stotliwo�� wyst�powania")

#Kolejnym krokiem jest stworzenie chmury s��w, kt�ra jest dobrym sposobem na zwizualizowanie i analiz� danych
set.seed(587)
wordcloud(words = ultimate_most_popular_Words$word2, freq = ultimate_most_popular_Words$n, min.freq = 10, max.words = 60, random.order = FALSE,
          rot.per = 0.30, colors=brewer.pal(8, "Dark2"))

#Chmura s��w wyra�nie pokazuje dominacj� character(0), jednak poza tym dominuj� s�owa, jak 'story', 'game', 'heart', 'cry' i inne

# W kolejnym kroku b�d� oblicza� i zajmowa� si� analiz� sentyment�w znajduj�cych si� w tek�cie. W jej ramach korzystam ze
# s�ownika 'bing', a tak�e sprawdzam sentyment dla ka�dego u�ytkownika, aby sprawdzi� najbardziej i najmniej negatywne recenzje

tidied_test_ultimate_bing <- tidied_test_ultimate %>%
  inner_join(get_sentiments("bing"))

glimpse(tidied_test_ultimate_bing)

tidied_summary <- tidied_test_ultimate_bing %>%
  group_by(username) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
  

#Og�lny sentyment pozytywny to 3108, a og�lny sentyment negatywny to -1281
sentiment_table <- table(tidied_summary$sentiment)
# Dodatkowo jak mo�emy zobaczy� w tabeli, wyniki oscylowa�y wok� 0, by�y raczej neutralnie nastawione do samej gry, co
# mo�e dziwi�, kiedy we�mie si� pod uwag� jej pozytywne rekomendacje. Jednak nale�y wzi�� tutaj pod uwag� nastr�j gry
# Jest ona zdeycdowanie melancholijna, a wi�c s�owa jak 'cry' czy 'sad', kt�re cz�sto si� pojawia�y nie mia�y znaczenia negatywnego, 
# ale pozytywny, dlatego warto mie� to na uwadz� przy analizie tekst�w, kt�re maj� by� z za�o�enia smutne

#Na sam koniec przeprowadz� klasyfikacje emocji ka�dej z recenzji, aby sprawdzi� jakie emocje dominuj� w niej
d<-get_nrc_sentiment(text)
head (d,10)

# Aby m�c te dane zwizualizowa�, najpierw b�d� transponowa� tabel�, zliczy wyst�pienia ka�dej z emocji, wyczyscz� j�, a na sam koniec stworz� plota, kt�ry to b�dzie lepiej obrazowa�
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[2:253]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Najcz�stsze emocje")

# Na sam koniec analizy stworz� barplota, kt�ry poka�e jak to wygl�da�o procentowo

barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emocje w recenzji", xlab="Procent"
)
