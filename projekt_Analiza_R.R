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
# Usuwamy te kolumny, poniewa¿ nie dostarczaj¹ nam ¿adnej wartoœci w dalszej czêœci analizy
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

#W przypadku data rozbijam j¹ na rok oraz miesi¹c(dzieñ odrzucam, poniewa¿ nie jest potrzebny w analizie)
# Zosta³o to wykonane w programie Excel ju¿ przy wstepnym czyszczeniu tam danych. Pozosta³o mi jednak zmiana miesiêcy
# z wartoœci numerycznych na znaki, dla lepszego efektu wizualnego na wykresach i wiêkszej jego czytelnoœci

month.abb
df$month <- month.abb[df$month]

#Ostatnim krokiem we wstêpnej obróbce danych jest zastapienie danych Na dla kolumny 'found_funny' na 0 dla
# ³atwiejszej póŸniejszej analizy

for (i in 1:nrow(df)){
  if(is.na(df$found_funny[i])){
    df$found_funny[i] <- 0
  }
}

# Na samym koñcu tej czêœci zmieniam czêœæ klas zmiennych na  typ numerical i podsumowuje wszystkie zebrane dane
df$found_funny <- as.numeric(df$found_funny)
df$compensation <- as.numeric(df$compensation)
df$hours <- as.numeric(df$hours)

summary(df)

################################################################################################################

df %>%
  mutate(each_year=year) %>%
  group_by(each_year) %>%
  count(each_year)

#Jak mo¿emy zobaczyæ, najwiêcej recenzji pojawi³o siê w 2021 roku pomimo tego, ¿e sama gra zosta³a dodana na steama
# w 2011 roku. Mo¿e to jednak wynikaæ z filtrów zastosowanych w web scraperze(który by³y biased w stosunku do póŸniejszych lat), dlatego najbardziej prawdopodobny
# rok z najwiêksz¹ iloœci¹ recenzji to rok 2014, co nadal jest mocno zaskakuj¹ce. Prawdopodobnie dopiero wtedy gra
# sta³a siê naprawdê popularna. Wynika to te¿ z tego, ¿e druga gra studia "A bird story" ukaza³a siê w³aœnie w 2014 roku
# co mo¿e byæ najbardziej prawdopodobnym wynikiem tego
Each_year <- df$year
hist(Each_year,
     main = 'Iloœæ napisanych recenzji w ka¿dym roku',
     xlab = 'Rok',
     col = 'darkmagenta',
     breaks = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021))


Each_month <- as.data.frame(table(df$month))
Each_month

months <- c('Styczeñ', 'Luty', 'Marzec', 'Kwiecieñ', 'Maj', 'Czerwiec', 'Lipiec', 'Sierpieñ', 'Wrzesieñ', 'PaŸdziernik', 'Listopad', 'Grudzieñ')
values <- c(271, 277, 201, 193, 211, 223, 159, 124, 92, 116, 230, 233)
Each_month_to_plot <- data.frame(months, values)

ggplot(Each_month_to_plot) + geom_point(aes(x = months, y = values, color = 'red'), size = 3.0) +
  xlab('Wartoœci liczbowe') +
  ylab('Miesi¹ce') +
  ggtitle('Iloœæ napisanych recenzji w ka¿dym miesi¹cu')

mean_hours <- mean(df$hours, na.rm = TRUE)
mean_hours
mean_hours_without_extremes <- mean(df$hours[df$hours < 270], na.rm = TRUE)
mean_hours_without_extremes
# Tutaj liczê œredni¹ iloœæ godzin na grê nie uwzglêdniaj¹c skrajnych przypadków, tj. 4100 godzin oraz 1153 godzin
# które mocno zaburzaj¹ wartoœæ œredniej, o ponad 2 godziny. Policzê dlatego jeszcze b³¹d wzglêdny
percentage_error <- ((mean_hours - mean_hours_without_extremes)/mean_hours) * 100
percentage_error
# W tym wypadku wynosi on ponad 25%, co jest b. wysokim wynikiem. Nastêpnie policzê œredni¹ iloœæ produktów
mean_products <- mean(df$products, na.rm = TRUE)
mean_products
# Wynosi ona ponad 625 gier na jedn¹ osobê. Nastêpnie policzê % osób, które poleci³oby t¹ grê
percentage_recommended <- (sum(df$recommended, na.rm = TRUE) / nrow(df)) * 100
percentage_recommended
# Wychodzi on na prawie 95% co jest bardzo dobrym wynikiem jak na grê z tak du¿¹ iloœci¹ recenzji. Ostatni¹ rzecz¹ jest
# policzenie ile œrednio osób uwa¿a³o dan¹ recenzjê za zabawn¹, pomijaj¹c recenzje, które takich wartoœci nie mia³y
mean_funny_reviews <- mean(df$found_funny[df$found_funny > 0], na.rm = TRUE)
mean_funny_reviews


# Na samym koñcu tej czêœci zliczê iloœæ wyrazów w ka¿dej recenzji, i obliczê œredni¹ iloœæ s³ów w recenzji, a tak¿e sprawdzê,
# czy jest zwi¹zek pomiêdzy d³ugoœci¹ recenzji, a d³ugoœæ grania w t¹ grê + d³ugoœæ grania, a polecenie gry


for(i in 1:nrow(df)){
  df$word_count[i] <- sapply(strsplit(df$text[i], " "), length)
}

mean_word_count <- mean(df$word_count, na.rm = TRUE)
mean_word_count

# Na sam koniec po³¹czê te wszystkie dane w jeden data frame
variable <- c(mean_hours, mean_hours_without_extremes, percentage_error, mean_products, percentage_recommended, mean_funny_reviews, mean_word_count)
names <- c('mean_hours', 'mean_hours_without_extremes', 'percentage_error', 'mean_products', 'percentage_recommended', 'mean_funny_reviews', 'mean_word_count')

every_fact <- data.frame(names, variable)
every_fact

#Nastêpny wykres to wykres zale¿noœci miêdzy d³ugoœci¹ recenzji, a iloœci¹ godzin spêdzonych nad gr¹
df$word_count <- as.numeric(df$word_count)

# Jak mo¿emy zobaczyæ, godziny grania, a d³ugoœæ recenzji nie jest w ¿aden sposób skorelowana ze sob¹
ggplot(df) +
  aes(x = hours, y = word_count) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

#Tym oto koñczy siê czêœæ pierwsza raportu. W nastêpnej czêœci zajmê siê dok³adniejsz¹ analiz¹ zebranych recenzji
#################################################################################################################
# W tej czêœci zajmê siê analiz¹ semantyczn¹ recenzji, ale zacznê najpierw od ich wstêpnego wyczyszczenia
# jak tokenizacji, usuniêcia znaków stop oraz lematyzacji

tidied_text <- df %>%
  unnest_tokens(word, text)

tidied_text_2 <- tidied_text %>%
  anti_join(stop_words)

tidied_test_ultimate <- tidied_text_2 %>%
  mutate(word2 = hunspell_stem(word))

#Nastêpnym krokiem bêdzie podliczenie najbardziej popularnych s³óW wystêpuj¹cych w recenzjach. Przedstawiê je na
# wykresie s³upkowym. Wczeœniej jednak muszê usun¹æ te wiersze, w których wystêpuj¹ nieznane znaki w celu lepszej
# przejrzystoœci


ultimate_most_popular_Words <- tidied_test_ultimate %>%
  count(word2) %>%
  arrange(desc(n))

ultimate_most_popular_Words <- ultimate_most_popular_Words[-c(1),]
head(ultimate_most_popular_Words)


# Jak mo¿emy zobaczyæ, najbardziej popluarnym slowem jest puste pole. Wynika to z tego, ze zamiast tego pola wystepuja
# inne znaki niewystepujace w angielskim, dlatego bedzie to potem pominiete w analizie oraz wykresach

barplot(ultimate_most_popular_Words[1:6,]$n, las = 2, names.arg = ultimate_most_popular_Words[2:7,]$word2, col = 'darkgreen', main="Top 6 najczêœciej wystêpuj¹cych s³ów", ylab="Czêstotliwoœæ wystêpowania")

#Kolejnym krokiem jest stworzenie chmury s³ów, która jest dobrym sposobem na zwizualizowanie i analizê danych
set.seed(587)
wordcloud(words = ultimate_most_popular_Words$word2, freq = ultimate_most_popular_Words$n, min.freq = 10, max.words = 60, random.order = FALSE,
          rot.per = 0.30, colors=brewer.pal(8, "Dark2"))

#Chmura s³ów wyraŸnie pokazuje dominacjê character(0), jednak poza tym dominuj¹ s³owa, jak 'story', 'game', 'heart', 'cry' i inne

# W kolejnym kroku bêdê oblicza³ i zajmowa³ siê analiz¹ sentymentów znajduj¹cych siê w tekœcie. W jej ramach korzystam ze
# s³ownika 'bing', a tak¿e sprawdzam sentyment dla ka¿dego u¿ytkownika, aby sprawdziæ najbardziej i najmniej negatywne recenzje

tidied_test_ultimate_bing <- tidied_test_ultimate %>%
  inner_join(get_sentiments("bing"))

glimpse(tidied_test_ultimate_bing)

tidied_summary <- tidied_test_ultimate_bing %>%
  group_by(username) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
  

#Ogólny sentyment pozytywny to 3108, a ogólny sentyment negatywny to -1281
sentiment_table <- table(tidied_summary$sentiment)
# Dodatkowo jak mo¿emy zobaczyæ w tabeli, wyniki oscylowa³y wokó³ 0, by³y raczej neutralnie nastawione do samej gry, co
# mo¿e dziwiæ, kiedy weŸmie siê pod uwagê jej pozytywne rekomendacje. Jednak nale¿y wzi¹æ tutaj pod uwagê nastrój gry
# Jest ona zdeycdowanie melancholijna, a wiêc s³owa jak 'cry' czy 'sad', które czêsto siê pojawia³y nie mia³y znaczenia negatywnego, 
# ale pozytywny, dlatego warto mieæ to na uwadzê przy analizie tekstów, które maj¹ byæ z za³o¿enia smutne

#Na sam koniec przeprowadzê klasyfikacje emocji ka¿dej z recenzji, aby sprawdziæ jakie emocje dominuj¹ w niej
d<-get_nrc_sentiment(text)
head (d,10)

# Aby móc te dane zwizualizowaæ, najpierw bêdê transponowa³ tabelê, zliczy wyst¹pienia ka¿dej z emocji, wyczysczê j¹, a na sam koniec stworzê plota, który to bêdzie lepiej obrazowa³
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[2:253]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Najczêstsze emocje")

# Na sam koniec analizy stworzê barplota, który poka¿e jak to wygl¹da³o procentowo

barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emocje w recenzji", xlab="Procent"
)
