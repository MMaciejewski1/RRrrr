#biblioteki
library(quanteda)
library(epubr)
library(tidyverse)
library(tidytext)
library(purrrlyr)
library(tibble)


#do liczenia literkow
mf <- function(x, y){
  count <- table(strsplit(x, '')[[1]])
  count[names(count) == y]
}


#dunski
bookdir <- file.path("C:/Studia/Magisterka/Tkacz/rrrrrrrrrr")
file <- file.path("C:/Studia/Magisterka/Tkacz/rrrrrrrrrr/dun.epub")
list.files(bookdir, recursive = TRUE)
epub_meta(file) # parse EPUB file metadata only
all_data <- epub(file) # parse entire e-book
dun_data_from_data<-all_data$data[[1]]
dun_text_from_book<-dun_data_from_data$text
dun_text_withouth_interpunkcji<-gsub('\\.|\\,|\\!|\\?|\\;|\\-',' ',dun_text_from_book)

#slowacki
file <- file.path("C:/Studia/Magisterka/Tkacz/rrrrrrrrrr/slow.epub")
epub_meta(file) # parse EPUB file metadata only
all_data <- epub(file) # parse entire e-book
slo_data_from_data<-all_data$data[[1]]
slo_text_from_book<-slo_data_from_data$text
slo_text_withouth_interpunkcji<-gsub('\\.|\\,|\\!|\\?|\\;|\\-',' ',slo_text_from_book)

#francuski
file <- file.path("C:/Studia/Magisterka/Tkacz/rrrrrrrrrr/fra.epub")
epub_meta(file) # parse EPUB file metadata only
all_data <- epub(file) # parse entire e-book
fra_data_from_data<-all_data$data[[1]]
fra_text_from_book<-fra_data_from_data$text
fra_text_withouth_interpunkcji<-gsub('\\.|\\,|\\!|\\?|\\;|\\-',' ',fra_text_from_book)

#hiszpanski
file <- file.path("C:/Studia/Magisterka/Tkacz/rrrrrrrrrr/hiszp.epub")
epub_meta(file) # parse EPUB file metadata only
all_data <- epub(file) # parse entire e-book
hiszp_data_from_data<-all_data$data[[1]]
hiszp_text_from_book<-hiszp_data_from_data$text
hiszp_text_withouth_interpunkcji<-gsub('\\.|\\,|\\!|\\?|\\;|\\-',' ',hiszp_text_from_book)

#zmniejszenie literkow
sapply(hiszp_text_withouth_interpunkcji, tolower)
sapply(dun_text_withouth_interpunkcji, tolower)
sapply(fra_text_withouth_interpunkcji, tolower)
sapply(slo_text_withouth_interpunkcji, tolower)

#liczebnosc literek
count_char_hiszp=c((mf(hiszp_text_withouth_interpunkcji[2], 'a')),(mf(hiszp_text_withouth_interpunkcji[2], 'b')),(mf(hiszp_text_withouth_interpunkcji[2], 'c')),(mf(hiszp_text_withouth_interpunkcji[2], 'd')),(mf(hiszp_text_withouth_interpunkcji[2], 'e')),(mf(hiszp_text_withouth_interpunkcji[2], 'f')),(mf(hiszp_text_withouth_interpunkcji[2], 'g')),(mf(hiszp_text_withouth_interpunkcji[2], 'h')),(mf(hiszp_text_withouth_interpunkcji[2], 'i')),(mf(hiszp_text_withouth_interpunkcji[2], 'j')),(mf(hiszp_text_withouth_interpunkcji[2], 'k')),(mf(hiszp_text_withouth_interpunkcji[2], 'l')),(mf(hiszp_text_withouth_interpunkcji[2], 'm')),(mf(hiszp_text_withouth_interpunkcji[2], 'n')),(mf(hiszp_text_withouth_interpunkcji[2], 'ñ')),(mf(hiszp_text_withouth_interpunkcji[2], 'o')),(mf(hiszp_text_withouth_interpunkcji[2], 'p')),(mf(hiszp_text_withouth_interpunkcji[2], 'q')),(mf(hiszp_text_withouth_interpunkcji[2], 'r')),(mf(hiszp_text_withouth_interpunkcji[2], 's')),(mf(hiszp_text_withouth_interpunkcji[2], 't')),(mf(hiszp_text_withouth_interpunkcji[2], 'u')),(mf(hiszp_text_withouth_interpunkcji[2], 'v')),(mf(hiszp_text_withouth_interpunkcji[2], 'x')),(mf(hiszp_text_withouth_interpunkcji[2], 'y')),(mf(hiszp_text_withouth_interpunkcji[2], 'z')))
count_char_slo=c((mf(slo_text_withouth_interpunkcji[2], 'a')),(mf(slo_text_withouth_interpunkcji[2], 'á')),(mf(slo_text_withouth_interpunkcji[2], 'ä')),(mf(slo_text_withouth_interpunkcji[2], 'b')),(mf(slo_text_withouth_interpunkcji[2], 'c')),(mf(slo_text_withouth_interpunkcji[2], 'č')),(mf(slo_text_withouth_interpunkcji[2], 'd')),(mf(slo_text_withouth_interpunkcji[2], 'ď')),(mf(slo_text_withouth_interpunkcji[2], 'dz')),(mf(slo_text_withouth_interpunkcji[2], 'dž')),(mf(slo_text_withouth_interpunkcji[2], 'e')),(mf(slo_text_withouth_interpunkcji[2], 'é')),(mf(slo_text_withouth_interpunkcji[2], 'f')),(mf(slo_text_withouth_interpunkcji[2], 'g')),(mf(slo_text_withouth_interpunkcji[2], 'h')),(mf(slo_text_withouth_interpunkcji[2], 'i')),(mf(slo_text_withouth_interpunkcji[2], 'í')),(mf(slo_text_withouth_interpunkcji[2], 'j')),(mf(slo_text_withouth_interpunkcji[2], 'k')),(mf(slo_text_withouth_interpunkcji[2], 'l')),(mf(slo_text_withouth_interpunkcji[2], 'ĺ')),(mf(slo_text_withouth_interpunkcji[2], 'ľ')),(mf(slo_text_withouth_interpunkcji[2], 'm')),(mf(slo_text_withouth_interpunkcji[2], 'n')),(mf(slo_text_withouth_interpunkcji[2], 'ň')),(mf(slo_text_withouth_interpunkcji[2], 'o')),(mf(slo_text_withouth_interpunkcji[2], 'ó')),(mf(slo_text_withouth_interpunkcji[2], 'ô')),(mf(slo_text_withouth_interpunkcji[2], 'p')),(mf(slo_text_withouth_interpunkcji[2], 'q')),(mf(slo_text_withouth_interpunkcji[2], 'r')),(mf(slo_text_withouth_interpunkcji[2], 'ŕ')),(mf(slo_text_withouth_interpunkcji[2], 's')),(mf(slo_text_withouth_interpunkcji[2], 'š')),(mf(slo_text_withouth_interpunkcji[2], 't')),(mf(slo_text_withouth_interpunkcji[2], 'ť')),(mf(slo_text_withouth_interpunkcji[2], 'u')),(mf(slo_text_withouth_interpunkcji[2], 'ú')),(mf(slo_text_withouth_interpunkcji[2], 'v')),(mf(slo_text_withouth_interpunkcji[2], 'w')),(mf(slo_text_withouth_interpunkcji[2], 'y')),(mf(slo_text_withouth_interpunkcji[2], 'ý')),(mf(slo_text_withouth_interpunkcji[2], 'z')),(mf(slo_text_withouth_interpunkcji[2], 'ž')))
count_char_fra=c((mf(fra_text_withouth_interpunkcji[2], 'a')),(mf(fra_text_withouth_interpunkcji[2], 'b')),(mf(fra_text_withouth_interpunkcji[2], 'c')),(mf(fra_text_withouth_interpunkcji[2], 'd')),(mf(fra_text_withouth_interpunkcji[2], 'e')),(mf(fra_text_withouth_interpunkcji[2], 'f')),(mf(fra_text_withouth_interpunkcji[2], 'g')),(mf(fra_text_withouth_interpunkcji[2], 'h')),(mf(fra_text_withouth_interpunkcji[2], 'i')),(mf(fra_text_withouth_interpunkcji[2], 'j')),(mf(fra_text_withouth_interpunkcji[2], 'k')),(mf(fra_text_withouth_interpunkcji[2], 'l')),(mf(fra_text_withouth_interpunkcji[2], 'm')),(mf(fra_text_withouth_interpunkcji[2], 'n')),(mf(fra_text_withouth_interpunkcji[2], 'o')),(mf(fra_text_withouth_interpunkcji[2], 'p')),(mf(fra_text_withouth_interpunkcji[2], 'q')),(mf(fra_text_withouth_interpunkcji[2], 'r')),(mf(fra_text_withouth_interpunkcji[2], 's')),(mf(fra_text_withouth_interpunkcji[2], 't')),(mf(fra_text_withouth_interpunkcji[2], 'u')),(mf(fra_text_withouth_interpunkcji[2], 'v')),(mf(fra_text_withouth_interpunkcji[2], 'x')),(mf(fra_text_withouth_interpunkcji[2], 'y')),(mf(fra_text_withouth_interpunkcji[2], 'z')),(mf(fra_text_withouth_interpunkcji[2], 'à')),(mf(fra_text_withouth_interpunkcji[2], 'â')),(mf(fra_text_withouth_interpunkcji[2], 'ç')),(mf(fra_text_withouth_interpunkcji[2], 'é')),(mf(fra_text_withouth_interpunkcji[2], 'è')),(mf(fra_text_withouth_interpunkcji[2], 'ê')),(mf(fra_text_withouth_interpunkcji[2], 'ë')),(mf(fra_text_withouth_interpunkcji[2], 'î')),(mf(fra_text_withouth_interpunkcji[2], 'ï')),(mf(fra_text_withouth_interpunkcji[2], 'ô')),(mf(fra_text_withouth_interpunkcji[2], 'û')),(mf(fra_text_withouth_interpunkcji[2], 'ù')),(mf(fra_text_withouth_interpunkcji[2], 'ü')),(mf(fra_text_withouth_interpunkcji[2], 'ÿ')),(mf(fra_text_withouth_interpunkcji[2], 'œ')),(mf(fra_text_withouth_interpunkcji[2], 'æ')))
count_char_dun=c((mf(dun_text_withouth_interpunkcji[2], 'a')),(mf(dun_text_withouth_interpunkcji[2], 'b')),(mf(dun_text_withouth_interpunkcji[2], 'c')),(mf(dun_text_withouth_interpunkcji[2], 'd')),(mf(dun_text_withouth_interpunkcji[2], 'e')),(mf(dun_text_withouth_interpunkcji[2], 'f')),(mf(dun_text_withouth_interpunkcji[2], 'g')),(mf(dun_text_withouth_interpunkcji[2], 'h')),(mf(dun_text_withouth_interpunkcji[2], 'i')),(mf(dun_text_withouth_interpunkcji[2], 'j')),(mf(dun_text_withouth_interpunkcji[2], 'k')),(mf(dun_text_withouth_interpunkcji[2], 'l')),(mf(dun_text_withouth_interpunkcji[2], 'm')),(mf(dun_text_withouth_interpunkcji[2], 'n')),(mf(dun_text_withouth_interpunkcji[2], 'o')),(mf(dun_text_withouth_interpunkcji[2], 'p')),(mf(dun_text_withouth_interpunkcji[2], 'q')),(mf(dun_text_withouth_interpunkcji[2], 'r')),(mf(dun_text_withouth_interpunkcji[2], 's')),(mf(dun_text_withouth_interpunkcji[2], 't')),(mf(dun_text_withouth_interpunkcji[2], 'u')),(mf(dun_text_withouth_interpunkcji[2], 'v')),(mf(dun_text_withouth_interpunkcji[2], 'x')),(mf(dun_text_withouth_interpunkcji[2], 'y')),(mf(dun_text_withouth_interpunkcji[2], 'z')),(mf(dun_text_withouth_interpunkcji[2], 'Æ')),(mf(dun_text_withouth_interpunkcji[2], 'Ø')),(mf(dun_text_withouth_interpunkcji[2], ' Å')))
setwd("d:/")
write.csv(count_char_hiszp,'dobre literki z hiszpani.csv')
write.csv(count_char_slo,'dobre literki z slowakow.csv')
write.csv(count_char_fra,'dobre literki z francji.csv')
write.csv(count_char_dun,'dobre literki z duni.csv')
#digramy
digr_hisz<-dfm (hiszp_text_withouth_interpunkcji , n =2)
digr_dun<-dfm (dun_text_withouth_interpunkcji , n =2)
digr_fra<-dfm (fra_text_withouth_interpunkcji , n =2)
digr_slo<-dfm (slo_text_withouth_interpunkcji , n =2)

write.csv(digr_hiszp,'digramy z hiszpani.csv')
write.csv(digr_slo,'digramy z slowakow.csv')
write.csv(digr_fra,'digramy z francji.csv')
write.csv(digr_dun,'digramy z duni.csv')

#trigramy
trigr_hisz<-dfm (hiszp_text_withouth_interpunkcji , n =3)
trigr_dun<-dfm (dun_text_withouth_interpunkcji , n =3)
trigr_fra<-dfm (fra_text_withouth_interpunkcji , n =3)
trigr_slo<-dfm (slo_text_withouth_interpunkcji , n =3)

write.csv(trigr_hiszp,'trigramy z hiszpani.csv')
write.csv(trigr_slo,'trigramy z slowakow.csv')
write.csv(trigr_fra,'trigramy z francji.csv')
write.csv(trigr_dun,'trigramy z duni.csv')

#zmienne do wykresu
book_to_analize <-hiszp_text_withouth_interpunkcji #w zmiennych musi byc ksiazka wczytana
title_of_plot <- "Wykresik" #tytuly wykresow


#Wizualizacja trigramu
n_word <- 20
n_top <- 150
n_gramming <- 3
trigrams <- tibble(text = book_to_analize) %>% #text = zmienna ksiazki/textu cokolwiek
  unnest_tokens(trigram, text, token = "ngrams", n = n_gramming)
start_words <- c("lo", "ella") #Slowa poczatkowe
pattern <- str_c("^", start_words, " ", collapse = "|")
top_words <- trigrams %>%
  filter(str_detect(trigram, pattern)) %>%
  count(trigram, sort = TRUE) %>%
  slice(seq_len(n_top)) %>%
  pull(trigram)
trigrams <- trigrams %>%
  filter(trigram %in% top_words)

#function to extract the nth word in a string.
str_nth_word <- function(x, n, sep = " ") {
  str_split(x, pattern = " ") %>%
    map_chr(~ .x[n])
}

#wezly
nodes <- map_df(seq_len(n_gramming),
                ~ trigrams %>%
                  mutate(word = str_nth_word(trigram, .x)) %>%
                  count(word, sort = TRUE) %>%
                  slice(seq_len(n_word)) %>% 
                  mutate(y = seq(from = n_word + 1, to = 0, 
                                 length.out = n() + 2)[seq_len(n()) + 1],
                         x = .x))


#krawedz
sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}
egde_lines <- function(trigram, from_word, to_word, scale = 5, n = 50, 
                       x_space = 0) {
  
  from_word <- from_word %>%
    select(-n) %>%
    set_names(c("from", "y_from", "x_from"))
  to_word <- to_word %>%
    select(-n) %>%
    set_names(c("to", "y_to", "x_to"))
  links <- crossing(from = from_word$from, 
                    to = to_word$to) %>%
    mutate(word_pair = paste(from, to),
           number = map_dbl(word_pair, 
                            ~ sum(str_detect(trigram$trigram, .x)))) %>%
    left_join(from_word, by = "from") %>%
    left_join(to_word, by = "to")
  
  links %>%
    by_row(~ sigmoid(x_from = .x$x_from + 0.2 + x_space,
                     x_to = .x$x_to - 0.05, 
                     y_from = .x$y_from, y_to = .x$y_to, 
                     scale = scale, n = n) %>%
             mutate(word_pair = .x$word_pair,
                    number = .x$number,
                    from = .x$from)) %>%
    pull(.out) %>%
    bind_rows()
}

# egdes between first and second column
egde1 <- egde_lines(trigram = trigrams, 
                    from_word = filter(nodes, x == 1), 
                    to_word = filter(nodes, x == 2), 
                    n = 50) %>%
  filter(number > 0) %>%
  mutate(id = word_pair)

# Words in second colunm
## That start with he
second_word_he <- nodes %>%
  filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[1]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

## That start with she
second_word_she <- nodes %>%
  filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[2]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

# Words in third colunm
## That start with he
third_word_he <- nodes %>%
  filter(x == 3) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[1]) %>%
      mutate(word = str_nth_word(trigram, 3)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

## That start with she
third_word_she <- nodes %>%
  filter(x == 3) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[2]) %>%
      mutate(word = str_nth_word(trigram, 3)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

# egdes between second and third column that starts with he
egde2_he <- egde_lines(filter(trigrams, 
                              str_detect(trigram, paste0("^", start_words[1], " "))), 
                       second_word_he, third_word_he, n = 50) %>%
  mutate(y = y + 0.05,
         from = start_words[1],
         id = str_c(from, word_pair, sep = " ")) %>%
  filter(number > 0)

# egdes between second and third column that starts with she
egde2_she <- egde_lines(filter(trigrams, 
                               str_detect(trigram, paste0("^", start_words[2], " "))), 
                        second_word_she, third_word_she, n = 50) %>%
  mutate(y = y - 0.05,
         from = start_words[2],
         id = str_c(from, word_pair, sep = " ")) %>%
  filter(number > 0)

# All edges
edges <- bind_rows(egde1, egde2_he, egde2_she)

#Wykres dla Trigramu
tri <- nodes %>% 
  ggplot(aes(x, y, label = word, size = n)) +
  geom_text(hjust = 0, color = "#DDDDDD") +
  theme_void() +
  geom_line(data = edges,
            aes(x, y, group = id, color = from, alpha = sqrt(number)),
            inherit.aes = FALSE) +
  theme(plot.background = element_rect(fill = "#666666", colour = 'black'),
        text = element_text(color = "#EEEEEE", size = 15)) +
  guides(alpha = "none", color = "none", size = "none") +
  xlim(c(0.9, 3.2)) +
  scale_color_manual(values = c("#5EF1F1", "#FA62D0")) +
  labs(title = title_of_plot) + 
  scale_size(range = c(3, 8))




#Wizualizacja Bigramu
n_word <- 20
n_top <- 150
n_gramming <- 2
trigrams <- tibble(text = book_to_analize) %>% #text = zmienna ksiazki/textu cokolwiek
  unnest_tokens(trigram, text, token = "ngrams", n = n_gramming)
start_words <- c("lo", "ella") #Slowa poczatkowe
pattern <- str_c("^", start_words, " ", collapse = "|")
top_words <- trigrams %>%
  filter(str_detect(trigram, pattern)) %>%
  count(trigram, sort = TRUE) %>%
  slice(seq_len(n_top)) %>%
  pull(trigram)
trigrams <- trigrams %>%
  filter(trigram %in% top_words)


#wezly
nodes <- map_df(seq_len(n_gramming),
                ~ trigrams %>%
                  mutate(word = str_nth_word(trigram, .x)) %>%
                  count(word, sort = TRUE) %>%
                  slice(seq_len(n_word)) %>% 
                  mutate(y = seq(from = n_word + 1, to = 0, 
                                 length.out = n() + 2)[seq_len(n()) + 1],
                         x = .x))


#krawedz
sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}
egde_lines <- function(trigram, from_word, to_word, scale = 5, n = 50, 
                       x_space = 0) {
  
  from_word <- from_word %>%
    select(-n) %>%
    set_names(c("from", "y_from", "x_from"))
  to_word <- to_word %>%
    select(-n) %>%
    set_names(c("to", "y_to", "x_to"))
  links <- crossing(from = from_word$from, 
                    to = to_word$to) %>%
    mutate(word_pair = paste(from, to),
           number = map_dbl(word_pair, 
                            ~ sum(str_detect(trigram$trigram, .x)))) %>%
    left_join(from_word, by = "from") %>%
    left_join(to_word, by = "to")
  
  links %>%
    by_row(~ sigmoid(x_from = .x$x_from + 0.2 + x_space,
                     x_to = .x$x_to - 0.05, 
                     y_from = .x$y_from, y_to = .x$y_to, 
                     scale = scale, n = n) %>%
             mutate(word_pair = .x$word_pair,
                    number = .x$number,
                    from = .x$from)) %>%
    pull(.out) %>%
    bind_rows()
}

# egdes between first and second column
egde1 <- egde_lines(trigram = trigrams, 
                    from_word = filter(nodes, x == 1), 
                    to_word = filter(nodes, x == 2), 
                    n = 50) %>%
  filter(number > 0) %>%
  mutate(id = word_pair)

# Words in second colunm
## That start with he
second_word_he <- nodes %>%
  filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[1]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

## That start with she
second_word_she <- nodes %>%
  filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[2]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))


# All edges
edges <- bind_rows(egde1)

#Wykres dla bigramu
bi <- nodes %>% 
  ggplot(aes(x, y, label = word, size = n)) +
  geom_text(hjust = 0, color = "#DDDDDD") +
  theme_void() +
  geom_line(data = edges,
            aes(x, y, group = id, color = from, alpha = sqrt(number)),
            inherit.aes = FALSE) +
  theme(plot.background = element_rect(fill = "#666666", colour = 'black'),
        text = element_text(color = "#EEEEEE", size = 15)) +
  guides(alpha = "none", color = "none", size = "none") +
  xlim(c(0.9, 3.2)) +
  scale_color_manual(values = c("#5EF1F1", "#FA62D0")) +
  labs(title = title_of_plot) + 
  scale_size(range = c(3, 8))

bi #wyswietla wykres dla bigramu jak zostaly wykonane obliczenia czy cos
tri #jak wyzej
