library(DBI)

sent_dict <- read.csv("dicts/słownik_anotacji_emocjonlanej.csv")
str(sent_dict)

table(sent_dict$stopien_nacechowania)

sent_dict <- sent_dict %>% 
  mutate(stopien_nacechowania = trimws(stopien_nacechowania)) %>% 
  mutate(stopien_nacechowania = case_when(stopien_nacechowania %in% c("amb", "NULL", "") ~ NA,
                                          .default = stopien_nacechowania))

database_connection <- dbConnect(RSQLite::SQLite(), "tagged_database.sqlite")

library(tidyverse)

table(sent_dict$stopien_nacechowania)

sent_dict_deg <- sent_dict %>% 
  filter(!is.na(stopien_nacechowania)) %>% 
  select(lemat, stopien_nacechowania) %>% 
  unique()

sent_dict_deg %>% group_by(lemat) %>% 
  mutate(cc = n()) %>% 
  ungroup() %>% 
  group_by(cc) %>% 
  summarise(oo = n())
  
sent_dict_deg <- sent_dict_deg %>% 
  group_by(lemat) %>% 
  mutate(cc = n()) %>% 
  filter(cc == 1) %>% 
  select(lemat, stopien_nacechowania)

res <- dbGetQuery(conn = database_connection, 
                 statement = "select tagged as lemat, id from ipcontenttagged")
suppressMessages(res <- res %>% 
                   anti_join(y = data.frame(lemat = polish_stop_words())) %>% 
                   left_join(y = sent_dict_deg, by = "lemat") %>% 
                   group_by(id, stopien_nacechowania) %>% 
                   summarise(j = n()))
res_piv <- pivot_wider(data = res, id_cols = "id", names_from = "stopien_nacechowania", values_from = "j", values_fill = 0)
col_to_sum <- names(res_piv)[-1]
col_ex <- col_to_sum[-length(col_to_sum)]
res_piv$sum <- rowSums(res_piv[, col_to_sum], na.rm = T)
# - m <-- mocno negatywne,
# - s <-- słabo negatywne,
# 0 <-- neutralne,
# + s <-- słabo pozytywne,
# + m <-- mocno pozytywne
res_piv <- res_piv %>% 
  mutate(ss = `NA` +`+ m`+`+ s`+`- m`+`- s`,
         ve = (`+ m` + `+ s`) / (`- m` + `- s` + 1),
         vi = (`+ m` + `+ s`) - (`- m` + `- s`),
         vo = vi / `NA`)
summary(res_piv$ve)
ggplot(data = res_piv, mapping = aes(x = ss, y = ve)) +
  geom_point(shape = 21)
ggplot(data = res_piv, mapping = aes(x = ss, y = vi)) +
  geom_point(shape = 21)
ggplot(data = res_piv, mapping = aes(x = ss, y = vo)) +
  geom_point(shape = 21)

# wybierz recenzje i pomaluj ja zgodnie z mapowniaem sentymentu?
res_piv %>% arrange(-vo)
topres <- dbGetQuery(conn = database_connection, 
                  statement = "select original, tagged as lemat, id from ipcontenttagged where ipcontenttagged.id = '2156'") %>% 
  left_join(y = data.frame(lemat = polish_stop_words(), st = 1)) %>% 
  left_join(y = sent_dict_deg, by = "lemat")
topres[1:20, ]

library(ggtext)
topres <- topres %>% 
  mutate(oo = case_when(stopien_nacechowania == "- m" ~ sprintf("<span style='color:red'>**%s**</span>", original),
                        stopien_nacechowania == "- s" ~ sprintf("<span style='color:red'>%s</span>", original),
                        stopien_nacechowania == "+ s" ~ sprintf("<span style='color:green'>%s</span>", original),
                        stopien_nacechowania == "+ m" ~ sprintf("<span style='color:green'>**%s**</span>", original),
                        .default = original),
         cc = floor(cumsum(nchar(original)) / 80))

ggplot(data = topres[1:220, ] %>% group_by(cc) %>% summarise(l = paste0(oo, collapse = " ")) %>% .$l %>% paste0(collapse = "<br>") %>% enframe(), mapping = aes(x = 1, y = 1, label = value)) +
  ggtext::geom_richtext() +
  theme_void()

# do kogo sa te interpelacje?
# przez kogo sa skladane?
# kiedy byly skladane?
