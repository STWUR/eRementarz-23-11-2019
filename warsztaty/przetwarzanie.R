################################################################
#####################PAWEL POPLAWSKI############################
#####################eRementarz 23.11.2019######################
################################################################

library(tidyverse)
library(lubridate)
#Dane najczesciej wczytujemy z pliku.
data2 <- read_csv('//Los_Angeles_International_Airport_-_Passenger_Traffic_By_Terminal.csv',
                  col_names = TRUE,
                  skip = 0,
                  comment = "#",
                  na = c("NA", ".", "NOT_AVAILABLE", "66")
)
data2

#lub
data2 <- read_csv('//Los_Angeles_International_Airport_-_Passenger_Traffic_By_Terminal.csv',
                  col_names = TRUE,
                  skip = 0,
                  comment = "#",
                  na = c("NA", ".", "NOT_AVAILABLE", "66"),
                  col_types = cols(
                    DataExtractDate = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
                    ReportPeriod = col_datetime(format = "%m/%d/%Y %H:%M:%S %p"),
                    Terminal = col_character(),
                    Arrival_Departure = col_character(),
                    Domestic_International = col_character(),
                    Passenger_Count = col_double()
                  )
)


data2 <- rbind(data2, list(NA, NA, NA, NA, NA, NA))
data2 <- rbind(data2, list('2018-01-01 00:00:00', NA, 'Terminal 2', NA, NA, NA))

#Mozna tez tak:
# data2 <- read_csv('//Los_Angeles_International_Airport_-_Passenger_Traffic_By_Terminal.csv',
#                   col_names = TRUE,
#                   skip = 0,
#                   comment = "#",
#                   na = c("NA", ".", "NOT_AVAILABLE", "66"),
#                   col_types = cols(.default = col_character())
# )
#data2

#Do wczytywania duzych zbiorów danych duzo lepiej sprawdza sie funkcja fread z biblioteki data.table.



#Bardzo wygodna biblioteka do pracy z ramkami danych jest biblioteka dplyr(zawiera sie w tidyverse). Umozliwia ona latwa i intuicyjna manipulacje danych.

#################################
#Wybór zmiennych:
data2 %>%
  select(Terminal, 
         Direction = Arrival_Departure, 
         Type = Domestic_International)
#lub krócej:
data2 %>%
  select(Terminal : Domestic_International)

#dplyr pozwala równiez na wybór zmiennych, które chcemy odrzucic:
data2 %>%
  select(-starts_with("Data")) 


#pozbadzmy sie pierwszej kolumny i zmienmy nazwy kolumny czwartej i piatej:
data2 %>% select(ReportPeriod, 
                          Terminal, 
                          Direction = Arrival_Departure, 
                          Type = Domestic_International, 
                          Passenger_Count)
#lepiej:
data2 <- data2 %>% 
  select(-DataExtractDate) %>%
  rename(Direction = Arrival_Departure,
         Type = Domestic_International)

################################
#filtrowanie:

data2 %>% filter(Terminal == 'Terminal 2')

data2 %>% filter(Passenger_Count > 300000)
data2 %>% filter(ReportPeriod <ymd(20060102))

#Spróbujmy znalezc liczby dla lotów krajowych z terminala drugiego i miedzynarodowych z terminala pierwszego:

data2 %>% filter((Terminal == 'Terminal 2' & Type == 'Domestic') |
                   (Terminal == 'Terminal 1' & Type == 'International'))


################################
#Modyfikacja zmiennych

data2 %>% mutate(
  Passenger_Count = Passenger_Count - 1
)

data2 %>% mutate(
  ReportPeriod = year(ReportPeriod),
  Passenger_Count = Passenger_Count - 1,
  Type = substr(Type, 1, 3),
  Direction = toupper(Direction)
)

################################
#Porzadkowanie:

data2 %>% arrange(desc(Passenger_Count))


################################
#Grupowanie:

data2 %>% summarise(Total = sum(Passenger_Count))

#Wiadomo jak sobie z tym poradzic?

data2 %>% filter(!is.na(Passenger_Count)) %>%
  summarise(Total = sum(Passenger_Count))

#W jakim czasie?

data2 %>% filter(!is.na(Passenger_Count) & !is.na(ReportPeriod)) %>%
  summarise(first_day = min(ReportPeriod),
            last_day = max(ReportPeriod),
            total = sum(Passenger_Count)) %>%
  mutate(days = last_day - first_day)


#A jak to wyglada w zaleznosci od terminala?

data2 %>% filter(!is.na(Passenger_Count) & !is.na(ReportPeriod) &!is.na(Terminal)) %>%
  group_by(Terminal) %>%
  summarise(first_day = min(ReportPeriod),
            last_day = max(ReportPeriod),
            total = sum(Passenger_Count), 
            no_of_reports=n()) %>%
  mutate(days = last_day - first_day) 


#Wygodniejszy sposób na radzenie sobie z brakujacymi danymi w funkcjach agregujacych:

data2 %>%
  group_by(Terminal) %>%
  summarise(first_day = min(ReportPeriod, na.rm = TRUE),
            last_day = max(ReportPeriod, na.rm = TRUE),
            total = sum(Passenger_Count, na.rm = TRUE), 
            no_of_reports=n()) %>%
  mutate(days = last_day - first_day)


#ZADANIE1: znajdz terminal z najwieksza srednia liczba pasazerów na jeden okres raportowania. Funkcja do liczenia sredniej to mean()


#ZADANIE2: Przygotuj zestawienie roczne dla terminali: znajdz laczna liczbe przylotów i odlotów na kazdym terminalu w podziale na lata.



################################
#BONUS gather and spread

data3 <- data2 %>%
  drop_na %>%
  mutate(ReportPeriod = year(ReportPeriod)) %>%
  group_by(ReportPeriod, Terminal, Direction) %>%
  summarise(yearly = sum(Passenger_Count)) %>%
  spread(key = Direction, value = yearly)

data3

data3 %>% gather(Arrival, Departure, key = 'Dir', value = total) %>%
  arrange(ReportPeriod, Terminal)


#USEFUL RESOURCES:
#1. Wickham, Grolemund 'R for Data Science'