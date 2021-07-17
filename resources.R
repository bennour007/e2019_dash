library(tidyverse)
library(janitor)



data <- readxl::read_xlsx('data/general_results_2019.xlsx')
seats <- readxl::read_xlsx('data/seats.xlsx')

dash_theme <- theme(text = element_text(family = "DejaVu Sans Mono",
                                        color = "black",
                                        margin = margin(t = 10)),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    strip.background = element_blank(),
                    axis.ticks.x  = element_blank(),
                    axis.ticks.y  = element_blank(),
                    plot.background = element_rect(fill = "#f5ffff"),
                    legend.background = element_rect(fill = "#f5ffff"),
                    legend.box.background = element_rect(fill = "#f5ffff", colour = NA),
                    legend.position ="bottom",
                    panel.background = element_rect(fill= "#f5ffff"),
                    plot.margin = margin(10, 100, 10, 100))


zirou <- function(x){
  x %>% 
    replace_na(., 0)
}

# preparing data 

data_clean <- data %>% 
  clean_names() %>% 
  mutate_all(.funs = zirou)

seats <-seats %>% 
  rename('state' = 'Circonscription',
         'n_of_seats' = Sièges)


data_ready <- data_clean %>% 
  pivot_longer(cols = afek_tounes:autres,
               values_to = 'votes',
               names_to = 'party') %>% 
  rename('state'='name_fr') %>% 
  left_join(seats, by = 'state') %>% 
  mutate(party = str_replace_all(party, '_',' '),
         party = str_to_upper(party))


good_data <- function(data, x){
  
  
  data_numbers <- data %>% 
    group_by(state) %>%
    mutate(total_votes = sum(votes),
           percent = votes/total_votes) %>% 
    filter(percent >= x) %>% 
    filter(party != 'AUTRES') %>% 
    mutate(hare_quota = total_votes/n_of_seats,
           party_quota = votes/hare_quota,
           quota_seats = as.integer(party_quota),
           remains = party_quota - quota_seats, 
           remains_seats = 0) %>% 
    mutate(remaining_seat = n_of_seats - sum(quota_seats)) %>% 
    arrange(desc(remains), .by_group = T) %>% 
    ungroup()
  
  remaining_list <- data_numbers %>% 
    select(state, party, remaining_seat) %>% 
    nest_by(state, remaining_seat) %>% 
    mutate(data = map2(data, remaining_seat, function(x,y) rep(x,3)[1:y])) %>% 
    unnest(data) %>% 
    add_column(remains_seats = 0) %>% 
    mutate(remains_seats = remains_seats+1)
  
  good_data_remains <- remaining_list %>% 
    group_by(data) %>% # cna group by state fro mapping later for an anlysis for a state by state 
    summarize(r_seats = sum(remains_seats)) %>% 
    rename('party' = 'data')
  
  
  good_data_quota <- data_numbers %>% 
    group_by(party) %>%
    summarise(q_seats = sum(quota_seats))
  
  
  good_data_all <- good_data_quota %>% 
    left_join(good_data_remains, by = 'party') %>% 
    mutate(r_seats = zirou(r_seats),
           total_seats = q_seats + r_seats) %>% 
    filter(total_seats != 0)
  
  return(good_data_all)
}


# comparaison plot

j_c <- function(per){
  
  good_data(data_ready, 0) %>% 
    left_join(good_data(data_ready, x = per), 
              by = "party") %>% 
    mutate_all(.funs = zirou) %>% 
    rename('initial_quota' = "q_seats.x",
           'new_quota' = "q_seats.y",
           'initial_remains' = "r_seats.x",
           'initial_total' = "total_seats.x",
           'new_remains' = "r_seats.y",
           'new_total' = "total_seats.y") %>% 
    select(- initial_total, -new_total) %>% 
    pivot_longer(cols = initial_quota:new_remains,
                 values_to = 'seats',
                 names_to = 'nature') %>% 
    mutate(comp = if_else(str_detect(nature, 'initial') == T, 'Initial', 'New'),
           # comp = if_else(str_detect(nature, 'quota') == T, 'Quota', comp),
           nature = str_remove_all(nature, 'initial_'),
           nature = str_remove_all(nature, 'new_')) %>% 
    mutate(party = fct_lump(party, w = seats, 7)) %>%
    mutate(party = fct_reorder(party, seats)) %>%
    ggplot() +
    geom_col(aes(x = seats,
                 y = party,
                 fill = nature)
    ) +
    scale_fill_manual(values=c("skyblue", "#3a5f63"), 
                      name="Decomposition:",
                      breaks=c("quota", "remains"),
                      labels=c("Direct votes seats(quota)", "Remains seats"))+
    facet_grid(~ comp) +
    labs(title = 'How the total aquired seats are decomposed between direct votes and largest remains:')+
    dash_theme %>% 
    return()
}





















