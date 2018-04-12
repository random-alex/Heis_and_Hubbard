
# prereq ------------------------------------------------------------------

require(xml2)
require(tidyverse)

dir <- 'data/chain_8_large/'


fol <- 'data/periodic_2/Heiz_periodic_2.task1.out.xml'




# functions ---------------------------------------------------------------

my_read_xml <- function(fol) {
  df <- read_xml(fol)
  par <- xml_find_all(df,'.//PARAMETER') 
  df_param <- tibble(mod_param = xml_attr(par,'name'),
                     mod_value  = xml_text(par))%>% 
    filter(mod_param %in% c('J','L','U'))
  dat <- xml_find_all(df,'.//AVERAGES//SCALAR_AVERAGE') 
  df_data <- tibble(parameter = xml_attr(dat,'name'),
                    value  = xml_text(dat))
  df_param <- rbind(df_param,df_param) %>% 
    arrange(mod_param)
  df_res <- bind_cols(df_param,rbind(df_data,df_data))
  return(df_res)
}


# read file ---------------------------------------------------------------



df <- tibble(dirs = list.files(dir,pattern = 'task[0-9]{1}\\.out\\.xml|[0-9]{2}\\.out\\.xml',full.names = T)) %>% 
  mutate(model = str_extract_all(dirs,'Heiz|Hubbard',simplify = T)[,1],
         data = map(dirs,my_read_xml)) %>% 
  unnest() %>% 
  select(-dirs) %>% 
  mutate(value = as.numeric(value))


df %>% 
  filter(mod_param != 'L'& parameter != 'Ground State Energy') %>% 
  mutate(mod_value = ifelse(model == 'Heiz',
                            as.numeric(4/as.numeric(mod_value)),
                            as.numeric(mod_value))) %>% 
  ggplot(aes(mod_value,value,col = model)) +
  geom_line() +
  geom_point() +
  facet_grid(parameter ~ .) +
  labs(y = 'Energy Gap', x = 'U and J') +
  # scale_y_log10() +
  ggtitle('Comparisson Heisenberg and Hubbard models for chain lattice with L = 8 ') +
  theme_bw()



df %>% 
  filter(mod_param != 'L'& parameter != 'Ground State Energy') %>% 
  mutate(mod_value = ifelse(model == 'Heiz',
                            round(as.numeric(4/as.numeric(mod_value))),
                            as.numeric(mod_value))) %>% 
  select(-mod_param) %>% 
  spread(model,value) %>% 
  ggplot(aes(mod_value,(Heiz - Hubbard)/Hubbard)) +
  geom_line() +
  geom_hline(aes(yintercept = 0)) +
  geom_point() +
  facet_grid(parameter ~ .) +
  ggtitle('The drawn values are obtained using this formula: (Heiz - Hubbard)/Hubbard') +
  labs(y = 'Energy Gap', x = 'U and J') +
  theme_bw()


  
#plot for square lattice 2 
df %>% 
  filter(mod_param != 'L'& parameter != 'Ground State Energy') %>% 
  mutate(mod_value = ifelse(model == 'Heiz',
                            as.numeric(8/as.numeric(mod_value)),
                            as.numeric(mod_value))) %>% 
  ggplot(aes(mod_value,value,col = model)) +
  geom_line(position = position_dodge(10)) +
  geom_point(position = position_dodge(10)) +
  facet_grid(parameter ~ .) +
  labs(y = 'Energy Gap', x = 'U and J') +
  # scale_y_log10() +
  theme_bw()


x <- seq(1,70,by = 4)

paste0(round(x,4),collapse = ' , ')
paste0(round(4/x,4),collapse = ' , ')




dd <- df %>% 
  filter(mod_param != 'L'& parameter != 'Ground State Energy') %>% 
  mutate(mod_value = ifelse(model == 'Heiz',
                            round(as.numeric(4/as.numeric(mod_value))),
                            as.numeric(mod_value)))
