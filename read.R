
# prereq ------------------------------------------------------------------

require(xml2)
require(tidyverse)

dir <- 'data/'


fol <- 'data/Heiz.task1.out.xml'


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
  filter(mod_param != 'L') %>% 
  ggplot(aes(mod_value,value)) +
  geom_line() +
  geom_point() +
  facet_grid(model~parameter) +
  theme_bw()




