library(ggplot2)
library(magrittr)
library(dplyr)
library(ggrepel)
library(stringr)

a <-c('a','a','b','a','c')
data <- as.data.frame(a)
names(data)<-'id'

data <- data %>% group_by(id) %>% summarise('count'=n()) %>% 
  mutate('proportion'= round(count*100/nrow(data), digits = 2))

data %>% ggplot(aes(x=2, y=proportion, fill= id)) +
  geom_bar(alpha=0.7, stat = 'identity',color='white') +
  coord_polar('y', start = 0, direction = 1) +
  scale_fill_brewer(palette = 'Set1', name= 'id') + 
  theme_void()+ xlim(0.5,2.5) + xlab('') +ylab('')+
  geom_label_repel(aes(y = 100-cumsum(proportion) + proportion/2, label=str_c(proportion,'%'))) +
  ggtitle('id') + theme(plot.title = element_text(size=15,face="bold"),
                            legend.title = element_text(face = 'bold'))

