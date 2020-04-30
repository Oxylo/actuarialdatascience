library(readxl)
library(tidyverse)
 
xlswb <- "travel insurance.xlsx"
premium <- read_excel(xlswb, sheet="premium")
commission <- read_excel(xlswb, sheet="commission")

outp <- premium %>% 
  filter(channel=="Online") %>%
  merge(commission) %>%
  mutate(commission=pct_commission * premium) %>%
  group_by(agent) %>%
  summarise(tot.commission=sum(commission)) %>%
  arrange(tot.commission)
  
ggplot(outp, aes(x=reorder(agent, tot.commission), y=tot.commission)) +
  geom_bar(stat="identity", fill="blue") +
  coord_flip() +
  ggtitle("Commission per Agent") +
  labs(x="agent", y="USD")
