librarian::shelf(ggplot2, tidyverse, dplyr, vegan)

# chargement des données
codes <- read.csv(file = "data/raw-data/codes_ech.csv", h = T, sep = ";")
fungi0 <- read.csv(file = "data/raw-data/fungi.csv", h = T, sep = ";")

fungi <- fungi0 %>%
  group_by(Kingdom, Phylum, Class, Order, Family) %>%
  summarise(across(matches("MET"), sum)) %>%
  filter(!Kingdom %in%  "no data",
         !Family %in% c("unidentified", "Multi-affiliation")) %>%
  pivot_longer(cols = starts_with("MET"),
               names_to = "code",
               values_to = "Abundance") %>%
  ungroup() %>%
  select(Family, code, Abundance) %>%
  pivot_wider(names_from = Family,
              values_from = Abundance,
              values_fill = 0)%>%
  mutate(code = sub("_.*", "", code)) %>%
  left_join(codes) %>%
  relocate(141:149, .after = code) %>%
  filter(!is.na(os))

fungi_comm <- fungi %>%
  select(where(is.numeric), -c(1:11)) 
         
fungi$H <- diversity(fungi_comm, index = "shannon")
fungi$FR <- rowSums(fungi_comm > 0)


ggplot(fungi, aes(x = Profondeur, y = FR, fill = Traitement))+
  geom_boxplot()+
  facet_grid(os~Annee)
         