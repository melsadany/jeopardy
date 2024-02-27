################################################################################
#              correlation between facial landmarks and other features         #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/jeopardy"
setwd(project.dir)
################################################################################
# read scores 
scores <- readxl::read_xlsx("data/raw/money_shows_metadata.xlsx", sheet = 3)[-1,] %>%
  mutate(name = tolower(name))
scores.avg <- scores %>%
  separate(cor_incorrect, into = c("correct_count", "incorrect_count"),
           sep = "/") %>%
  mutate_at(.vars = c(7:12,14), .funs = function(x) as.numeric(parse_number(x))) %>%
  drop_na(final_score) %>%
  filter(BUZ<=60, ATT<=60) %>%
  distinct() %>%
  select(name, final_score, ATT, BUZ, correct_count, incorrect_count) %>%
  group_by(name) %>%
  dplyr::summarise(avg_score = mean(final_score, na.rm = T),
                   avg_ATT = mean(ATT, na.rm = T),
                   avg_BUZ = mean(BUZ, na.rm = T),
                   avg_corr = mean(correct_count, na.rm = T),
                   avg_incorr = mean(incorrect_count, na.rm = T)) %>% 
  distinct()
# save
write_rds(scores.avg, "data/derivatives/jeopardata/scores-avg.rds")
##
scores.avg %>%
  pivot_longer(cols = colnames(scores.avg)[2:6], names_to = "score") %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 40) +
  facet_wrap(~score, scales = "free")
ggsave("figs/distribution-of-avg-score.png", bg = "white",
       width = 6, height = 6, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
############################ correlation with distances ########################
################################################################################
################################################################################
# get pairs distances
int.pairs <- c("EB_R", "EB_L", "E_R", "E_L", "M_H", "N_V", "N_H", "M_V", "EB_C","EB_N_R", "EB_N_L", "EB_E_R", "EB_E_L","NT_E_R", "NT_E_L","EB_M_R", "EB_M_L","E_M_R", "E_M_L")
pairs.dis <- read_csv("data/derivatives/pairs-distances.csv") %>%
  select(PID=te_id, paste0("P_", int.pairs)) %>% # only keep distances of int
  mutate(PID = sub("\\.png", "", PID)) %>%
  left_join(scores.avg) %>%
  distinct(name, .keep_all = T)

# plot correlation bet distances and money
pairs.dis %>%
  filter(avg_score <= 100000) %>%
  pivot_longer(cols = starts_with("P_"), names_to = "pair", values_to = "v1") %>%
  ggplot(aes(x=avg_score, y=v1)) +
  geom_point()+geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red")+
  facet_wrap(~pair, scales = "free") + 
  labs(y="measured distance", caption = paste0("n(samples<=100,000): ", 
                                               nrow(pairs.dis %>% filter(avg_score <= 100000))))
ggsave("figs/corr_facial-distances-and-avg_score-less-than-100k.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
################################################################################
################################################################################

################################################################################
################################################################################
############################## correlation with areas ##########################
################################################################################
################################################################################
# get facial areas
areas <- read_csv("data/derivatives/facial.areas.csv") %>%
  select(PID=te_id, starts_with("A_")) %>% # only keep distances of int
  mutate(PID = sub("\\.png", "", PID)) %>%
  left_join(scores.avg) %>%
  distinct(name, .keep_all = T)

# plot correlation bet areas and avg score
areas %>%
  filter(avg_score <= 100000) %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area", values_to = "v1") %>%
  ggplot(aes(x=avg_score, y=v1)) +
  geom_point()+ geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red")+
  facet_wrap(~area, scales = "free") + 
  labs(y="measured distance", caption = paste0("n(samples<=100,000): ", 
                                               nrow(pairs.dis %>% filter(avg_score <= 100000))))
ggsave("figs/corr_facial-areas-and-avg_score-less-than-100k.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
################################################################################
################################################################################
# build a RF to predict acore from measurements
library(randomForest)
all.one <- inner_join(scores.avg, 
                      inner_join(pairs.dis, areas)) %>%
  filter(avg_score <= 100000) %>% select(-PID, -name) %>% ungroup()
# train to predict score
rf <- randomForest::randomForest(x = all.one %>% select(starts_with("P_"),
                                                        starts_with("A_")),
                                 y = all.one$avg_score,
                                 nodesize=5, nPerm = 100, ntree = 800)
p1 <- cbind(y=rf$y, predicted=rf$predicted) %>% as.data.frame() %>%
  ggplot(aes(x=y, y=predicted))+geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color="red")+
  labs(title = "RF prediction for avg_score")
p2 <- rf$importance %>% as.data.frame() %>% rownames_to_column("var") %>%
  ggplot(aes(x=IncNodePurity, y=reorder(var, desc(IncNodePurity))))+geom_point()+labs(y="")
p1+p2
ggsave("figs/RF-predict-avg_score-less-than-100k.png", bg = "white",
       width = 10, height = 8, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# correlation between measured areas and distances from faces against faces2
pairs.dis1 <- read_csv("data/derivatives/pairs-distances.csv") %>%
  select(PID=te_id, paste0("P_", int.pairs)) %>% # only keep distances of int
  mutate(PID = sub("\\.png", "", PID))
pairs.dis2 <- read_csv("data/derivatives/pairs-distances2.csv") %>%
  select(PID=te_id, paste0("P_", int.pairs)) %>% # only keep distances of int
  mutate(PID = sub("\\.png", "", PID),
         PID = sub("\\.jpeg", "", PID),
         PID = sub("\\.jpg", "", PID)) %>%
  left_join(scores.avg) %>%
  distinct(name, .keep_all = T)
areas1 <- read_csv("data/derivatives/facial.areas.csv") %>%
  select(PID=te_id, starts_with("A_")) %>% # only keep distances of int
  mutate(PID = sub("\\.png", "", PID))
areas2 <- read_csv("data/derivatives/facial.areas2.csv") %>%
  select(PID=te_id, starts_with("A_")) %>% # only keep distances of int
  mutate(PID = sub("\\.png", "", PID),
         PID = sub("\\.jpeg", "", PID),
         PID = sub("\\.jpg", "", PID)) %>%
  left_join(scores.avg) %>%
  distinct(name, .keep_all = T)
# combine areas and areas2
# 
areas.all <- inner_join(areas1 %>% rename_at(.vars = vars(starts_with("A_")), .funs = function(x) sub("A_", "A1_", x)),
                        areas2 %>% rename_at(.vars = vars(starts_with("A_")), .funs = function(x) sub("A_", "A2_", x)))
p10 <- areas.all %>% 
  pivot_longer(cols = starts_with("A1"), names_to = "area1", values_to = "val1") %>%
  pivot_longer(cols = starts_with("A2"), names_to = "area2", values_to = "val2") %>%
  mutate(A1 = sub("A1_", "", area1),
         A2 = sub("A2_", "", area2)) %>%
  filter(A1==A2) %>%
  ggplot(aes(x=val1, y=val2)) +
  geom_point(size=2) + geom_smooth(method = "lm") + ggpubr::stat_cor(color="red") +
  facet_wrap(~A1, scales = "free") +
  labs(title = "correlation between measured areas from set1 VS. set2",
       caption = paste0("n(samples): ", nrow(areas.all), "\n",
                        "set 1 (x-axis) is the face pictures from the Jeopardy game website.", "\n",
                        "set 2 (y-axis) is from google search."))
p11 <- areas.all %>% 
  filter(avg_score <= 100000) %>%
  pivot_longer(cols = starts_with("A1"), names_to = "area1", values_to = "val1") %>%
  pivot_longer(cols = starts_with("A2"), names_to = "area2", values_to = "val2") %>%
  mutate(A1 = sub("A1_", "", area1),
         A2 = sub("A2_", "", area2)) %>%
  filter(A1==A2) %>%
  ggplot(aes(x=val1, y=val2)) +
  geom_point(size=2) + geom_smooth(method = "lm") + ggpubr::stat_cor(color="red") +
  facet_wrap(~A1, scales = "free") +
  labs(title = "correlation between measured areas from set1 VS. set2",
       caption = paste0("n(samples): ", nrow(areas.all %>% filter(avg_score<=100000)), "\n",
                        "samples were filtered to only keep the ones with avg_winnings of <= $100,000", "\n",
                        "set 1 (x-axis) is the face pictures from the Jeopardy game website.", "\n",
                        "set 2 (y-axis) is from google search."))
ggsave(p10+p11, filename = "figs/corr-of-facial-areas-from-set1-to-set2.png", bg="white",
       width = 18, height = 10, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################


################################################################################


################################################################################


################################################################################