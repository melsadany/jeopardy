################################################################################
#                  correlation between guessings and other features            #
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
scores <- readxl::read_xlsx("data/raw/money_shows_metadata.xlsx", sheet = 1)[-1,]
scores.avg <- scores %>%
  group_by(name) %>%
  dplyr::summarise(avg_score = as.numeric(all_time_winnings)/n()) %>% distinct() %>%
  left_join(scores %>% select(PID, name,sex))
################################################################################
# read guessings spreadsheet
guess <- readxl::read_xlsx("data/raw/money_shows_metadata.xlsx", sheet = 2)
guess.age <- guess[-c(1:2),] %>%
  select(PID, name, sex, age_1=6, age_2=7, age_3=8, age_4=9, age_5=10) %>%
  drop_na()  %>%
  mutate_at(.vars = vars(starts_with("age")), .funs = function(x) as.numeric(x)) %>%
  group_by(PID, name) %>%
  dplyr::summarise(avg_age = mean(c_across(starts_with("age"))))

guess.eth <- guess[-c(1:2),] %>%
  select(PID, name, sex, eth_1=11, eth_2=12, eth_3=13, eth_4=14, eth_5=15) %>%
  drop_na()
guess.eth <- guess.eth %>%
  select(PID, name, sex) %>%
  mutate(avg_eth = apply(guess.eth[, 4:8], MARGIN=1, function(x) DescTools::Mode(x)))

guess.weight <- guess[-c(1:2),] %>%
  select(PID, name, sex, w_1=16, w_2=17, w_3=18, w_4=19, w_5=20) %>%
  drop_na()
guess.weight <- guess.weight %>%
  select(PID, name, sex) %>%
  mutate(avg_w = apply(guess.weight[, 4:8], MARGIN=1, function(x) DescTools::Mode(x)[1]))

# combine all

guess.avg <- inner_join(guess.age, inner_join(guess.eth, guess.weight))
################################################################################
df1 <- inner_join(scores.avg,
                  inner_join(guess.avg, guess %>% select(PID, smiling, teeth, eyes, dress))) %>% filter(avg_score <=100000)
p1 <-df1 %>%
  ggplot(aes(x=avg_age, y=avg_score)) +
  geom_point() +
  geom_smooth(method="lm") +
  ggpubr::stat_cor(color="red")
p2 <-df1 %>%
  ggplot(aes(x=avg_eth, y=avg_score)) +
  geom_violin() +geom_boxplot(fill="white", width=0.2) +
  ggpubr::stat_compare_means(color="red")
p3 <-df1 %>%
  ggplot(aes(x=avg_w, y=avg_score)) +
  geom_violin() +geom_boxplot(fill="white", width=0.2) +
  ggpubr::stat_compare_means(color="red")
p4 <-df1 %>%
  mutate(smiling=toupper(smiling)) %>%
  ggplot(aes(x=smiling, y=avg_score)) +
  geom_violin() +geom_boxplot(fill="white", width=0.2) +
  ggpubr::stat_compare_means(color="red")
p5 <-df1 %>%
  ggplot(aes(x=teeth, y=avg_score)) +
  geom_violin() +geom_boxplot(fill="white", width=0.2) +
  ggpubr::stat_compare_means(color="red")
p6 <-df1 %>%
  ggplot(aes(x=eyes, y=avg_score)) +
  geom_violin() +geom_boxplot(fill="white", width=0.2) +
  ggpubr::stat_compare_means(color="red")
p7 <-df1 %>%
  ggplot(aes(x=dress, y=avg_score)) +
  geom_violin() +geom_boxplot(fill="white", width=0.2) +
  ggpubr::stat_compare_means(color="red")
patchwork::wrap_plots(p1,p2,p3,p4,p5,p6,p7)
ggsave("figs/corr-of-judgements-w-score-less-than-100k.png", bg="white", 
       width = 10, height = 10, units = "in", dpi=360)
################################################################################
# get facial areas
areas <- read_csv("data/derivatives/facial.areas.csv") %>%
  select(PID=te_id, starts_with("A_")) %>% # only keep distances of int
  mutate(PID = sub("\\.png", "", PID)) %>%
  mutate(A_M = A_M_R+A_M_L) %>% select(-"A_M_R", -"A_M_L")%>%
  mutate(A_N = A_N_R+A_N_L) %>% select(-"A_N_R", -"A_N_L")%>%
  mutate(A_CHK_R = A_CHK_I_R+A_CHK_O_R) %>% select(-"A_CHK_I_R", -"A_CHK_O_R")%>%
  mutate(A_CHK_L = A_CHK_I_L+A_CHK_O_L) %>% select(-"A_CHK_I_L", -"A_CHK_O_L")%>%
  left_join(scores.avg) %>%
  distinct(name, .keep_all = T) %>%
  mutate(PID = ifelse(PID=="J121", "J119", PID))
# corr of age/sex/others with facial areas
df2 <- inner_join(areas,
                  inner_join(guess.avg, 
                             guess %>% select(PID, smiling, teeth, eyes, dress))) %>%
  filter(avg_score <= 100000)
df2 %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area") %>%
  ggplot(aes(x=avg_age, y=value)) +
  geom_point()+
  geom_smooth(method="lm") +
  ggpubr::stat_cor(color="red") +
  facet_wrap("area", scales = "free")
ggsave("figs/corr-of-judged-age-w-face-areas-less-than-100k.png", bg="white", 
       width = 10, height = 10, units = "in", dpi=360)

df2 %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area") %>%
  pivot_longer(cols = c("smiling", "teeth", "eyes", "dress", "dress", "avg_eth", "avg_w", "sex"), names_to = "var2", values_to = "val2") %>%
  mutate(val2 = toupper(val2)) %>%
  ggplot(aes(x=val2, y=value)) +
  geom_violin() + geom_boxplot(fill="white", width=0.2) +
  ggpubr::stat_compare_means(color="red")+
  ggh4x::facet_grid2(rows = vars(area), cols = vars(var2), scales = "free", axes = T, remove_labels = T)
ggsave("figs/corr-of-judgements-w-face-areas-less-than-100k.png", bg="white", 
       width = 12, height = 19, units = "in", dpi=360)
df2 %>%
  pivot_longer(cols = c("smiling", "teeth", "eyes", "dress", "dress", "avg_eth", "avg_w", "sex"), names_to = "var2", values_to = "val2") %>%
  mutate(val2 = toupper(val2)) %>%
  group_by(var2, val2) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(x=val2, y=count))+
  geom_bar(stat = "identity")+
  facet_wrap(~var2, scales = "free")
ggsave("figs/dist-of-judgements-less-than-100k.png", bg="white", 
       width = 8, height = 8, units = "in", dpi=360)
################################################################################
################################################################################
################################################################################
# correct for age, sex, interaction, eth, weight, smiling, teeth
res.areas <- cbind(PID= df2$PID,
                   apply(df2 %>% select(starts_with("A_")), MARGIN = 2, FUN = function(x) {
                     residuals(glm(y ~ avg_age + sex + avg_age:sex + avg_eth + avg_w + smiling + teeth + eyes, 
                                   data = cbind(df2 %>% select(avg_age,sex,avg_eth,avg_w,smiling,teeth,eyes) %>%
                                                  mutate(sex = as.factor(sex)),
                                                y = x)))
                   }) %>%
                     as.data.frame())
# corr res areas with avg_score
res.areas %>% 
  left_join(scores.avg) %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area") %>%
  ggplot(aes(x=avg_score, y=value))+
  geom_point()+
  geom_smooth(method="lm") +
  ggpubr::stat_cor(color="red") +
  facet_wrap("area", scales = "free")+
  labs(caption = paste0("areas were residualized:", "\n",
                        "   residuals(glm(y ~ avg_age + sex + avg_age:sex + avg_eth + avg_w + smiling + teeth + eyes))"))
ggsave("figs/corr_residualized-facial-areas-and-avg_score-less-than-100k.png", bg="white", 
       width = 10, height = 8, units = "in", dpi=360)
################################################################################
# build a RF to predict acore from measurements
library(randomForest)
all.one <- inner_join(scores.avg, 
                      res.areas) %>%
  filter(avg_score <= 100000) %>% select(-PID, -name) %>% ungroup()
# train to predict score
rf <- randomForest::randomForest(x = all.one %>% select(starts_with("A_")),
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
