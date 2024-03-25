################################################################################
#                  correlation between guessings and other features            #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
# best guess function per row
rowMode_char <- function(mat) {
  apply(mat, 1, function(x) {
    tbl <- table(x)
    mode <- names(tbl)[which.max(tbl)]
    if (length(mode) == 0) {
      return(NA_character_)
    } else {
      return(mode)
    }
  })
}
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/jeopardy"
setwd(project.dir)
################################################################################
# read scores 
scores.avg <- read_rds("data/derivatives/jeopardata/scores-avg.rds")
################################################################################
# read guessings spreadsheet
guess <- readxl::read_xlsx("data/raw/money_shows_metadata.xlsx", sheet = 5)
guess.age <- guess[-c(1:2),] %>%
  select(JPID=ID2, name, sex, age_1=5, age_2=6, age_3=7, age_4=8, age_5=9) %>%
  # drop_na()  %>%
  mutate_at(.vars = vars(starts_with("age")), .funs = function(x) as.numeric(x)) %>%
  group_by(JPID, name) %>%
  dplyr::summarise(avg_age = mean(c_across(starts_with("age")), na.rm = T))
guess.eth <- guess[-c(1:2),] %>%
  select(JPID=ID2, name, sex, eth_1=10, eth_2=11, eth_3=12, eth_4=13, eth_5=14) 
guess.eth <- guess.eth %>%
  select(JPID, name, sex) %>%
  mutate(avg_eth = rowMode_char(as.matrix(guess.eth[, 4:7])))

guess.weight <- guess[-c(1:2),] %>%
  select(JPID=ID2, name, sex, w_1=15, w_2=16, w_3=17, w_4=18, w_5=19) %>%
  mutate(w_1 = ifelse(w_1=="I", "IS", w_1))
guess.weight <- guess.weight %>%
  select(JPID, name, sex) %>%
  mutate(avg_w = rowMode_char(as.matrix(guess.weight[, 4:7])))

# combine all
guess.avg <- inner_join(guess.age, inner_join(guess.eth, guess.weight))
write_rds(guess.avg, "data/derivatives/jeopardata/guess-avg.rds")
################################################################################
df1 <- inner_join(scores.avg,
                  inner_join(guess.avg, 
                             guess %>% select(JPID=ID2, smiling, teeth, eyes, dress))) 
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
ggsave("figs/jeopardata/corr-of-judgements-w-score.png", bg="white", 
       width = 10, height = 10, units = "in", dpi=360)
################################################################################
################################################################################
# build meta data for guessings and scores average
meta <- full_join(scores.avg, guess.avg)
write_csv(meta, "data/derivatives/jeopardata/participants-metadata.csv")
################################################################################
################################################################################
# get facial areas
areas <- read_csv("data/derivatives/jeopardata/facial.areas.csv") %>%
  select(JPID, starts_with("A_")) %>% # only keep distances of int
  mutate(JPID = sub("\\.png", "", JPID)) %>%
  # filter(!(JPID %in% c("JD0886", "JD0405]", "JD0700", "JD0622"))) %>%
  mutate(A_M = A_M_R+A_M_L) %>% select(-"A_M_R", -"A_M_L")%>%
  mutate(A_N = A_N_R+A_N_L) %>% select(-"A_N_R", -"A_N_L")%>%
  mutate(A_CHK_R = A_CHK_I_R+A_CHK_O_R) %>% select(-"A_CHK_I_R", -"A_CHK_O_R")%>%
  mutate(A_CHK_L = A_CHK_I_L+A_CHK_O_L) %>% select(-"A_CHK_I_L", -"A_CHK_O_L")%>%
  pivot_longer(cols = starts_with("A_"), names_to = "area") %>%
  # mutate(area = sub("_[R|L]", "", area)) %>% group_by(JPID, area) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup() %>%
  pivot_wider(names_from = "area", values_from = "value", id_cols = JPID) %>%
  full_join(meta) %>%
  drop_na() %>%
  distinct(name, .keep_all = T) 
# corr of age/sex/others with facial areas
df2 <- inner_join(areas,
                  inner_join(guess.avg, 
                             guess %>% select(JPID=ID2, smiling, teeth, eyes, dress)))
df2 %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area") %>%
  ggplot(aes(x=avg_age, y=value)) +
  geom_point()+
  geom_smooth(method="lm") +
  ggpubr::stat_cor(color="red") +
  facet_wrap("area", scales = "free")
ggsave("figs/jeopardata/corr-of-judged-age-w-face-areas.png", bg="white", 
       width = 10, height = 10, units = "in", dpi=360)

df2 %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area") %>%
  pivot_longer(cols = c("smiling", "teeth", "eyes", "dress", "avg_eth", "avg_w", "sex"), names_to = "var2", values_to = "val2") %>%
  mutate(val2 = toupper(val2)) %>%
  ggplot(aes(x=val2, y=value, fill = val2)) +
  geom_violin() + geom_boxplot(fill="white", width=0.2) +
  ggpubr::stat_compare_means(color="red")+
  ggh4x::facet_grid2(rows = vars(area), cols = vars(var2), scales = "free", axes = T, remove_labels = T)
ggsave("figs/jeopardata/corr-of-judgements-w-face-areas.png", bg="white", 
       width = 12, height = 19, units = "in", dpi=360)
df2 %>%
  pivot_longer(cols = c("smiling", "teeth", "eyes", "dress", "dress", "avg_eth", "avg_w", "sex"), names_to = "var2", values_to = "val2") %>%
  mutate(val2 = toupper(val2)) %>%
  group_by(var2, val2) %>%
  dplyr::summarise(count=n()) %>%
  ggplot(aes(x=val2, y=count))+
  geom_bar(stat = "identity")+
  facet_wrap(~var2, scales = "free")
ggsave("figs/jeopardata/dist-of-judgements.png", bg="white", 
       width = 8, height = 8, units = "in", dpi=360)
################################################################################
################################################################################
################################################################################
# correct for age, sex, interaction, eth, weight, smiling, teeth
res.areas <- cbind(JPID= df2$JPID,
                   apply(df2 %>% select(starts_with("A_")), MARGIN = 2, FUN = function(x) {
                     residuals(glm(y ~ avg_age + sex + avg_age:sex + avg_eth + avg_w, 
                                   data = cbind(df2 %>% select(avg_age,sex,avg_eth,avg_w,smiling,teeth,eyes) %>%
                                                  mutate(sex = as.factor(sex)),
                                                y = x)))
                   }) %>%
                     as.data.frame())
# corr res areas with avg_score
res.areas %>% 
  left_join(meta) %>%
  filter(avg_score < 20000) %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area") %>%
  # mutate(area = sub("_[R|L]", "", area)) %>% group_by(JPID, area) %>% mutate(value = mean(value, na.rm = T)) %>%
  pivot_longer(cols = colnames(meta)[2:8], names_to = "avg_measure", values_to = "avg") %>%
  ggplot(aes(x=avg, y=value))+
  geom_point()+
  geom_smooth(method="lm") +
  ggpubr::stat_cor(color="red") +
  ggh4x::facet_grid2(cols = vars(avg_measure), rows = vars(area), scales = "free")+
  theme(strip.text.y.right = element_text(angle = 0))+
  labs(caption = paste0("areas were residualized:", "\n",
                        "   residuals(glm(y ~ avg_age + sex + avg_age:sex + avg_eth + avg_w + smiling + teeth + eyes))"))
ggsave("figs/jeopardata/corr_residualized-facial-areas-and-avg_score.png", bg="white", 
       width = 14, height = 20, units = "in", dpi=360)
################################################################################
################################################################################
