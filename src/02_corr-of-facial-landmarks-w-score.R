################################################################################
################################################################################
rm(list = ls());gc()
device <- ifelse(grepl("/LSS/", system("cd &pwd", intern = T)), "IDAS", "argon")
source(paste0(ifelse(device == "IDAS", "~/LSS", "/Dedicated"),"/jmichaelson-wdata/msmuhammad/msmuhammad-source.R"))
################################################################################
################################################################################
project.dir <- correct_path("/Dedicated/jmichaelson-wdata/msmuhammad/projects/jeopardy")
setwd(project.dir)
################################################################################
################################################################################
facial.labels <- read_rds("../RPOE/photographs/data/derivatives/facial-meta.rds")
################################################################################
################################################################################
# read scores 
scores <- readxl::read_xlsx("data/raw/money_shows_metadata.xlsx", sheet = 1)[-1,] %>% 
  mutate(winnings=as.numeric(winnings), all_time_winnings = as.numeric(all_time_winnings))
scores.avg <- scores %>% group_by(name) %>% mutate(use = mean(all_time_winnings)) %>%
  filter(use <= 100000) %>% select(PID, name, sex, use)
scores.avg$norm_score <- qnorm((rank(scores.avg$use,na.last="keep")-0.5)/sum(!is.na(scores.avg$use)))
scores.avg %>% 
  ggplot(aes(norm_score)) +
  geom_histogram(bins = 40) + bw.theme + labs(x="normalized total winnings")
ggsave2("figs/distribution-of-avg-score.png", 4,3)
scores.avg %>%
  filter(sex %in% c("M", "F")) %>%
  ggplot(aes(x=sex, y=norm_score, fill=sex))+
  geom_violin(show.legend = F)+geom_boxplot(width=0.2, fill="white")+
  ggpubr::stat_compare_means()+ bw.theme + scale_fill_manual(values = palette.1)+
  labs(y="normalized total winnings")
ggsave2("figs/corr-of-sex-with-avg-score.png", 3,4)
################################################################################
################################################################################
################################################################################
################################################################################
## get facial data
# get pairs distances
int.pairs <- c("EB_R", "EB_L", "E_R", "E_L", "M_H", "N_V", "N_H", "M_V", "EB_C","EB_N_R", "EB_N_L", "EB_E_R", "EB_E_L","NT_E_R", "NT_E_L","EB_M_R", "EB_M_L","E_M_R", "E_M_L")
pairs.dis <- read_csv("data/derivatives/pairs-distances.csv") %>%
  select(PID=te_id, paste0("P_", int.pairs)) %>% # only keep distances of int
  mutate(PID = sub("\\.png", "", PID)) %>%
  left_join(scores.avg) %>%
  distinct(name, .keep_all = T)
# get facial areas
areas <- read_csv("data/derivatives/facial.areas.csv") %>%
  select(PID=te_id, starts_with("A_")) %>% # only keep distances of int
  mutate(PID = sub("\\.png", "", PID)) %>%
  left_join(scores.avg) %>%
  distinct(name, .keep_all = T) %>%
  mutate(A_all = rowSums(across(starts_with("A_")))) %>%
  mutate(A_N = A_N_R + A_N_L, A_M = A_M_R + A_M_L,
         A_CHK_R = A_CHK_I_R + A_CHK_O_R, A_CHK_L = A_CHK_I_L + A_CHK_O_L) %>%
  select(-c(A_N_R, A_N_L, A_M_R, A_M_L, A_CHK_I_R, A_CHK_O_R, A_CHK_I_L, A_CHK_O_L)) %>%
  mutate(A_asym = rowSums(across(ends_with("_R")))-rowSums(across(ends_with("_L"))))



## normalize for sex
res.pairs <- cbind(PID = pairs.dis$PID,
                   apply(pairs.dis %>% select(starts_with("P_")), MARGIN = 2, FUN = function(x) {
                     df <- cbind(pairs.dis %>% select(sex) %>% mutate(sex = as.factor(sex)), y = x) %>%
                       select(y,sex)
                     z_from_lm(y = df$y, x = df[,-1])
                   }) %>% as.data.frame())
summary(res.pairs)
## normalize for sex and total area
res.areas <- cbind(areas %>% select(PID, A_all),
                   apply(areas %>% select(c(2:5,11:15)), MARGIN = 2, FUN = function(x) {
                     df <- cbind(areas %>% select(sex, A_all) %>% mutate(sex = as.factor(sex)), y = x) %>%
                       select(y, sex)
                     z_from_lm(y = df$y, x = df[,-1])
                   }) %>%
                     as.data.frame()) %>%
  drop_na()
summary(res.areas)

################################################################################
################################################################################
################################################################################
m123 <- inner_join(res.pairs, res.areas) %>% inner_join(scores.avg) %>%
  select(colnames(scores.avg), starts_with("P_"), starts_with("A_")) %>% select(-name)
m123 %>% pivot_longer(cols = c(5:23)) %>%
  inner_join(facial.labels %>% mutate(P_label = paste0(meaning, ifelse(!side%in%c("","general"), paste0(" (", side,")"),""))) %>% 
               select(name=label, P_label)) %>% mutate(P_label = ifelse(P_label=="face", "full face", P_label)) %>%
  ggplot(aes(value,norm_score))+
  geom_point(shape=1)+geom_smooth(method = "lm",color=palette.1[2],se=F) +
  ci_ribbon1+ ggpubr::stat_cor(color = "red")+
  facet_wrap(~P_label, scales = "free") + labs(x="measured distance", y="normalized total winnings") + bw.theme +
  labs(caption = paste0("n(samples): ", nrow(m123),"\ncorrected for sex"))
ggsave2("figs/distances-to-winnings-sex-corrected.png",16,10)

m123 %>% pivot_longer(cols = c(24:33)) %>% filter(name!="A_asym") %>% 
  inner_join(facial.labels %>% mutate(A_label = paste0(meaning, ifelse(!side%in%c("","general"), paste0(" (", side,")"),""))) %>% 
               select(name=label, A_label)) %>% mutate(A_label = ifelse(A_label=="face", "full face", A_label)) %>%
  ggplot(aes(value,norm_score))+
  geom_point(shape=1)+geom_smooth(method = "lm",color=palette.1[2],se=F) + 
  ci_ribbon1+ ggpubr::stat_cor(color = "red")+
  facet_wrap(~A_label, scales = "free") + labs(x="measured area", y="normalized total winnings") + bw.theme +
  labs(caption = paste0("n(samples): ", nrow(m123),"\ncorrected for sex"))
ggsave2("figs/areas-to-winnings-sex-corrected.png",10,8)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# correlation between measured areas from face pictures of the website against faces pictures from google search
areas.google <- read_csv("data/derivatives/facial.areas2.csv") %>%
  select(PID=te_id, starts_with("A_")) %>%
  mutate(PID = sub("\\.png|\\.jpeg|\\.jpg", "", PID)) %>%
  left_join(scores.avg) %>% distinct(name, .keep_all = T) %>%
  mutate(A_all = rowSums(across(starts_with("A_")))) %>%
  mutate(A_N = A_N_R + A_N_L, A_M = A_M_R + A_M_L,
         A_CHK_R = A_CHK_I_R + A_CHK_O_R, A_CHK_L = A_CHK_I_L + A_CHK_O_L) %>%
  select(-c(A_N_R, A_N_L, A_M_R, A_M_L, A_CHK_I_R, A_CHK_O_R, A_CHK_I_L, A_CHK_O_L)) %>%
  mutate(A_asym = rowSums(across(ends_with("_R")))-rowSums(across(ends_with("_L"))))

# combine areas and areas2
areas.all <- inner_join(areas %>% rename_at(.vars = vars(starts_with("A_")), .funs = function(x) sub("A_", "A1_", x)),
                        areas.google %>% rename_at(.vars = vars(starts_with("A_")), .funs = function(x) sub("A_", "A2_", x)))
colnames(areas.all)
areas.all %>% select(-c(A1_asym, A2_asym)) %>%
  pivot_longer(cols = starts_with("A1"), names_to = "area1", values_to = "val1") %>%
  pivot_longer(cols = starts_with("A2"), names_to = "area2", values_to = "val2") %>%
  left_join(facial.labels %>% mutate(area1 = sub("A_", "A1_", label), A1_label = paste0(meaning, ifelse(!side%in%c("","general"), paste0(" (", side,")"),""))) %>% 
              select(area1, A1_label)) %>% mutate(A1_label = ifelse(A1_label=="face", "full face", A1_label)) %>%
  mutate(A1 = sub("A1_", "", area1), A2 = sub("A2_", "", area2)) %>% filter(A1==A2) %>%
  ggplot(aes(x=val1, y=val2)) +
  geom_point(shape=1) + geom_smooth(method = "lm", se=F, color = palette.1[2]) + ci_ribbon1 +
  ggpubr::stat_cor(color="red") + facet_wrap(~A1_label, scales = "free") + bw.theme +
  labs(x = "Area (Jeopardy Website Pictures)",y="Area (Google Images Search Pictures)")
ggsave2("figs/corr-of-facial-areas-from-jeopardy-pictures-to-google-search-pictures.png",10,8)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################