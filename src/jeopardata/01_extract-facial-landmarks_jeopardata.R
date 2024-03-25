################################################################################
#                         extract facial landmarks and features                #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/jeopardy"
setwd(project.dir)
################################################################################
################################################################################
# run the python script to process these files
py.sc <- "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/customized-functions/facial_coords.py"
cmd <- paste("python3", py.sc,
             "--dir_path", "/Dedicated/jmichaelson-wdata/msmuhammad/data/jeopardy/jeopardata/clean",
             sep = " ")
system(cmd)
#######
# read the landmarks csv, and save them with matched IDs
system(paste0("mv clean2_coords.csv ", "data/derivatives/landmarks-jeopardata_JP.csv"))
#####
######
landmarks <- cbind(read_csv("data/derivatives/landmarks-jeopardata_JP.csv")) %>%
  # filter(!filename %in% c("JD0400.jpg", "JD0367.jpg", "JD0131.jpg", "JD0462.jpg", "JD0645.jpg")) %>% # drop duplicated unwanted pics
  filter(QCPASS!="N") %>%
  rename(JPID = filename) %>%
  mutate(JPID = sub("\\.[a-z]*", "", JPID)) %>%
  select(-QCPASS)
colnames(landmarks)[c(2,3)] <- c("NT_x", "NT_y")
################################################################################
################################################################################
# list of points of interest
# 17,21,22,26,36,39,27,42,45,"NT",48,54
keep <- c(17,21,22,26,36,39,27,42,45,"NT",48,54,31,35,51,57)
main.distances <- data.frame(from = c(17, 22, 36, 42, 48, 27, 31,51,21,21,22,17,26,39,42,17,26,36,45),
                             to = c(21, 26, 39, 45, 54, "NT", 35,57,22,27,27,36,45,"NT","NT",48,54,48,54),
                             label = c("EB_R", "EB_L", "E_R", "E_L", "M_H", "N_V", "N_H", "M_V", "EB_C","EB_N_R", "EB_N_L", "EB_E_R", "EB_E_L","NT_E_R", "NT_E_L","EB_M_R", "EB_M_L","E_M_R", "E_M_L"))
all.distances <- data.frame(t(combn(keep, 2))) %>%
  rownames_to_column("pair") 
registerDoMC(cores = 6)
distances <- foreach(j = 1:nrow(landmarks), .combine = rbind) %dopar% {
  p.landmarks <- landmarks[j,] %>%
    pivot_longer(cols = c(contains("x"), contains("y")), names_to = "coord") %>%
    filter(!grepl("nose", coord)) %>%
    mutate(coord2 = ifelse(grepl("x", coord), "x", "y"),
           coord3 = sub("x", "", coord),
           coord3 = sub("y", "", coord3)) %>%
    pivot_wider(names_from = "coord2", values_from = "value", id_cols = c("JPID", "coord3")) %>%
    mutate(coord3 = sub("rel__", "P_", coord3)) %>%
    mutate(point = parse_number(coord3),
           point = ifelse(grepl("NT", coord3), "NT", point))
  p.nt.x <- p.landmarks %>% filter(coord3 == "NT_") %>% select(x) %>% as.numeric()
  p.nt.y <- p.landmarks %>% filter(coord3 == "NT_") %>% select(y) %>% as.numeric()
  p.landmarks <- p.landmarks %>% 
    mutate(x=x+p.nt.x, y=p.nt.y+y)%>%
    mutate(x = ifelse(coord3 == "NT_", p.nt.x, x),
           y = ifelse(coord3 == "NT_", p.nt.y, y)) %>%
    mutate(x = scales::rescale(x),
           y = scales::rescale(y))
  
  d_m <- foreach(i = 1:nrow(all.distances), .combine = rbind) %dopar% {
    p1 <- all.distances$X1[i]
    p2 <- all.distances$X2[i]
    p1x <- p.landmarks %>% filter(point == p1) %>% select(x) %>% as.numeric()
    p1y <- p.landmarks %>% filter(point == p1) %>% select(y) %>% as.numeric()
    p2x <- p.landmarks %>% filter(point == p2) %>% select(x) %>% as.numeric()
    p2y <- p.landmarks %>% filter(point == p2) %>% select(y) %>% as.numeric()
    d <- sqrt(((p2x-p1x)^2) + ((p2y-p1y)^2))
    data.frame(pair = all.distances$pair[i],
               X1 = p1,
               X2 = p2,
               distance = d)
  }
  d_m %>%
    mutate(JPID = sub("face_", "", landmarks$JPID[j]),
           JPID = sub("\\.jpg", "", JPID))
}
# plot distribution of distances for each pair
p <- distances %>%
  left_join(main.distances %>%
              rename(X1 = "from", X2 = "to") %>%
              mutate(X1 = as.character(X1))) %>%
  mutate(pair = ifelse(is.na(label), pair, label)) %>%
  filter(!is.na(label)) %>%
  ggplot(aes(x=distance)) +
  geom_histogram()+
  facet_wrap("pair", scales = "free")
ggsave(p, filename = "figs/jeopardata/distribution-of-landmarks-pairs-distances.png", bg = "white",
       height = 8, width = 9, units = "in", dpi = 360)
#
################################################################################
wider.distances <- distances %>%
  left_join(main.distances %>%
              rename(X1 = "from", X2 = "to") %>%
              mutate(X1 = as.character(X1))) %>%
  mutate(pair = ifelse(is.na(label), pair, label),
         pair = paste0("P_", pair)) %>%
  pivot_wider(names_from = "pair", values_from = "distance", id_cols = "JPID")
write_csv(wider.distances, "data/derivatives/jeopardata/pairs-distances.csv")
################################################################################
################################################################################
################################################################################
# get the areas in the face
a1 <- c(27,22,23,24,25,26,45,44,43,42,27) # ES_R
a2 <- c(42,43,44,45,46,47,42) # E_R
a3 <- c(27,45,46,47,42,27) # CHK_I_R
a4 <- c(27,28,29,30,33,34,35,27) # N_R
a5 <- c(26,45,35,34,33,51,52,53,54,26) # CHK_O_R
a6 <- c(62,63,64,65,66,57,56,55,54,53,52,51,62) # M_R
a7 <- c(27,21,20,19,18,17,36,37,38,39,27) # ES_L
a8 <- c(36,37,38,39,40,41,36) # E_L
a9 <- c(27,36,41,40,39,27) # CHK_I_L
a10 <- c(27,28,29,30,33,32,31,27) # N_L
a11 <- c(17,36,31,32,33,51,50,49,48,17) # CHK_O_L
a12 <- c(62,61,60,67,66,57,58,59,48,49,50,51,62) # M_L


facial.areas <- foreach(j = 1:nrow(landmarks), .combine = rbind) %dopar% {
  #
  p.landmarks <- landmarks[j,] %>%
    pivot_longer(cols = c(contains("x"), contains("y")), names_to = "coord") %>%
    filter(!grepl("nose", coord)) %>%
    mutate(coord2 = ifelse(grepl("x", coord), "x", "y"),
           coord3 = sub("x", "", coord),
           coord3 = sub("y", "", coord3)) %>%
    pivot_wider(names_from = "coord2", values_from = "value", id_cols = c("JPID", "coord3")) %>%
    mutate(coord3 = sub("rel__", "P_", coord3)) %>%
    mutate(point = parse_number(coord3),
           point = ifelse(grepl("NT", coord3), "NT", point))
  p.nt.x <- p.landmarks %>% filter(coord3 == "NT_") %>% select(x) %>% as.numeric()
  p.nt.y <- p.landmarks %>% filter(coord3 == "NT_") %>% select(y) %>% as.numeric()
  p.landmarks <- p.landmarks %>% 
    mutate(x=x+p.nt.x, y=p.nt.y+y)%>%
    mutate(x = ifelse(coord3 == "NT_", p.nt.x, x),
           y = ifelse(coord3 == "NT_", p.nt.y, y)) %>%
    mutate(x = scales::rescale(x),
           y = scales::rescale(y))
  #
  coords.all <- p.landmarks %>%
    filter(point != "NT") %>%
    mutate(point = as.numeric(point)) %>%
    column_to_rownames("point")
  library(geometry)
  a.a1 <- polyarea(x = coords.all$x[a1+1], y = coords.all$y[a1+1])
  a.a2 <- polyarea(x = coords.all$x[a2+1], y = coords.all$y[a2+1])
  a.a3 <- polyarea(x = coords.all$x[a3+1], y = coords.all$y[a3+1])
  a.a4 <- polyarea(x = coords.all$x[a4+1], y = coords.all$y[a4+1])
  a.a5 <- polyarea(x = coords.all$x[a5+1], y = coords.all$y[a5+1])
  a.a6 <- polyarea(x = coords.all$x[a6+1], y = coords.all$y[a6+1])
  a.a7 <- polyarea(x = coords.all$x[a7+1], y = coords.all$y[a7+1])
  a.a8 <- polyarea(x = coords.all$x[a8+1], y = coords.all$y[a8+1])
  a.a9 <- polyarea(x = coords.all$x[a9+1], y = coords.all$y[a9+1])
  a.a10 <- polyarea(x = coords.all$x[a10+1], y = coords.all$y[a10+1])
  a.a11 <- polyarea(x = coords.all$x[a11+1], y = coords.all$y[a11+1])
  a.a12 <- polyarea(x = coords.all$x[a12+1], y = coords.all$y[a12+1])
  data.frame(area_label = c("ES_R", "E_R", "CHK_I_R", "N_R", "CHK_O_R", "M_R",
                            "ES_L", "E_L", "CHK_I_L", "N_L", "CHK_O_L", "M_L"),
             area = c(a.a1, a.a2, a.a3, a.a4, a.a5, a.a6, a.a7, a.a8, a.a9, a.a10, a.a11, a.a12)) %>%
    mutate(JPID = sub("face_", "", landmarks$JPID[j]),
           JPID = sub("\\.jpg", "", JPID))
}
# plot distribution of areas
p <- facial.areas %>%
  ggplot(aes(x=area)) +
  geom_histogram()+
  facet_wrap("area_label", scales = "free")
ggsave(p, filename = "figs/jeopardata/distribution-of-landmarks-areas.png", bg = "white",
       height = 8, width = 9, units = "in", dpi = 360)
#
################################################################################
wider.facial.areas <- facial.areas %>%
  mutate(area_label = paste0("A_", area_label)) %>%
  pivot_wider(names_from = "area_label", values_from = "area", id_cols = "JPID")
write_csv(wider.facial.areas, "data/derivatives/jeopardata/facial.areas.csv")
################################################################################


################################################################################
