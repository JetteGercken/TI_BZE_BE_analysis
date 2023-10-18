# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# sorting trees according to tree 
# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------


source(paste0(getwd(), "/scripts/functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ",")
 



# ----- 0.6 harmonising column names & structure  -----------------------------------------------------------------
# HBI 
colnames(HBI_trees) <- c("multi_stem", "D_mm", "DBH_class", "DBH_h_cm", "H_dm",
                         "azi_gon", "SP_code", "tree_ID", "plot_ID", "tree_inventory_status", 
                         "DBH_cm", "age", "C_layer", "C_h_dm", "Kraft", "Dist_cm", "age_meth")  
HBI_trees <- HBI_trees %>% 
  select(plot_ID,  tree_ID,  tree_inventory_status,  multi_stem, Dist_cm,  azi_gon, age, age_meth,  
         SP_code, DBH_class,  Kraft, C_layer, H_dm,  C_h_dm, D_mm,   DBH_h_cm,  DBH_cm ) %>% 
  mutate(inv = "HBI",
         DBH_cm = ifelse(DBH_h_cm == 130, D_mm/10, DBH_BWI(D_mm, DBH_h_cm)))

# create practice dataset from HBI data
BZE3_trees <- HBI_trees[1:10,] %>% 
  mutate(D_mm = D_mm+10,
         H_dm = as.numeric(H_dm)+10, 
         Dist_cm= Dist_cm+20, 
         tree_inventory_status = case_when(row_number() == 1 ~ -9,
                                           row_number() == 2 ~ -1,
                                           row_number() == 3 ~ 0,
                                           row_number() == 4 ~ 1,
                                           row_number() == 5 ~ 2,
                                           row_number() == 6 ~ 3,
                                           row_number() == 7 ~ 4,
                                           row_number() == 8 ~ 5,
                                           row_number() == 9 ~ 6,
                                           row_number() == 10 ~ 7,
                                           TRUE ~ NA)) %>% 
  mutate(inv = "BZE3",
         DBH_cm = ifelse(DBH_h_cm == 130, as.numeric(D_mm)/10, DBH_BWI(as.numeric(D_mm), as.numeric(DBH_h_cm))))

# mutate two no 
BZE3_trees <- rbind(
  BZE3_trees,
  BZE3_trees %>% filter(tree_inventory_status == 6) %>% mutate(tree_ID = 11, SP_code = "gki", azi_gon = azi_gon -10, D_mm = D_mm+30, Dist_cm= Dist_cm+30, tree_inventory_status = 0),
  BZE3_trees %>% filter(tree_inventory_status == 6) %>% mutate(tree_ID = 12, SP_code = , azi_gon = azi_gon +5, D_mm = D_mm+10, Dist_cm= Dist_cm+20, tree_inventory_status = 0)
  )

# creating dataset with information about the concentric sampling circles
data_circle <- data.frame(x0 = c(0,0,0),       # x of centre point of all 3 circles is 0 
                          y0 = c(0,0,0),       # y of centre point of all 3 circles is 0 
                          r0 = c(5.64, 12.62, 17.84), # darius in m
                          rmax = c(30.00, 30.00, 30.00)) # these are the radi of the sampling circuits in m


# tree inventory status == -9 ---------------------------------------------
# this is like NA. 
# what we can check is, if theres a tree with a similar position (+-10 gon and 20cm distance or so)
# if so, we can change the inventory ID to 1 - repeated inventory
# if not we will have to set it to 0 - newly inventorised
# subset data frot inventory status -9 
BZE3_trees_9 <- BZE3_trees %>% filter(tree_inventory_status == -9)
tree_inventory_status_9.list <- vector(mode = "list", length = length(BZE3_trees_9$tree_ID))

for (i in 1:length(BZE3_trees_9$tree_ID)) {
  # i = 1
  
  my.plot.id <- BZE3_trees_9[i, "plot_ID"]
  my.tree.id <- BZE3_trees_9[i, "tree_ID"]
  my.tree.spec <- BZE3_trees_9[i, "SP_code"]
  my.inv <- BZE3_trees_9[i, "inv"]
  my.dbh.cm <- BZE3_trees_9[i, "DBH_cm"]
  
  azi.tree.2 <- as.numeric(BZE3_trees_9[i, "azi_gon"])
  dist.tree.2 <- as.numeric(BZE3_trees_9[i, "Dist_cm"])/100
  x.tree.2 <- dist.tree.2*sin(azi.tree.2)       # this is: easting, longitude, RW
  y.tree.2 <- dist.tree.2*cos(azi.tree.2)       # this is: northing, latitude, HW 
  
  # select the distance and azimute of the trees of the previous inventory by plot ID 
  # to calcualte the coordiantes of all trees of the plot in the previous inventory
  azi.tree.1 <- HBI_trees$azi_gon[HBI_trees$plot_ID == my.plot.id]
  dist.tree.1 <- HBI_trees$Dist_cm[HBI_trees$plot_ID == my.plot.id]/100
  x.tree.1 <- dist.tree.1*sin(azi.tree.1)       # this is: easting, longitude, RW
  y.tree.1 <- dist.tree.1*cos(azi.tree.1)       # this is: northing, latitude, HW 

  # select the row number of the tree point in the HBI (inventory 1) dataframe of the same plot ID,
  # which has the smallest distance to the given tree corodinates from BZE3 (inventory 2)
  closest.id <- which.min( distance(x.tree.1, y.tree.1, x.tree.2, y.tree.2))
  
  # calculate or select the actual distance, species and dbh between the selected row/ coordiantes of tzhe nearest neighbout canidate from HBI and the given tree from BZE3
  distance.point.and.nearest.neighbour <- distance(x.tree.1[closest.id], y.tree.1[closest.id], x.tree.2, y.tree.2)
  species.nearest.neighbour <- HBI_trees$SP_code[HBI_trees$plot_ID == my.plot.id][closest.id]
  dbh.nearest.neighbour <- HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id][closest.id]
  
  # we can assume its the same tree and they just forgot to give a tree 
  # inventory status number if:
    # the distance is within a range of +/- 50cm, 
    # if the species is identical 
  # maybe also if the dbh is lower or equal? 
  tree_inventory_status <- ifelse(distance.point.and.nearest.neighbour <= 50 & my.tree.spec == species.nearest.neighbour & my.dbh.cm >= dbh.nearest.neighbour | 
                                    distance.point.and.nearest.neighbour >= 50 & my.tree.spec == species.nearest.neighbour & my.dbh.cm >= dbh.nearest.neighbour, 1, NA)
  
  # build dataset that links tree status with plot, tree and inventory ID so the tree remains indentifiable
  tree_inventory_status_9.list[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(my.plot.id),
    "tree_ID" = c(my.tree.id),
    "inv" = c(my.inv),
    "tree_inventory_status_new" = c(tree_inventory_status)
    ))

  print(ggplot()+ 
          geom_circle(aes(x0 = data_circle$x0, y0 = data_circle$y0, r = data_circle$r0))+
          geom_point(aes(x.tree.1, y.tree.1, size = HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id]))+
          geom_point(aes(x.tree.2, y.tree.2, size =my.dbh.cm, color= "red"))+ 
          guides(color=guide_legend(title="tree from inv. 2"))+
          guides(size=guide_legend(title="DBH cm"))+
          geom_text(aes(x.tree.1, y.tree.1), 
                    label= HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id],
                    nudge_x=0.45, nudge_y=0.1,check_overlap=T)+
          geom_text(aes(x.tree.2, y.tree.2), 
                    label= BZE3_trees_9$tree_ID[BZE3_trees_9$plot_ID == my.plot.id & BZE3_trees_9$tree_ID == my.tree.id],
                    nudge_x=0.45, nudge_y=0.1, check_overlap=T)
  )
  
}
# safe list in dataframe
tree_inventory_status_9.df <- as.data.frame(tree_inventory_status_9.list)

# join the new tree inventory status in and replace -9s and NAs if possible
BZE3_trees <- BZE3_trees %>% 
  left_join(., tree_inventory_status_9.df, 
            by = c("plot_ID","tree_ID", "inv" )) %>% 
  mutate(new_tree_inventory_status = ifelse(tree_inventory_status == -9 | is.na(tree_inventory_status),tree_inventory_status_new, tree_inventory_status)) %>% 
    select(-tree_inventory_status_new)


# tree inventory status == 4 ---------------------------------------------
# for trees that have the status 4 the tree should have not been assessed in the previous inventory 
# what we have to do is find the tree in the previous inventory (so a tree that has the somewhat similar position and tree ID)
# and remove it from the dataset 

BZE3_trees_4 <- BZE3_trees %>% filter(tree_inventory_status == 4)
tree_inventory_status_4.list <- vector(mode = "list", length = length(BZE3_trees_4$tree_ID))

for (i in 1:length(BZE3_trees_4$tree_ID)) {
  # i = 1
  
  my.plot.id <- BZE3_trees_4[i, "plot_ID"]
  my.tree.id <- BZE3_trees_4[i, "tree_ID"]
  my.tree.spec <- BZE3_trees_4[i, "SP_code"]
  my.inv <- BZE3_trees_4[i, "inv"]
  my.dbh.cm <- BZE3_trees_4[i, "DBH_cm"]
  
  azi.tree.2 <- as.numeric(BZE3_trees_4[i, "azi_gon"])
  dist.tree.2 <- as.numeric(BZE3_trees_4[i, "Dist_cm"])/100
  x.tree.2 <- dist.tree.2*sin(azi.tree.2)       # this is: easting, longitude, RW
  y.tree.2 <- dist.tree.2*cos(azi.tree.2)       # this is: northing, latitude, HW 
  
  # select the distance and azimute of the trees of the previous inventory by plot ID 
  # to calcualte the coordiantes of all trees of the plot in the previous inventory
  azi.tree.1 <- HBI_trees$azi_gon[HBI_trees$plot_ID == my.plot.id]
  dist.tree.1 <- HBI_trees$Dist_cm[HBI_trees$plot_ID == my.plot.id]/100
  x.tree.1 <- dist.tree.1*sin(azi.tree.1)       # this is: easting, longitude, RW
  y.tree.1 <- dist.tree.1*cos(azi.tree.1)       # this is: northing, latitude, HW 
  
  # select the row number of the tree point in the HBI (inventory 1) dataframe of the same plot ID,
  # which has the smallest distance to the given tree corodinates from BZE3 (inventory 2)
  closest.id <- which.min( distance(x.tree.1, y.tree.1, x.tree.2, y.tree.2))
  
  # calculate or select the actual distance, species and dbh between the selected row/ coordiantes of tzhe nearest neighbout canidate from HBI and the given tree from BZE3
  distance.point.and.nearest.neighbour <- distance(x.tree.1[closest.id], y.tree.1[closest.id], x.tree.2, y.tree.2)
  species.nearest.neighbour <- HBI_trees$SP_code[HBI_trees$plot_ID == my.plot.id][closest.id]
  dbh.nearest.neighbour <- HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id][closest.id]
  tree.id.nearest.neighbour <- HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]
  inv.status.nearest.neighbour <- HBI_trees$tree_inventory_status[HBI_trees$plot_ID == my.plot.id][closest.id]
  
  # we can assume its the same tree and they just forgot to give a tree 
  # inventory status number if:
  # the distance is within a range of +/- 50cm, 
  # if the species is identical 
  # maybe also  dbh is lower or equal
  # if the tree id is identical
  tree.found.in.inv.1.dataset <- ifelse(distance.point.and.nearest.neighbour <= 50 & 
                                          my.tree.spec == species.nearest.neighbour & 
                                          my.dbh.cm >= dbh.nearest.neighbour & 
                                          my.tree.id == tree.id.nearest.neighbour| 
                                          distance.point.and.nearest.neighbour >= 50 & 
                                          my.tree.spec == species.nearest.neighbour & 
                                          my.dbh.cm >= dbh.nearest.neighbour  & 
                                          my.tree.id == tree.id.nearest.neighbour, "yes", "no")
  
  # build dataset that enables to identify the tree in the dataset of the previous inventory
  tree_inventory_status_4.list[[i]] <- if(tree.found.in.inv.1.dataset == "yes"){
    as.data.frame(rbind(HBI_trees%>% filter(plot_ID == my.plot.id) %>% slice(closest.id) %>% mutate(new_tree_inventory_status=NA),
                        BZE3_trees_4[i,]))}else{
                          as.data.frame(rbind(BZE3_trees_4[i,]))
                        }
  
  print(ggplot()+ 
          geom_circle(aes(x0 = data_circle$x0, y0 = data_circle$y0, r = data_circle$r0))+
          geom_point(aes(x.tree.1, y.tree.1, size = HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id]))+
          geom_point(aes(x.tree.2, y.tree.2, size =my.dbh.cm, color= "red"))+ 
          guides(color=guide_legend(title="tree from inv. 2"))+
          guides(size=guide_legend(title="DBH cm"))+
          geom_text(aes(x.tree.1, y.tree.1), 
                    label= HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id],
                    nudge_x=0.45, nudge_y=0.1,check_overlap=T)+
          geom_text(aes(x.tree.2, y.tree.2), 
                    label= BZE3_trees_4$tree_ID[BZE3_trees_4$plot_ID == my.plot.id & BZE3_trees_4$tree_ID == my.tree.id],
                    nudge_x=0.45, nudge_y=0.1, check_overlap=T)
  )
  
}
# safe list in dataframe
tree_inventory_status_4.df <- as.data.frame(tree_inventory_status_4.list)

# remove 
BZE3_trees <- BZE3_trees %>% 
  anti_join(., tree_inventory_status_4.df, 
            by = c("plot_ID","tree_ID", "inv" ))

HBI_trees <- HBI_trees %>% 
  anti_join(., tree_inventory_status_4.df, 
            by = c("plot_ID","tree_ID", "inv" ))


# tree inventory status == 6 ---------------------------------------------
# for trees that have the status 6 a tree that was previously part of the inventory is not part of the inventory anymore but 

BZE3_trees_6 <- BZE3_trees %>% filter(tree_inventory_status == 6)
tree_inventory_status_6.list <- vector(mode = "list", length = length(BZE3_trees_6$tree_ID))

for (i in 1:length(BZE3_trees_6$tree_ID)) {
  # i = 1
  
  # select tree details for respective tree i 
  my.plot.id <- BZE3_trees_6[i, "plot_ID"]
  my.tree.id <- BZE3_trees_6[i, "tree_ID"]
  my.tree.spec <- BZE3_trees_6[i, "SP_code"]
  my.inv <- BZE3_trees_6[i, "inv"]
  my.dbh.cm <- BZE3_trees_6[i, "DBH_cm"]
  # calculate coordiantes for tree i 
  azi.tree.2 <- as.numeric(BZE3_trees_6[i, "azi_gon"])
  dist.tree.2 <- as.numeric(BZE3_trees_6[i, "Dist_cm"])/100
  x.tree.2 <- dist.tree.2*sin(azi.tree.2)       # this is: easting, longitude, RW
  y.tree.2 <- dist.tree.2*cos(azi.tree.2)       # this is: northing, latitude, HW 
  
  
  # select the distance and azimute of the trees of the previous inventory by plot ID 
  # to calcualte the coordiantes of all trees of the plot in the previous inventory
  azi.tree.1 <- HBI_trees$azi_gon[HBI_trees$plot_ID == my.plot.id]
  dist.tree.1 <- HBI_trees$Dist_cm[HBI_trees$plot_ID == my.plot.id]/100
  x.tree.1 <- dist.tree.1*sin(azi.tree.1)       # this is: easting, longitude, RW
  y.tree.1 <- dist.tree.1*cos(azi.tree.1)       # this is: northing, latitude, HW 
  # select the row number of the tree point in the HBI (previous inventory, inventory 1) dataframe of the same plot ID,
  # which has the smallest distance to the given tree corodinates from BZE3 (inventory 2)
  closest.id <- which.min(distance(x.tree.1, y.tree.1, x.tree.2, y.tree.2))
  # calculate or select the actual distance, species and dbh between the selected row/ coordiantes of tzhe nearest neighbout canidate from HBI and the given tree from BZE3
  distance.point.and.nearest.neighbour <- distance(x.tree.1[closest.id], y.tree.1[closest.id], x.tree.2, y.tree.2)
  species.nearest.neighbour <- HBI_trees$SP_code[HBI_trees$plot_ID == my.plot.id][closest.id]
  dbh.nearest.neighbour <- HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id][closest.id]
  tree.id.nearest.neighbour <- HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]
  inv.status.nearest.neighbour <- HBI_trees$tree_inventory_status[HBI_trees$plot_ID == my.plot.id][closest.id]
  # check if the tree we found in HBI/ previous inventory/ inventors 1 is actually fitting with our tree i 
  # we can assume its the same tree and they just forgot to give a tree inventory status number if:
      # the distance is within a range of +/- 50cm, 
      # if the species is identical 
      # maybe also  dbh is lower or equal
      # if the tree id is identical
  tree.found.in.inv.1.dataset <- ifelse(distance.point.and.nearest.neighbour <= 50 & my.tree.spec == species.nearest.neighbour & 
                                          my.dbh.cm >= dbh.nearest.neighbour & my.tree.id == tree.id.nearest.neighbour| 
                                          distance.point.and.nearest.neighbour >= 50 & my.tree.spec == species.nearest.neighbour & 
                                          my.dbh.cm >= dbh.nearest.neighbour  & my.tree.id == tree.id.nearest.neighbour, 
                                        "yes", "no") 
  
# find partner trees in the same inventory dataset (bze3, inventory 2, most recent inventory): 
  # narrow down the closest two trees dataset 
  closest.trees.canidates.2 <- BZE3_trees %>% 
    filter(plot_ID == my.plot.id & tree_ID != my.tree.id & SP_code == my.tree.spec & tree_inventory_status == 0)
  
  # calcualte cartesian coordiantes of all trees at the plot
  azi.all.trees.2 <- as.numeric(closest.trees.canidates.2$azi_gon)
  dist.all.trees.2 <- as.numeric(closest.trees.canidates.2$Dist_cm)/100
  x.all.trees.2 <- dist.all.trees.2*sin(azi.all.trees.2)       # this is: easting, longitude, RW
  y.all.trees.2 <- dist.all.trees.2*cos(azi.all.trees.2)       # this is: northing, latitude, HW 
  
  closest.id.1 <- (as.data.frame(cbind(
    d = distance(x.all.trees.2, y.all.trees.2, x.tree.2, y.tree.2),
    id = BZE3_trees$tree_ID[HBI_trees$plot_ID == my.plot.id])) %>%
      arrange(d))[1,2]
  closest.id.2 <- (as.data.frame(cbind(
    d = distance(x.all.trees.2, y.all.trees.2, x.tree.2, y.tree.2),
    id = BZE3_trees$tree_ID[HBI_trees$plot_ID == my.plot.id])) %>%
      arrange(d))[2,2]
  # select the info of the two clostest trees in one dataframe  
  closest.trees.df <- BZE3_trees[BZE3_trees$plot_ID == my.plot.id][c(closest.id.1, closest.id.2),]
  # check if the clostest trees can really be the trees originatinf from tree i (status 6) by species
  closest.id.1 <- if(clostet.tree.df$SP_code[1] == my.tree.spec){closest.id.1}else{NA}
  closest.id.2 <- if(clostet.tree.df$SP_code[2] == my.tree.spec){closest.id.2}else{NA}
  
  # select the tree among the two closest trees in BZE3/invenotry 2/ recent inventory 
  # that has the most similar diameter to the one of the tree from HBI/ inventory / previous inventory
  closest.tree.2 <- closest.trees.df[which.min(abs(closest.trees.df$DBH_cm - dbh.nearest.neighbour)),]
  
  # check if the coordinates, dbh, and species 
  
  
  # build dataset that enables to identify the tree in the dataset of the previous inventory
  tree_inventory_status_6.list[[i]] <- if(tree.found.in.inv.1.dataset == "yes"){
    as.data.frame(rbind(HBI_trees%>% filter(plot_ID == my.plot.id) %>% slice(closest.id) %>% mutate(new_tree_inventory_status=NA),
                        BZE3_trees_6[i,]))}else{
                          as.data.frame(rbind(BZE3_trees_6[i,]))
                        }
  
  print(ggplot()+ 
          geom_circle(aes(x0 = data_circle$x0, y0 = data_circle$y0, r = data_circle$r0))+
          geom_point(aes(x.tree.1, y.tree.1, size = HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id]))+
          geom_point(aes(x.tree.2, y.tree.2, size =my.dbh.cm, color= "red"))+ 
          guides(color=guide_legend(title="tree from inv. 2"))+
          guides(size=guide_legend(title="DBH cm"))+
          geom_text(aes(x.tree.1, y.tree.1), 
                    label= HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id],
                    nudge_x=0.45, nudge_y=0.1,check_overlap=T)+
          geom_text(aes(x.tree.2, y.tree.2), 
                    label= BZE3_trees_6$tree_ID[BZE3_trees_6$plot_ID == my.plot.id & BZE3_trees_6$tree_ID == my.tree.id],
                    nudge_x=0.45, nudge_y=0.1, check_overlap=T)
  )
  
}
# safe list in dataframe
tree_inventory_status_4.df <- as.data.frame(tree_inventory_status_4.list)

# remove 
BZE3_trees <- BZE3_trees %>% 
  anti_join(., tree_inventory_status_4.df, 
            by = c("plot_ID","tree_ID", "inv" ))

HBI_trees <- HBI_trees %>% 
  anti_join(., tree_inventory_status_4.df, 
            by = c("plot_ID","tree_ID", "inv" ))









