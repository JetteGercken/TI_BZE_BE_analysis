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
HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/beab.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)
 



# ----- 0.6 harmonising column names & structure  -----------------------------------------------------------------
# HBI 
colnames(HBI_trees) <- c("multi_stem", "D_mm", "DBH_class", "DBH_h_cm", "H_dm",
                         "azi_gon", "SP_code", "tree_ID", "plot_ID", "tree_inventory_status", 
                         "DBH_cm", "age", "C_layer", "C_h_dm", "Kraft", "Dist_cm", "age_meth")  
HBI_trees <- HBI_trees %>% 
  select(plot_ID,  tree_ID,  tree_inventory_status,  multi_stem, Dist_cm,  azi_gon, age, age_meth,  
         SP_code, DBH_class,  Kraft, C_layer, H_dm,  C_h_dm, D_mm,   DBH_h_cm,  DBH_cm ) %>% 
  mutate(inv = "HBI",
         inv_year = 2012, 
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
         inv_year = 2023, 
         DBH_cm = ifelse(DBH_h_cm == 130, as.numeric(D_mm)/10, DBH_BWI(as.numeric(D_mm), as.numeric(DBH_h_cm))))

# mutate two no 
BZE3_trees <- rbind(
  BZE3_trees,
  BZE3_trees %>% filter(tree_inventory_status == 6) %>% mutate(tree_ID = 27, SP_code = "gki", azi_gon = azi_gon -1, D_mm = D_mm+30, tree_inventory_status = 0),
  BZE3_trees %>% filter(tree_inventory_status == 6) %>% mutate(tree_ID = 28, SP_code = "gki" , azi_gon = azi_gon +1, D_mm = D_mm+10, tree_inventory_status = 0)
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
  distance.my.tree.and.nearest.neighbour <- distance(x.tree.1[closest.id], y.tree.1[closest.id], x.tree.2, y.tree.2)
  species.nearest.neighbour <- HBI_trees$SP_code[HBI_trees$plot_ID == my.plot.id][closest.id]
  dbh.nearest.neighbour <- HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id][closest.id]
  t_id.nearest.neighbour <- HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]
  
  
  # we can assume its the same tree and they just forgot to give a tree 
  # inventory status number if:
    # the distance is within a range of +/- 50cm, 
    # if the species is identical 
  # maybe also if the dbh is lower or equal? 
  tree_inventory_status_2 <- ifelse(distance.my.tree.and.nearest.neighbour <= 0.5 & 
                                      my.tree.spec == species.nearest.neighbour & 
                                      my.dbh.cm >= dbh.nearest.neighbour & 
                                      my.tree.id == t_id.nearest.neighbour | 
                                    distance.my.tree.and.nearest.neighbour >= 0.5 & 
                                      my.tree.spec == species.nearest.neighbour & 
                                      my.dbh.cm >= dbh.nearest.neighbour & 
                                      my.tree.id == t_id.nearest.neighbour, 1, NA)
  
  tree_inventory_status_1 <- ifelse(!is.na(tree_inventory_status_2), 0, NA)
  # build dataset that links tree status with plot, tree and inventory ID so the tree remains indentifiable
  tree_inventory_status_9.list[[i]] <- as.data.frame(cbind(
    "plot_ID" = c(my.plot.id, HBI_trees$plot_ID[HBI_trees$plot_ID == my.plot.id][closest.id]),
    "tree_ID" = c(my.tree.id, HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]),
    "inv" = c(my.inv, HBI_trees$inv[HBI_trees$plot_ID == my.plot.id][closest.id]),
    "tree_inventory_status_new" = c(tree_inventory_status_2, tree_inventory_status_1)
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
# https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
tree_inventory_status_9.df[,c(1,2)] <- lapply(tree_inventory_status_9.df[,c(1,2)], as.integer)

# join the new tree inventory status in and replace -9s and NAs if possible
BZE3_trees <- BZE3_trees %>% 
  left_join(., tree_inventory_status_9.df, 
            by = c("plot_ID","tree_ID", "inv" )) %>% 
  mutate(new_tree_inventory_status = ifelse(tree_inventory_status == -9 | is.na(tree_inventory_status),tree_inventory_status_new, tree_inventory_status)) %>% 
    select(-tree_inventory_status_new)


HBI_trees <- HBI_trees %>% 
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
  distance.my.tree.and.nearest.neighbour <- distance(x.tree.1[closest.id], y.tree.1[closest.id], x.tree.2, y.tree.2)
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
  tree.found.in.inv.1.dataset <- ifelse(distance.my.tree.and.nearest.neighbour <= 0.5 & 
                                          my.tree.spec == species.nearest.neighbour & 
                                          my.dbh.cm >= dbh.nearest.neighbour & 
                                          my.tree.id == tree.id.nearest.neighbour| 
                                          distance.my.tree.and.nearest.neighbour >= 0.5 & 
                                          my.tree.spec == species.nearest.neighbour & 
                                          my.dbh.cm >= dbh.nearest.neighbour  & 
                                          my.tree.id == tree.id.nearest.neighbour, "yes", "no")
  
  # build dataset that enables to identify the tree in the dataset of the previous inventory (HBI, inventory 1)
  tree_inventory_status_4.list[[i]] <- if(tree.found.in.inv.1.dataset == "yes"){
   # https://dplyr.tidyverse.org/reference/slice.html 
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
# goal of this loop is therefore to find the tree IDs in the candidates dataset/ most recent inventory/ inventory 2
# of those trees that are closest to my.tree (tree i with status 6) 
# this is because the stem of my.tree (tree i with status 6) was assessed as one stem in the previous inventory, 
# but is assessed as as tree with split stem/ multi-stem now, meaning the two parts of the tree are
# assessed as newly inventoried tho there is a partner tree to tree i in the previous inventory
# in from of the main branch of tree i 
# 1. thus we first identify the partner tree of my.tree/ tree i in the previous inventory/ inventory 1 by: 
      # closest position +/- 50cm distance to my.tree
      # smalle or same diameter as my.tree
      # same species as my.tree
      # same ID as my.tree
# 2. further we identify the two new trees (tree status == 0) in the recent inventory/ inventory 2 dataset 
#    that originate from tree i/ my.tree with tree_status == 6 by
       # same species
       # only trees with id != my.tree.id
       # tree inventory status == 0 
# 3. then we check which of these two new trees is closest to the original tree i/ my.tree by: 
      # closest position
      # tree status == 0 
      # tree species of candidate and original tree match
# 4. after that we partner the closest candidate originating from tree i/ my.tree with the potential partner tree from 
#    the previous inventory if there is one


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
  # select the row number of the tree point in the HBI (previous inventory, inventory 1) dataframe 
  # of the same plot ID, which has the smallest distance to the given my.tree corodinates from BZE3 (inventory 2)
  closest.id <- which.min(distance(x.tree.1, y.tree.1, x.tree.2, y.tree.2))
  
  # calculate or select the actual distance, species and dbh between the selected row/ coordiantes of tzhe nearest neighbout canidate from HBI and the given tree from BZE3
  distance.my.tree.and.nearest.neighbour <- distance(x.tree.1[closest.id], y.tree.1[closest.id], x.tree.2, y.tree.2)
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
  my.tree.found.in.inv.1.dataset <- ifelse(distance.my.tree.and.nearest.neighbour <= 50 & my.tree.spec == species.nearest.neighbour & 
                                          my.dbh.cm >= dbh.nearest.neighbour & my.tree.id == tree.id.nearest.neighbour| 
                                          distance.my.tree.and.nearest.neighbour >= 50 & my.tree.spec == species.nearest.neighbour & 
                                          my.dbh.cm >= dbh.nearest.neighbour  & my.tree.id == tree.id.nearest.neighbour, 
                                        "yes", "no") 
  
# find partner trees in the same inventory dataset (bze3, inventory 2, most recent inventory): 
  # narrow down the closest two trees dataset by selecting from BZE3/ inventory 2/ most recent inventory:  
    # trees of the same plot_ID, 
    # trees with different tree_ID then my.tree.id
    # trees with same species as my.tree.spec
    # trees with inventory status 0
  closest.trees.canidates.2 <- BZE3_trees %>% 
    filter(plot_ID == my.plot.id & tree_ID != my.tree.id & SP_code == my.tree.spec & tree_inventory_status == 0)
  
  # calcualte cartesian coordiantes of all potential partner trees of my.tree at the plot
  azi.all.trees.2 <- as.numeric(closest.trees.canidates.2$azi_gon)
  dist.all.trees.2 <- as.numeric(closest.trees.canidates.2$Dist_cm)/100 # divide by 100 to transform into m 
  x.all.trees.2 <- dist.all.trees.2*sin(azi.all.trees.2)       # this is: easting, longitude, RW
  y.all.trees.2 <- dist.all.trees.2*cos(azi.all.trees.2)       # this is: northing, latitude, HW 
  # find the tree IDs in the canidates dataset/ current inventory/ inventory 2/ BZE3 dataset 
  # of the trees that are closest/ originating from my.tree (tree i with status 6) 
  # closest
  closest.id.1 <- (as.data.frame(cbind(
    d = distance(x.all.trees.2, y.all.trees.2, x.tree.2, y.tree.2),
    t_id = c(closest.trees.canidates.2$tree_ID))) %>%
      arrange(d))[1,2]
  # second closest
  closest.id.2 <- (as.data.frame(cbind(
    d = distance(x.all.trees.2, y.all.trees.2, x.tree.2, y.tree.2),
    t_id = c(closest.trees.canidates.2$tree_ID))) %>%
      arrange(d))[2,2]
  # select the info of the two clostest trees in one dataframe  
  closest.trees.df <- BZE3_trees %>% filter(plot_ID == my.plot.id & tree_ID %in% c(closest.id.1, closest.id.2))
  # select the tree among the two closest trees in BZE3/invenotry 2/ recent inventory 
  # that has the most similar diameter to the one of the tree from HBI/ inventory / previous inventory
  closest.tree.2.df <- closest.trees.df[which.min(abs(closest.trees.df$DBH_cm - dbh.nearest.neighbour)),]
  
  # build dataset that enables to identify partner tree of my.tree in the previous inventory, my.tree, 
  # and closest.tree in most recent inventory dataset that 
  tree_inventory_status_6.list[[i]] <- if(tree.found.in.inv.1.dataset == "yes"){
    as.data.frame(rbind(
      # partner tree of my.tree in previous inventory (HBI/ inventory 1)
      HBI_trees %>% filter(plot_ID == my.plot.id) %>% slice(closest.id) %>% 
        mutate(new_tree_inventory_status=tree_inventory_status,
               # we have to change the tree ID to the one of the potential partner tree 
               # originating from my.tree that´s still labled status == 0 in the current inventory dataset
               old_tree_ID = tree_ID,
               tree_ID = closest.tree.2.df$tree_ID, 
               tree_type_status_6 = "partner_tree_inv_1"),
      # my.tree row --> this one has to be removed later 
      BZE3_trees_6[i,] %>% mutate(tree_type_status_6 = "my_tree_inv_2", 
                                  old_tree_ID = tree_ID),
      # newly inventoried tree that most likely originates from my.tree and matches best with partner tree
      # --> here we have to change the new tree ID to 1, as the tree has a partner tree in 
      closest.tree.2.df %>% mutate(new_tree_inventory_status = 1, 
                                   tree_type_status_6 = "clostest_tree_inv_2", 
                                   old_tree_ID = tree_ID)
      ))}else{as.data.frame(rbind(BZE3_trees_6[i,]))}
  
  print(ggplot()+ 
          geom_circle(aes(x0 = data_circle$x0, y0 = data_circle$y0, r = data_circle$r0))+
          geom_point(aes(x.tree.1, y.tree.1, size = HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id]))+
          geom_point(aes(x.tree.2, y.tree.2, size =my.dbh.cm, color= "my tree"))+ 
          geom_point(aes(x.all.trees.2, y.all.trees.2, size = closest.trees.canidates.2$DBH_cm, color = "BZE3 nearest neighbours"))+
          guides(color=guide_legend(title="tree from inv. 2"))+
          guides(size=guide_legend(title="DBH cm"))+
          geom_text(aes(x.tree.1, y.tree.1), 
                    label= HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id],
                    nudge_x=0.45, nudge_y=0.1,check_overlap=T)+
          geom_text(aes(x.tree.2, y.tree.2), 
                    label= BZE3_trees_6$tree_ID[BZE3_trees_6$plot_ID == my.plot.id & BZE3_trees_6$tree_ID == my.tree.id],
                    nudge_x=0.45, nudge_y=0.1, check_overlap=T)+
          geom_text(aes(x.all.trees.2, y.all.trees.2), 
                    label= closest.trees.canidates.2$tree_ID,
                    nudge_x=0.45, nudge_y=0.1, check_overlap=T)
  )
  
}
# safe list in dataframe
tree_inventory_status_6.df <- as.data.frame(tree_inventory_status_6.list)

# remove my.tree with status 6 from recent inventory dataframe, 
# as well the tree identified as the closest tree in the original dataset 
# to then rbind it back in from the tree_inventory_status_6.df --> this serves like an update 
# of those trees that represent the partner of the partner or the origianl tree with status 6 in the previous inventory 
BZE3_trees <- rbind(BZE3_trees %>% 
  anti_join(., tree_inventory_status_6.df %>% 
              filter(tree_type_status_6 %in% c("my_tree_inv_2", "clostest_tree_inv_2")),
            by = c("plot_ID", c("tree_ID" = "old_tree_ID") , "inv" )), 
  tree_inventory_status_6.df %>% 
    filter(tree_type_status_6 == "clostest_tree_inv_2") %>% 
    select(-c(tree_type_status_6, old_tree_ID))) %>% 
  arrange(plot_ID, tree_ID)


HBI_trees <- rbind(HBI_trees %>% 
  anti_join(., tree_inventory_status_6.df %>% filter(tree_type_status_6 == "partner_tree_inv_1"), 
            by = c("plot_ID",c("tree_ID" = "old_tree_ID"), "inv" )), 
  tree_inventory_status_6.df %>% 
    filter(tree_type_status_6 == "partner_tree_inv_1") %>% 
    select(-c(tree_type_status_6, old_tree_ID))) %>% 
  arrange(plot_ID, tree_ID)





# tree inventory status == 5 ----------------------------------------------
# this inventory status means that the tree should have been assessed in the previous 
# invenotry but wasn´t
# thus we have to calculate how much the tree of that species at that plot would have grown 
# between the previous and current inventory, then deduct it from the diameter of the 
# respective tree in the current inventory and add the tree to the previous inventory with
# the same ID, tree status 0 and the reduced diameter
# for this inventory status 

## calculate averange annual diameter growth per single tree per species and plot 
growth.df <- left_join(HBI_trees %>% 
                        # select trees that were newly inventored, repeated inventory, or unknown status
                        filter(new_tree_inventory_status %in% c(0, 1, -9))%>% 
                        rename(HBI_DBH_cm = DBH_cm) %>% 
                        rename(HBI_inv_year = inv_year) %>% 
                        select(plot_ID, tree_ID, HBI_inv_year, SP_code, HBI_DBH_cm), 
                      # select trees that are repeatedly inventory, or unknown status
                      BZE3_trees %>% 
                        filter(new_tree_inventory_status %in% c(1, -9)) %>% 
                        rename(BZE3_DBH_cm = DBH_cm) %>% 
                        rename(BZE3_inv_year = inv_year) %>% 
                        select(plot_ID, tree_ID, BZE3_inv_year, SP_code, BZE3_DBH_cm), 
                      by = c("plot_ID", "tree_ID", "SP_code")) %>% 
  mutate(DBH_growth_cm = BZE3_DBH_cm - HBI_DBH_cm, 
         age_period = BZE3_inv_year- HBI_inv_year, 
         annual_growth_cm = DBH_growth_cm/age_period) %>% 
  group_by(plot_ID, SP_code) %>% 
  summarize(average_age_period_years = mean(age_period), 
            avg_annual_DBH_growth_cm = mean(annual_growth_cm))


                      

BZE3_trees_5 <- BZE3_trees %>% filter(tree_inventory_status == 5)
tree_inventory_status_5.list <- vector(mode = "list", length = length(BZE3_trees_5$tree_ID))

for (i in 1:length(BZE3_trees_5$tree_ID)) {
  # i = 1
  
  my.plot.id <- BZE3_trees_5[i, "plot_ID"]
  my.tree.id <- BZE3_trees_5[i, "tree_ID"]
  my.tree.spec <- BZE3_trees_5[i, "SP_code"]
  my.inv.year <- BZE3_trees_5[i, "inv_year"]
  inv.year.1 <- HBI_trees %>% 
    filter(plot_ID == my.plot.id & SP_code == my.tree.spec) %>% 
    group_by(plot_ID) %>% 
    summarise(year = mean(inv_year)) %>% 
    dplyr::pull(year)
  
  annual.growth.cm <- growth.df$avg_annual_DBH_growth_cm[growth.df$plot_ID == my.plot.id & growth.df$SP_code == my.tree.spec]
  years.passed.between.inv.1.and.2 <- my.inv.year - inv.year.1
  
  tree_inventory_status_5.list <- as.data.frame(
    rbind(BZE3_trees_5[i,] %>% mutate(new_tree_inventory_status = 1),
          BZE3_trees_5[i,] %>% mutate(DBH_cm = DBH_cm-annual.growth.cm*years.passed.between.inv.1.and.2, 
                                      inv = "HBI",
                                      inv_year =  inv.year.1,
                                      new_tree_inventory_status = 0)))
  
}
# safe list in dataframe
tree_inventory_status_5.df <- as.data.frame(tree_inventory_status_5.list)

# remove trees with inventory status 5 that match with trees in the tree_inventory_status_5.df 
BZE3_trees <- rbind(BZE3_trees %>% 
  anti_join(., tree_inventory_status_5.df, 
            by = c("plot_ID","tree_ID", "inv", "tree_inventory_status" )), 
  # then bind in the corrected data of that tree, with new tree_inventory_status == 1
  tree_inventory_status_5.df %>% filter(inv == "BZE3")
  )

# add tree with corrected diameter and otherwise identical to HBI dataset 
HBI_trees <- rbind(HBI_trees, 
                   tree_inventory_status_5.df %>% filter(inv == "HBI")
)
 




# remove trees that are not part of invenotry anymore ---------------------
# only pass on trees for analysis that are processed through new_invntory_status and labelled as 
# new, repeated or unknown inventory
# we can only do this after the trees with inventory status 4, 5 and 6 have been processed

HBI_trees_update <- HBI_trees %>% filter(new_tree_inventory_status %in% c(0, 1, -9, -1))



# tree inventory status == 2 ----------------------------------------------
# here we  calculate how much volume was removed from the inital plot 
# therefore we have to calculate the volume... which requires: 
  # tapeS species codes
  # DBH at 1.3m for all trees 
  # estimated heights for all trees 
# thus we have to estimate the heights first




