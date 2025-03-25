# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the national soil inventory
# sorting trees according to tree inventory status (baumkennzahl)

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------


source(paste0(getwd(), "/scripts/01_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()

out.path.BZE3 <- paste0(getwd(), "/output/out_data/out_data_BZE/") 
# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# BZE3 BE dataset: this dataset contains the inventory data of the tree inventory accompanying the third national soil inventory
# after they have been sorted into stands according to the forest edge data in script "01_forest_edges_HBI.R" or "01_forest_edges_BZE3.R" and 
# have been filtered for the plot inventory status in "01_00_RG_LT_DW_inventory_plot_status_HBI"
HBI_trees <- read.delim(file = paste0(out.path.BZE3,"HBI_LT_update_1.csv"), sep = ",", dec = ".")
HBI_trees_removed <- read.delim(file = paste0(out.path.BZE3, (HBI_trees$inv)[1], "_LT_removed.csv"), sep = ",", dec = ".")


try({
  BZE3_trees <- read.delim(file =  paste0(out.path.BZE3,"BZE3_LT_update_1.csv"), sep = ",", dec = ".")
  BZE3_trees_removed <- read.delim(file =  paste0(out.path.BZE3, paste0((BZE3_trees$inv)[1], "_LT_removed.csv")), sep = ",", dec = ".")}, 
  silent = T)

# ----- 0.6 harmonising column names & structure  -----------------------------------------------------------------
# creating dataset with information about the concentric sampling circles
data_circle <- data.frame(x0 = c(0,0,0),       # x of centre point of all 3 circles is 0 
                          y0 = c(0,0,0),       # y of centre point of all 3 circles is 0 
                          r0 = c(5.64, 12.62, 17.84), # darius in m
                          rmax = c(30.00, 30.00, 30.00)) # these are the radi of the sampling circuits in m

# complete pre inventory dataset
HBI_trees <- HBI_trees %>% 
  rename(old_tree_inventory_status = tree_inventory_status) %>% 
  # these two columns are meant to prepare for the comming data sorting
  mutate(old_tree_ID = tree_ID, 
         tree_inventory_status = old_tree_inventory_status) 

# this statement stops the whole BZE3 related code from running if there are no BZE3 data
# the if-statments before every loop are constructed for the case that there are BZE3 data but not for the respective tree inventory status
if(exists('BZE3_trees') == T){   #important# you can find the end of the statement by searching " #important# "
  
  # BZE3 tree data  
  BZE3_trees <- BZE3_trees %>% 
    rename(old_tree_inventory_status = tree_inventory_status) %>% 
    # these two columns are meant to prepare for the comming data sorting
    mutate(old_tree_ID = tree_ID, 
           tree_inventory_status = old_tree_inventory_status)
  
  
  
  ## TEST create fake/ practice BZE3/ post dataset from HBI/ pre data
  
  #BZE3_trees$tree_inventory_status[1] <- 5
  # BZE3_trees <- HBI_trees %>%
  #   filter(plot_ID == 50001) %>% 
  #   mutate(inv_year = 2023, 
  #          inv = inv_name(inv_year), 
  #          D_mm = D_mm+10,
  #          H_dm = ifelse(H_dm %in% c(-9, -1), H_dm, (as.numeric(H_dm)+10)), 
  #          dist_cm= dist_cm+20, 
  #          old_tree_inventory_status = case_when(row_number() == 1 ~ -9,
  #                                            row_number() == 2 ~ -1,
  #                                            row_number() == 3 ~ 0,
  #                                            row_number() == 4 ~ 1,
  #                                            row_number() == 5 ~ 2,
  #                                            row_number() == 6 ~ 3,
  #                                            row_number() == 7 ~ 4,
  #                                            row_number() == 8 ~ 5,
  #                                            row_number() == 9 ~ 6,
  #                                            row_number() == 10 ~ 7,
  #                                            TRUE ~ 1), 
  #          tree_inventory_status = old_tree_inventory_status) 
  #   # mutate two new trees to simulate a case of tree_inventory_status == 6
  # BZE3_trees <- rbind(
  #   BZE3_trees,
  #   BZE3_trees %>% filter(old_tree_inventory_status == 6) %>% mutate(tree_ID = 27, SP_code = "gki", azi_gon = azi_gon -1, D_mm = D_mm+300, old_tree_inventory_status = 0, tree_inventory_status = 0),
  #   BZE3_trees %>% filter(old_tree_inventory_status == 6) %>% mutate(tree_ID = 28, SP_code = "gki" , azi_gon = azi_gon +1, D_mm = D_mm+100, old_tree_inventory_status = 0, tree_inventory_status = 0)
  # )
  
  
  
  
  # tree inventory status == -9 ---------------------------------------------
  # this is like NA. 
  # what we can check is, if theres a tree with a similar position (+-10 gon and 20cm distance or so)
  # if so, we can change the inventory ID to 1 - repeated inventory
  # if not we will have to set it to 0 - newly inventorised
  # subset data frot inventory status -9 
  
  BZE3_trees_9 <- BZE3_trees %>% filter(old_tree_inventory_status == -9)
  tree_inventory_status_9.list <- vector(mode = "list", length = length(BZE3_trees_9$tree_ID))
  if(nrow(BZE3_trees_9) != 0){
    for (i in 1:length(BZE3_trees_9$tree_ID)) {
      # i = 1
      
      my.plot.id <- BZE3_trees_9[i, "plot_ID"]
      my.tree.id <- BZE3_trees_9[i, "tree_ID"]
      my.tree.spec <- BZE3_trees_9[i, "SP_code"]
      my.inv <- BZE3_trees_9[i, "inv"]#
      my.inv.year <- BZE3_trees_9[i, "inv_year"]
      my.dbh.cm <- BZE3_trees_9[i, "DBH_cm"]
      
      azi.tree.post <- as.numeric(BZE3_trees_9[i, "azi_gon"])
      dist.tree.post <- as.numeric(BZE3_trees_9[i, "dist_cm"])/100
      x.tree.post <- dist.tree.post*sin(azi.tree.post)       # this is: easting, longitude, RW
      y.tree.post <- dist.tree.post*cos(azi.tree.post)       # this is: northing, latitude, HW 
      
      # select the distance and azimute of the trees of the previous inventory by plot ID 
      # to calcualte the coordiantes of all trees of the plot in the previous inventory
      azi.tree.pre <- HBI_trees$azi_gon[HBI_trees$plot_ID == my.plot.id]
      dist.tree.pre <- HBI_trees$dist_cm[HBI_trees$plot_ID == my.plot.id]/100
      x.tree.pre <- dist.tree.pre*sin(azi.tree.pre)       # this is: easting, longitude, RW
      y.tree.pre <- dist.tree.pre*cos(azi.tree.pre)       # this is: northing, latitude, HW 
      
      # select the row number of the tree point in the HBI (inventory 1, pre) dataframe of the same plot ID,
      # which has the smallest distance to the given tree corodinates from BZE3 (inventory 2, post)
      closest.id <- which.min( distance(x.tree.pre, y.tree.pre, x.tree.post, y.tree.post))
      
      # calculate or select the actual distance, species and dbh between the selected row/ coordiantes of the nearest neighbout canidate from HBI and the given tree from BZE3
      distance.my.tree.and.nearest.neighbour <- distance(x.tree.pre[closest.id], y.tree.pre[closest.id], x.tree.post, y.tree.post)
      species.nearest.neighbour <- HBI_trees$SP_code[HBI_trees$plot_ID == my.plot.id][closest.id]
      dbh.nearest.neighbour <- HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id][closest.id]
      t_id.nearest.neighbour <- HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]
      inv.status.nearest.neighbour <- HBI_trees$old_tree_inventory_status[HBI_trees$plot_ID == my.plot.id][closest.id]
      
      # we can assume its the same tree and they just forgot to give a tree 
      # inventory status number if:
      # the distance is within a range of +/- 50cm, 
      # if the species is identical 
      # if the dbh is lower or equal? 
      # post means it´s the second invenotry
      tree_inventory_status_post <- ifelse(distance.my.tree.and.nearest.neighbour <= 0.5 & 
                                             my.tree.spec == species.nearest.neighbour & 
                                             my.dbh.cm >= dbh.nearest.neighbour & 
                                             my.tree.id == t_id.nearest.neighbour | 
                                             distance.my.tree.and.nearest.neighbour >= 0.5 & 
                                             my.tree.spec == species.nearest.neighbour & 
                                             my.dbh.cm >= dbh.nearest.neighbour & 
                                             my.tree.id == t_id.nearest.neighbour, 1, NA)
      
      # if we actually find a tree in the previous inventroy that fullfills our requirements
      # we have to assign a new inventory status to the "old" (Pre/ first inventory) tree as well. Unless the tree was already subject to a 
      # repeated inventory, we have to assign it to tree status 0 as it must have been the first inventory of my.tree
      # pre means it´s the first inventory
      tree_inventory_status_pre <- ifelse(!is.na(tree_inventory_status_post & inv.status.nearest.neighbour !=1 ), 0, inv.status.nearest.neighbour)
      
      # build dataset that links tree status with plot, tree and inventory ID so the tree remains indentifiable
      tree_inventory_status_9.list[[i]] <- as.data.frame(cbind(
        "plot_ID" = c(my.plot.id, HBI_trees$plot_ID[HBI_trees$plot_ID == my.plot.id][closest.id]),
        "tree_ID" = c(my.tree.id, HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]),
        "inv" = c(my.inv, HBI_trees$inv[HBI_trees$plot_ID == my.plot.id][closest.id]),
        "inv_year" = c(my.inv.year, HBI_trees$inv_year[HBI_trees$plot_ID == my.plot.id][closest.id]),
        "new_tree_inventory_status" = c(tree_inventory_status_post, tree_inventory_status_pre)
      ))
      
      print(ggplot()+ 
              geom_circle(aes(x0 = data_circle$x0, y0 = data_circle$y0, r = data_circle$r0))+
              geom_point(aes(x.tree.pre, y.tree.pre, size = HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id], colour = "pre tree"))+
              geom_point(aes(x.tree.post, y.tree.post, size = my.dbh.cm, color= "post tree", alpha = .3))+ 
              guides(color=guide_legend(title="tree from inv. 2 (post)"))+
              guides(size=guide_legend(title="DBH cm"))+
              geom_text(aes(x.tree.pre, y.tree.pre), 
                        label= HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id],
                        nudge_x=0.45, nudge_y=0.1,check_overlap=T)+
              geom_text(aes(x.tree.post, y.tree.post), 
                        label= BZE3_trees_9$tree_ID[BZE3_trees_9$plot_ID == my.plot.id & BZE3_trees_9$tree_ID == my.tree.id],
                        nudge_x=0.45, nudge_y=0.1, check_overlap=T)
      )
      
    }
    
    # safe list in dataframe
    tree_inventory_status_9.df <- as.data.frame(tree_inventory_status_9.list)
    # https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
    tree_inventory_status_9.df[,c(1,2, 4, 5)] <- lapply(tree_inventory_status_9.df[,c(1,2, 4, 5)], as.integer)
    
    ## update tree_inv_status in BZE daateset if possible
    BZE3_trees <- BZE3_trees %>% 
      # join the new tree inventory status in and replace -9s and NAs if possible
      left_join(., tree_inventory_status_9.df, 
                by = c("plot_ID","tree_ID", "inv", "inv_year")) %>% 
      # this is an update join/ mutate: this part replaces the current inventory status by the new inventory status that was created by the loop if there is one
      mutate(tree_inventory_status = ifelse(old_tree_inventory_status == -9 & !is.na(new_tree_inventory_status) | 
                                              is.na(old_tree_inventory_status) & !is.na(new_tree_inventory_status), 
                                            new_tree_inventory_status, tree_inventory_status)) %>% 
      # remove the column originating from the loop after "tree_inv_status" is updated
      select(-new_tree_inventory_status)
    
    
    ## update tree_inv_status in HBI daateset if possible
    HBI_trees <- HBI_trees %>% 
      left_join(., tree_inventory_status_9.df, 
                by = c("plot_ID","tree_ID", "inv",  "inv_year")) %>% 
      # this is an update join/ mutate: this part replaces the current inventory status by the new inventory status that was created by the loop if there is one
      mutate(tree_inventory_status = ifelse(old_tree_inventory_status == -9 & !is.na(new_tree_inventory_status) | 
                                              is.na(old_tree_inventory_status) & !is.na(new_tree_inventory_status), 
                                            new_tree_inventory_status, tree_inventory_status)) %>% 
      # remove the column originating from the loop after "tree_inv_status" is updated
      select(-new_tree_inventory_status)
    
  }# close if statement that blocks this loop if there are no trees with status 9 
  
  
  # tree inventory status == -1 ---------------------------------------------
  # this is like NA. 
  # what we can check is, if theres a tree with a similar position (+-10 gon and 20cm distance or so)
  # if so, we can change the inventory ID to 1 - repeated inventory
  # if not we will have to set it to 0 - newly inventorised
  # subset data frot inventory status -1
  BZE3_trees_1 <- BZE3_trees %>% filter(old_tree_inventory_status == -1)
  tree_inventory_status_1.list <- vector(mode = "list", length = length(BZE3_trees_1$tree_ID))
  if(nrow(BZE3_trees_1) != 0){
    for (i in 1:length(BZE3_trees_1$tree_ID)) {
      # i = 1
      
      my.plot.id <- BZE3_trees_1[i, "plot_ID"]
      my.tree.id <- BZE3_trees_1[i, "tree_ID"]
      my.tree.spec <- BZE3_trees_1[i, "SP_code"]
      my.inv <- BZE3_trees_1[i, "inv"]
      my.inv.year <- BZE3_trees_1[i, "inv_year"]
      my.dbh.cm <- BZE3_trees_1[i, "DBH_cm"]
      
      azi.tree.post <- as.numeric(BZE3_trees_1[i, "azi_gon"])
      dist.tree.post <- as.numeric(BZE3_trees_1[i, "dist_cm"])/100
      x.tree.post <- dist.tree.post*sin(azi.tree.post)       # this is: easting, longitude, RW
      y.tree.post <- dist.tree.post*cos(azi.tree.post)       # this is: northing, latitude, HW 
      
      # select the distance and azimute of the trees of the previous inventory by plot ID 
      # to calcualte the coordiantes of all trees of the plot in the previous inventory
      azi.tree.pre <- HBI_trees$azi_gon[HBI_trees$plot_ID == my.plot.id]
      dist.tree.pre <- HBI_trees$dist_cm[HBI_trees$plot_ID == my.plot.id]/100
      x.tree.pre <- dist.tree.pre*sin(azi.tree.pre)       # this is: easting, longitude, RW
      y.tree.pre <- dist.tree.pre*cos(azi.tree.pre)       # this is: northing, latitude, HW 
      
      # select the row number of the tree point in the HBI (inventory 1, pre) dataframe of the same plot ID,
      # which has the smallest distance to the given tree corodinates from BZE3 (inventory 2, post)
      closest.id <- which.min( distance(x.tree.pre, y.tree.pre, x.tree.post, y.tree.post))
      
      # calculate or select the actual distance, species and dbh between the selected row/ coordiantes of tzhe nearest neighbout canidate from HBI and the given tree from BZE3
      distance.my.tree.and.nearest.neighbour <- distance(x.tree.pre[closest.id], y.tree.pre[closest.id], x.tree.post, y.tree.post)
      species.nearest.neighbour <- HBI_trees$SP_code[HBI_trees$plot_ID == my.plot.id][closest.id]
      dbh.nearest.neighbour <- HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id][closest.id]
      t_id.nearest.neighbour <- HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]
      inv.status.nearest.neighbour <- HBI_trees$old_tree_inventory_status[HBI_trees$plot_ID == my.plot.id][closest.id]
      
      # we can assume its the same tree and they just forgot to give a tree 
      # inventory status number if:
      # the distance is within a range of +/- 50cm, 
      # if the species is identical 
      #  if the dbh is lower or equal
      tree_inventory_status_post <- ifelse(distance.my.tree.and.nearest.neighbour <= 0.5 & 
                                             my.tree.spec == species.nearest.neighbour & 
                                             my.dbh.cm >= dbh.nearest.neighbour & 
                                             my.tree.id == t_id.nearest.neighbour | 
                                             distance.my.tree.and.nearest.neighbour >= 0.5 & 
                                             my.tree.spec == species.nearest.neighbour & 
                                             my.dbh.cm >= dbh.nearest.neighbour & 
                                             my.tree.id == t_id.nearest.neighbour, 1, NA)
      
      # if we actually find a tree in the previous inventroy that fullfills our requirements
      # we have to assign a new inventory status to this tree as well. Unless the tree was already subject to a 
      # repeated inventory, we have to assign it to tree status 0 as it must have been the first inventory of my.tree
      tree_inventory_status_pre <- ifelse(!is.na(tree_inventory_status_post & inv.status.nearest.neighbour !=1 ), 0, inv.status.nearest.neighbour)
      
      # build dataset that links tree status with plot, tree and inventory ID so the tree remains indentifiable
      tree_inventory_status_1.list[[i]] <- as.data.frame(cbind(
        "plot_ID" = c(my.plot.id, HBI_trees$plot_ID[HBI_trees$plot_ID == my.plot.id][closest.id]),
        "tree_ID" = c(my.tree.id, HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]),
        "inv" = c(my.inv, HBI_trees$inv[HBI_trees$plot_ID == my.plot.id][closest.id]),
        "inv_year" = c(my.inv.year, HBI_trees$inv_year[HBI_trees$plot_ID == my.plot.id][closest.id]),
        "new_tree_inventory_status" = c(tree_inventory_status_post, tree_inventory_status_pre)
      ))
      
      print(ggplot()+ 
              geom_circle(aes(x0 = data_circle$x0, y0 = data_circle$y0, r = data_circle$r0))+
              geom_point(aes(x.tree.pre, y.tree.pre, size = HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id], color = "tree pre" ))+
              geom_point(aes(x.tree.post, y.tree.post, size = my.dbh.cm, color= "tree post", alpha = .3))+ 
              guides(color=guide_legend(title="tree from inv. 2"))+
              guides(size=guide_legend(title="DBH cm"))+
              geom_text(aes(x.tree.pre, y.tree.pre), 
                        label= HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id],
                        nudge_x=0.45, nudge_y=0.1,check_overlap=T)+
              geom_text(aes(x.tree.post, y.tree.post), 
                        label= BZE3_trees_1$tree_ID[BZE3_trees_1$plot_ID == my.plot.id & BZE3_trees_1$tree_ID == my.tree.id],
                        nudge_x=0.45, nudge_y=0.1, check_overlap=T)
      )
      
    }
    # safe list in dataframe
    tree_inventory_status_1.df <- as.data.frame(tree_inventory_status_1.list)
    # https://stackoverflow.com/questions/20637360/convert-all-data-frame-character-columns-to-factors
    tree_inventory_status_1.df[,c(1,2, 4, 5)] <- lapply(tree_inventory_status_1.df[,c(1,2, 4, 5)], as.integer)
    
    # join the new tree inventory status in and replace -9s and NAs if possible
    BZE3_trees <- BZE3_trees %>% 
      left_join(., tree_inventory_status_1.df, 
                by = c("plot_ID","tree_ID", "inv", "inv_year")) %>% 
      # this is an update join/ mutate: this part replaces the new inventory status that was created by the loop
      mutate(tree_inventory_status = ifelse(old_tree_inventory_status == -1 & !is.na(new_tree_inventory_status) | 
                                              is.na(old_tree_inventory_status) & !is.na(new_tree_inventory_status), new_tree_inventory_status, tree_inventory_status)) %>% 
      select(-new_tree_inventory_status)
    
    
    HBI_trees <- HBI_trees %>% 
      left_join(., tree_inventory_status_1.df, 
                by = c("plot_ID","tree_ID", "inv",  "inv_year")) %>% 
      # this is an update join/ mutate: this part replaces the new inventory status that was created by the loop
      mutate(tree_inventory_status = ifelse(old_tree_inventory_status == -1 & !is.na(new_tree_inventory_status) | 
                                              is.na(old_tree_inventory_status) & !is.na(new_tree_inventory_status), new_tree_inventory_status, tree_inventory_status)) %>% 
      select(-new_tree_inventory_status)
  } # close of stemtent that scipts this chunk if there are no trees with inventory status 1
  
  
  
  # tree inventory status == 4 ---------------------------------------------
  # for trees that have the status 4 the tree should have not been assessed in the previous inventory 
  # what we have to do is find the tree in the previous inventory (so a tree that has the somewhat similar position and tree ID)
  # and remove it from the dataset 
  
  BZE3_trees_4 <- BZE3_trees %>% filter(tree_inventory_status == 4)
  tree_inventory_status_4.list <- vector(mode = "list", length = length(BZE3_trees_4$tree_ID))
  if(nrow(BZE3_trees_4) != 0){
    for (i in 1:length(BZE3_trees_4$tree_ID)) {
      # i = 1
      
      my.plot.id <- BZE3_trees_4[i, "plot_ID"]
      my.tree.id <- BZE3_trees_4[i, "tree_ID"]
      my.tree.spec <- BZE3_trees_4[i, "SP_code"]
      my.inv <- BZE3_trees_4[i, "inv"]
      my.dbh.cm <- BZE3_trees_4[i, "DBH_cm"]
      
      azi.tree.post <- as.numeric(BZE3_trees_4[i, "azi_gon"])
      dist.tree.post <- as.numeric(BZE3_trees_4[i, "dist_cm"])/100
      x.tree.post <- dist.tree.post*sin(azi.tree.post)       # this is: easting, longitude, RW
      y.tree.post <- dist.tree.post*cos(azi.tree.post)       # this is: northing, latitude, HW 
      
      # select the distance and azimute of the trees of the previous inventory by plot ID 
      # to calcualte the coordiantes of all trees of the plot in the previous inventory
      azi.tree.pre <- HBI_trees$azi_gon[HBI_trees$plot_ID == my.plot.id]
      dist.tree.pre <- HBI_trees$dist_cm[HBI_trees$plot_ID == my.plot.id]/100
      x.tree.pre <- dist.tree.pre*sin(azi.tree.pre)       # this is: easting, longitude, RW
      y.tree.pre <- dist.tree.pre*cos(azi.tree.pre)       # this is: northing, latitude, HW 
      
      # select the row number of the tree point in the HBI (inventory 1, pre) dataframe of the same plot ID,
      # which has the smallest distance to the given tree corodinates from BZE3 (inventory 2, post)
      closest.id <- which.min( distance(x.tree.pre, y.tree.pre, x.tree.post, y.tree.post))
      
      # calculate or select the actual distance, species and dbh between the selected row/ coordiantes of tzhe nearest neighbout canidate from HBI and the given tree from BZE3
      distance.my.tree.and.nearest.neighbour <- distance(x.tree.pre[closest.id], y.tree.pre[closest.id], x.tree.post, y.tree.post)
      species.nearest.neighbour <- HBI_trees$SP_code[HBI_trees$plot_ID == my.plot.id][closest.id]
      dbh.nearest.neighbour <- HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id][closest.id]
      tree.id.nearest.neighbour <- HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]
      inv.status.nearest.neighbour <- HBI_trees$old_tree_inventory_status[HBI_trees$plot_ID == my.plot.id][closest.id]
      
      # we can assume its the same tree and they just forgot to give a tree 
      # inventory status number if:
      # the distance is within a range of +/- 50cm, 
      # if the species is identical 
      # maybe also  dbh is lower or equal
      # if the tree id is identical
      tree.found.in.pre.dataset <- ifelse(distance.my.tree.and.nearest.neighbour <= 0.5 & 
                                            my.tree.spec == species.nearest.neighbour & 
                                            my.dbh.cm >= dbh.nearest.neighbour & 
                                            my.tree.id == tree.id.nearest.neighbour| 
                                            distance.my.tree.and.nearest.neighbour >= 0.5 & 
                                            my.tree.spec == species.nearest.neighbour & 
                                            my.dbh.cm >= dbh.nearest.neighbour  & 
                                            my.tree.id == tree.id.nearest.neighbour, "yes", "no")
      
      # build dataset that enables to identify the tree in the dataset of the previous inventory (HBI, inventory 1, pre)
      tree_inventory_status_4.list[[i]] <- if(tree.found.in.pre.dataset == "yes"){
        # https://dplyr.tidyverse.org/reference/slice.html 
        as.data.frame(rbind(HBI_trees%>% filter(plot_ID == my.plot.id) %>% slice(closest.id) %>% mutate(tree_inventory_status=NA),
                            BZE3_trees_4[i,]))}else{
                              as.data.frame(rbind(BZE3_trees_4[i,]))
                            }
      
      print(ggplot()+ 
              geom_circle(aes(x0 = data_circle$x0, y0 = data_circle$y0, r = data_circle$r0))+
              geom_point(aes(x.tree.pre, y.tree.pre, size = HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id], color= "pre trees"))+
              geom_point(aes(x.tree.post, y.tree.post, size =my.dbh.cm, color= "post tree", alpha = .3))+ 
              guides(color=guide_legend(title="tree from inv. 2"))+
              guides(size=guide_legend(title="DBH cm"))+
              geom_text(aes(x.tree.pre, y.tree.pre), 
                        label= HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id],
                        nudge_x=0.45, nudge_y=0.1,check_overlap=T)+
              geom_text(aes(x.tree.post, y.tree.post), 
                        label= BZE3_trees_4$tree_ID[BZE3_trees_4$plot_ID == my.plot.id & BZE3_trees_4$tree_ID == my.tree.id],
                        nudge_x=0.45, nudge_y=0.1, check_overlap=T)
      )
      
    }
    # safe list in dataframe
    tree_inventory_status_4.df <- as.data.frame(tree_inventory_status_4.list)
    
    # remove tree that should have not been assessed in previous inventory from both invenotry datasets
    # BZE3: here the tree should be labelled 4 and then has to be assessed again with status 0 
    BZE3_trees <- BZE3_trees %>% 
      anti_join(., tree_inventory_status_4.df, 
                by = c("plot_ID","tree_ID", "inv" ))
    # HBI: the trees that were assessed in the previous inventory have to be removed from that dataset
    HBI_trees <- HBI_trees %>% 
      anti_join(., tree_inventory_status_4.df, 
                by = c("plot_ID","tree_ID", "inv" ))
  } # close if statemetn that blocks this part of code if there are no trees with status 4
  
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
  if(nrow(BZE3_trees_6) != 0){
    for (i in 1:length(BZE3_trees_6$tree_ID)) {
      # i = 1
      
      # select tree details for respective tree i 
      my.plot.id <- BZE3_trees_6[i, "plot_ID"]
      my.tree.id <- BZE3_trees_6[i, "tree_ID"]
      my.tree.spec <- BZE3_trees_6[i, "SP_code"]
      my.inv <- BZE3_trees_6[i, "inv"]
      my.dbh.cm <- BZE3_trees_6[i, "DBH_cm"]
      # calculate coordiantes for tree i 
      azi.tree.post <- as.numeric(BZE3_trees_6[i, "azi_gon"])
      dist.tree.post <- as.numeric(BZE3_trees_6[i, "dist_cm"])/100
      x.tree.post <- dist.tree.post*sin(azi.tree.post)       # this is: easting, longitude, RW
      y.tree.post <- dist.tree.post*cos(azi.tree.post)       # this is: northing, latitude, HW 
      
      
      # select the distance and azimute of the trees of the previous inventory by plot ID 
      # to calcualte the coordiantes of all trees of the plot in the previous inventory
      azi.tree.pre <- HBI_trees$azi_gon[HBI_trees$plot_ID == my.plot.id]
      dist.tree.pre <- HBI_trees$dist_cm[HBI_trees$plot_ID == my.plot.id]/100
      x.tree.pre <- dist.tree.pre*sin(azi.tree.pre)       # this is: easting, longitude, RW
      y.tree.pre <- dist.tree.pre*cos(azi.tree.pre)       # this is: northing, latitude, HW 
      # select the row number of the tree point in the HBI (previous inventory, inventory 1) dataframe 
      # of the same plot ID, which has the smallest distance to the given my.tree corodinates from BZE3 (inventory 2)
      closest.id <- which.min(distance(x.tree.pre, y.tree.pre, x.tree.post, y.tree.post))
      
      # calculate or select the actual distance, species and dbh between the selected row/ coordiantes of tzhe nearest neighbout canidate from HBI and the given tree from BZE3
      distance.my.tree.and.nearest.neighbour.pre <- distance(x.tree.pre[closest.id], y.tree.pre[closest.id], x.tree.post, y.tree.post)
      species.nearest.neighbour.pre <- HBI_trees$SP_code[HBI_trees$plot_ID == my.plot.id][closest.id]
      dbh.nearest.neighbour.pre <- HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id][closest.id]
      tree.id.nearest.neighbour.pre <- HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id][closest.id]
      inv.status.nearest.neighbour.pre <- HBI_trees$tree_inventory_status[HBI_trees$plot_ID == my.plot.id][closest.id]
      # check if the tree we found in HBI/ previous inventory/ inventors 1 is actually fitting with our tree i 
      # we can assume its the same tree and they just forgot to give a tree inventory status number if:
      # the distance is within a range of +/- 50cm, 
      # if the species is identical 
      # maybe also  dbh is lower or equal
      # if the tree id is identical
      my.tree.found.in.pre.dataset <- ifelse(distance.my.tree.and.nearest.neighbour.pre <= 0.5 & 
                                               my.tree.spec == species.nearest.neighbour.pre & 
                                               my.dbh.cm >= dbh.nearest.neighbour.pre &
                                               my.tree.id == tree.id.nearest.neighbour.pre| 
                                               distance.my.tree.and.nearest.neighbour.pre >= 0.5 & 
                                               my.tree.spec == species.nearest.neighbour.pre & 
                                               my.dbh.cm >= dbh.nearest.neighbour.pre  & 
                                               my.tree.id == tree.id.nearest.neighbour.pre, 
                                             "yes", "no") 
      
      # find partner trees in the same inventory dataset (bze3, inventory 2, most recent inventory): 
      # narrow down the closest two trees dataset by selecting from BZE3/ inventory 2/ most recent inventory:  
      # trees of the same plot_ID, 
      # trees with different tree_ID then my.tree.id
      # trees with same species as my.tree.spec
      # trees with inventory status 0
      closest.trees.canidates.post <- BZE3_trees %>% 
        filter(plot_ID == my.plot.id & tree_ID != my.tree.id & SP_code == my.tree.spec & tree_inventory_status == 0 &
                 DBH_cm >= dbh.nearest.neighbour.pre)
      
      # calcualte cartesian coordiantes of all potential partner trees of my.tree at the plot
      azi.all.trees.post <- as.numeric(closest.trees.canidates.post$azi_gon)
      dist.all.trees.post <- as.numeric(closest.trees.canidates.post$dist_cm)/100 # divide by 100 to transform into m 
      x.all.trees.post <- dist.all.trees.post*sin(azi.all.trees.post)       # this is: easting, longitude, RW
      y.all.trees.post <- dist.all.trees.post*cos(azi.all.trees.post)       # this is: northing, latitude, HW 
      # find the tree IDs in the canidates dataset/ current inventory/ inventory 2/ BZE3 dataset 
      # of the trees that are closest/ originating from my.tree (tree i with status 6) 
      # closest
      closest.id.1 <- (as.data.frame(cbind(
        d = distance(x.all.trees.post, y.all.trees.post, x.tree.post, y.tree.post),
        t_id = c(closest.trees.canidates.post$tree_ID))) %>%
          arrange(d))[1,2]
      # second closest
      closest.id.2 <- (as.data.frame(cbind(
        d = distance(x.all.trees.post, y.all.trees.post, x.tree.post, y.tree.post),
        t_id = c(closest.trees.canidates.post$tree_ID))) %>%
          arrange(d))[2,2]
      # select the info of the two clostest trees in one dataframe  
      closest.trees.df <- BZE3_trees %>% 
        filter(plot_ID == my.plot.id & tree_ID %in% c(closest.id.1, closest.id.2)) 
      # then we implement the critetia that the diameter of these geofrafically close 
      # select the tree among the two closest trees in BZE3/invenotry 2/ recent inventory 
      # that has the most similar diameter to the one of the tree from HBI/ inventory / previous inventory
      closest.tree.post.df <- closest.trees.df[which.min(abs(closest.trees.df$DBH_cm - dbh.nearest.neighbour.pre)),]
      
      
      # build dataset that enables to identify partner tree of my.tree in the previous inventory, my.tree, 
      # and closest.tree in most recent inventory dataset that 
      tree_inventory_status_6.list[[i]] <- if(tree.found.in.pre.dataset == "yes"){
        as.data.frame(rbind(
          # partner tree of my.tree in previous inventory (HBI/ inventory 1)
          HBI_trees %>% filter(plot_ID == my.plot.id) %>% slice(closest.id) %>% 
            # to create a proper pair we have to manipulate the inventory status in the previous inventory since only trees with the 
            # inventory status 0 or 1 will be passed on for the stock calcualtions
            # so if there was actually a partner tree found in the post dataset and the 
            # current tree inventory status is not 1 (so the pre inventory wasn´t already a repetition, which won´t be the case for HBI but maybe for subsequent inventories)
            # we havae to set the new_tree_inventory_status in the previous dataset to 0
            mutate(tree_inventory_status = ifelse(!is.na(closest.tree.post.df$tree_inventory_status) & 
                                                    inv.status.nearest.neighbour.pre !=1 , 0, 
                                                  inv.status.nearest.neighbour),
                   # we have to change the tree ID to the one of the potential partner tree 
                   # originating from my.tree that´s still labled status == 0 in the current inventory dataset
                   old_tree_ID = tree_ID,
                   tree_ID = closest.tree.post.df$tree_ID, 
                   tree_type_status_6 = "partner_tree_pre"),
          # my.tree row --> this one has to be removed later 
          BZE3_trees_6[i,] %>% mutate(old_tree_ID = tree_ID, 
                                      tree_type_status_6 = "my_tree_post"),
          # newly inventoried tree that most likely originates from my.tree and matches best with partner tree
          # --> here we have to change the new tree ID to 1, as the tree has a partner tree in 
          closest.tree.post.df %>% mutate(tree_inventory_status = 1, 
                                          old_tree_ID = tree_ID, 
                                          tree_type_status_6 = "clostest_tree_post")
        ))}else{as.data.frame(rbind(BZE3_trees_6[i,]))}
      
      print(ggplot()+ 
              geom_circle(aes(x0 = data_circle$x0, y0 = data_circle$y0, r = data_circle$r0))+
              geom_point(aes(x.tree.pre, y.tree.pre, size = HBI_trees$DBH_cm[HBI_trees$plot_ID == my.plot.id]))+
              geom_point(aes(x.tree.post, y.tree.post, size =my.dbh.cm, color= "my tree", alpha = .3))+ 
              geom_point(aes(x.all.trees.post, y.all.trees.post, size = closest.trees.canidates.post$DBH_cm, color = "BZE3 nearest neighbours", alpha = 0.3))+
              guides(color=guide_legend(title="tree from inv. 2"))+
              guides(size=guide_legend(title="DBH cm"))+
              geom_text(aes(x.tree.pre, y.tree.pre), 
                        label= HBI_trees$tree_ID[HBI_trees$plot_ID == my.plot.id],
                        nudge_x=0.45, nudge_y=0.1,check_overlap=T)+
              geom_text(aes(x.tree.post, y.tree.post), 
                        label= BZE3_trees_6$tree_ID[BZE3_trees_6$plot_ID == my.plot.id & BZE3_trees_6$tree_ID == my.tree.id],
                        nudge_x=0.45, nudge_y=0.1, check_overlap=T)+
              geom_text(aes(x.all.trees.post, y.all.trees.post), 
                        label= closest.trees.canidates.post$tree_ID,
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
                                      filter(tree_type_status_6 %in% c("my_tree_post", "clostest_tree_post")),
                                    by = c("plot_ID", c("tree_ID" = "old_tree_ID") , "inv" )), 
                        tree_inventory_status_6.df %>% 
                          filter(tree_type_status_6 == "clostest_tree_post") %>% 
                          select(-c(tree_type_status_6))) %>% 
      arrange(plot_ID, tree_ID)
    
    
    HBI_trees <- rbind(HBI_trees %>% 
                         anti_join(., tree_inventory_status_6.df %>% filter(tree_type_status_6 == "partner_tree_pre"), 
                                   by = c("plot_ID",c("tree_ID" = "old_tree_ID"), "inv" )), 
                       tree_inventory_status_6.df %>% 
                         filter(tree_type_status_6 == "partner_tree_pre") %>% 
                         select(-c(tree_type_status_6))) %>% 
      arrange(plot_ID, tree_ID)
  } # close if statemetn that blocks this part of code if there are no trees with status 6
  
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
                           filter(tree_inventory_status %in% c(0, 1, -9))%>% 
                           rename(HBI_DBH_cm = DBH_cm) %>% 
                           rename(HBI_inv_year = inv_year) %>% 
                           select(plot_ID, tree_ID, HBI_inv_year, SP_code, HBI_DBH_cm), 
                         # select trees that are repeatedly inventory, or unknown status
                         BZE3_trees %>% 
                           filter(tree_inventory_status %in% c(1)) %>% 
                           rename(BZE3_DBH_cm = DBH_cm) %>% 
                           rename(BZE3_inv_year = inv_year) %>% 
                           select(plot_ID, tree_ID, BZE3_inv_year, SP_code, BZE3_DBH_cm) %>% arrange(plot_ID, tree_ID), 
                         by = c("plot_ID", "tree_ID", "SP_code")) %>% 
    mutate(DBH_growth_cm = BZE3_DBH_cm - HBI_DBH_cm, 
           age_period = BZE3_inv_year- HBI_inv_year, 
           annual_growth_cm = DBH_growth_cm/age_period) %>% 
    group_by(plot_ID, SP_code) %>% 
    summarize(average_age_period_years = mean(na.omit(age_period)), 
              avg_annual_DBH_growth_cm = mean(na.omit(annual_growth_cm)))
  
  
  
  
  BZE3_trees_5 <- BZE3_trees %>% filter(tree_inventory_status == 5)
  tree_inventory_status_5.list <- vector(mode = "list", length = length(BZE3_trees_5$tree_ID))
  if(nrow(BZE3_trees_5) != 0){
    for (i in 1:length(BZE3_trees_5$tree_ID)) {
      # i = 1
      
      my.plot.id <- BZE3_trees_5[i, "plot_ID"]
      my.tree.id <- BZE3_trees_5[i, "tree_ID"]
      my.tree.spec <- BZE3_trees_5[i, "SP_code"]
      my.inv.year <- BZE3_trees_5[i, "inv_year"]
      inv.year.pre <- HBI_trees %>% 
        filter(plot_ID == my.plot.id & SP_code == my.tree.spec) %>% 
        group_by(plot_ID) %>% 
        summarise(year = mean(inv_year)) %>% 
        dplyr::pull(year)
      
      annual.growth.cm <- growth.df$avg_annual_DBH_growth_cm[growth.df$plot_ID == my.plot.id & growth.df$SP_code == my.tree.spec]
      years.passed.between.inv.pre.and.post <- my.inv.year - inv.year.pre
      
      tree_inventory_status_5.list <- as.data.frame(
        rbind(BZE3_trees_5[i,] %>% mutate(tree_inventory_status = 1),
              BZE3_trees_5[i,] %>% mutate(DBH_cm = DBH_cm-annual.growth.cm*years.passed.between.inv.pre.and.post, 
                                          inv = "HBI",
                                          inv_year =  inv.year.pre,
                                          tree_inventory_status = 0)))
      
    }
    # safe list in dataframe
    tree_inventory_status_5.df <- as.data.frame(tree_inventory_status_5.list)
    
    # remove trees with inventory status 5 that match with trees in the tree_inventory_status_5.df 
    BZE3_trees <- rbind(BZE3_trees %>% 
                          anti_join(., tree_inventory_status_5.df, 
                                    by = c("plot_ID","tree_ID", "inv", "inv_year", "old_tree_inventory_status" )), 
                        # then bind in the corrected data of that tree, with new tree_inventory_status == 1
                        tree_inventory_status_5.df %>% filter(inv == "BZE3")
    )
    
    # add tree with corrected diameter and otherwise identical to HBI dataset 
    HBI_trees <- rbind(HBI_trees, 
                       tree_inventory_status_5.df %>% 
                         filter(inv == "HBI") %>% 
                         arrange(plot_ID, tree_ID)
    )
    
    
  } # close if statemetn that blocks this part of code if there are no trees with status 5
  
  
  # remove trees that are not part of inventory anymore ---------------------
  # only pass on trees for analysis that are processed through new_invntory_status and labelled as 
  # new, repeated or unknown inventory
  # we can only do this after the trees with inventory status 4, 5 and 6 have been processed
  
  BZE3_trees_update_02 <- BZE3_trees %>% filter(tree_inventory_status %in% c(0, 1))
  BZE3_trees_removed <- if(exists('tree_type_status_6')) {plyr::rbind.fill(
    # dataset with previously removed trees form other processing steps 
    BZE3_trees_removed, 
    # trees whose inventory status is not "new" or "repeated"
    BZE3_trees %>% 
      filter(!(tree_inventory_status %in% c(0, 1))) %>% 
      mutate(rem_reason = "LT excluded during tree inventory status sorting (Baumkennzahl)"),
    # trees that were removed because they have the old inv status 6
    tree_inventory_status_6.df %>%  
      filter(tree_type_status_6 %in% c("my_tree_post") & old_tree_inventory_status == 6) %>%
      mutate(rem_reason = "LT excluded during tree inventory status sorting (Baumkennzahl)"))
  }else{
    plyr::rbind.fill(
      # dataset with previously removed trees form other processing steps 
      BZE3_trees_removed, 
      # trees whose inventory status is not "new" or "repeated"
      BZE3_trees %>% 
        filter(!(tree_inventory_status %in% c(0, 1))) %>% 
        mutate(rem_reason = "LT excluded during tree inventory status sorting (Baumkennzahl)")
    )}
  
} ## close if statement that blocks whole code if BZE3 data don´t exist #important#

# HBI Update
HBI_trees_update_02 <- HBI_trees %>% filter(tree_inventory_status %in% c(0, 1, -9, -1))
# if there are trees to exclude, add them to the excluded tree dataset: 
if(isTRUE(
  nrow(HBI_trees %>% filter(!(tree_inventory_status %in% c(0, 1, -9, -1)))) >0) == T){
  HBI_trees_removed <-  plyr::rbind.fill(HBI_trees_removed, 
                                         HBI_trees %>% 
                                           filter(!(tree_inventory_status %in% c(0, 1, -9, -1))) %>% 
                                           mutate(rem_reason = "LT excluded during tree inventory status sorting (Baumkennzahl)")
  )}else{
    HBI_trees_removed <- HBI_trees_removed}

# export data -------------------------------------------------------------
write.csv(HBI_trees_update_02, paste0(out.path.BZE3, paste(unique(HBI_trees_update_02$inv)[1], "LT", "update", "2", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(HBI_trees_removed, paste0(out.path.BZE3, paste(unique(HBI_trees_update_02$inv)[1], "LT", "removed", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")

if(exists('BZE3_trees') == T){
  write.csv(BZE3_trees_update_02,paste0(out.path.BZE3, paste(unique(BZE3_trees_update_02$inv)[1], "LT", "update","2", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(BZE3_trees_removed, paste0(out.path.BZE3, paste(unique(BZE3_trees_update_02$inv)[1], "LT", "removed", sep = "_"), ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
}## close if statement that blocks whole code if BZE3 data don´t exist



stop("this is where LT inventory sorting HBI BZE3 ends")
# write.csv2(BZE3_trees, paste0(out.path.BZE3.BZE3, paste(unique(BZE3_trees$inv)[1], "LT", "update","0", "demo", sep = "_"), ".csv"))
