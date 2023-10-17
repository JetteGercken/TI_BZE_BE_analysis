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
  select(plot_ID,  tree_ID ,  tree_inventory_status ,  multi_stem ,
         Dist_cm ,  azi_gon ,age ,  age_meth ,  SP_code , DBH_class ,  Kraft ,
         C_layer , H_dm ,  C_h_dm , D_mm ,   DBH_h_cm ,  DBH_cm ) %>% 
  mutate(inv = "HBI",
         DBH_cm = ifelse(DBH_h_cm == 130, D_mm/10, DBH_BWI(D_mm, DBH_h_cm)))

# create practive dataset from HBI data
BZE3_trees <- as.data.frame(cbind("multi_stem" = c(HBI_trees$multi_stem[1:10]), 
                                  "D_mm" = c(HBI_trees$D_mm[1:10]+10), 
                                  "DBH_class" = c(HBI_trees$DBH_class[1:10]), 
                                  "DBH_h_cm" = c(HBI_trees$DBH_h_cm[1:10]), 
                                  "H_dm"= c(HBI_trees$H_dm[1:10]+10),
                                  "azi_gon" = c(HBI_trees$azi_gon[1:10]), 
                                  "SP_code"= c(HBI_trees$SP_code[1:10]), 
                                  "tree_ID"= c(HBI_trees$tree_ID[1:10]), 
                                  "plot_ID"= c(HBI_trees$plot_ID[1:10]), 
                                  "tree_inventory_status" = c(-9, -1, 0, 1, 2, 3, 4, 5, 6, 7), 
                                  "DBH_cm"= c(HBI_trees$DBH_cm[1:10]), 
                                  "age"= c(HBI_trees$age[1:10]),
                                  "C_layer"= c(HBI_trees$C_layer[1:10]), 
                                  "C_h_dm"= c(HBI_trees$C_h_dm[1:10]), 
                                  "Kraft"= c(HBI_trees$Kraft[1:10]),
                                  "Dist_cm"= c(HBI_trees$Dist_cm[1:10]+20), 
                                  "age_meth"= c(HBI_trees$age_meth[1:10])) )%>% 
  select(plot_ID,  tree_ID ,  tree_inventory_status ,  multi_stem ,
         Dist_cm ,  azi_gon ,age ,  age_meth ,  SP_code , DBH_class ,  Kraft ,
         C_layer , H_dm ,  C_h_dm , D_mm ,   DBH_h_cm ,  DBH_cm ) %>% 
  mutate(inv = "BZE3",
         DBH_cm = ifelse(DBH_h_cm == 130, as.numeric(D_mm)/10, DBH_BWI(as.numeric(D_mm), as.numeric(DBH_h_cm))))

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
tree_inventory_status.list <- vector(mode = "list", length = length(BZE3_trees_9$tree_ID))

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
  tree_inventory_status.list[[i]] <- as.data.frame(cbind(
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
tree_inventory_status.df <- as.data.frame(tree_inventory_status.list)

# join the new tree inventory status in and replace -9s and NAs if possible
BZE3_trees <- BZE3_trees %>% 
  left_join(., tree_inventory_status.df, 
            by = c("plot_ID","tree_ID", "inv" )) %>% 
  mutate(tree_inventory_status = ifelse(tree_inventory_status == -9 | is.na(tree_inventory_status),tree_inventory_status_new, tree_inventory_status)) %>% 
    select(-tree_inventory_status_new)


# tree inventory status == 4 ---------------------------------------------
# for trees that have the status 4 the tree should have not been assessed in the previous inventory 
# what we have to do is find the tree in the previous inventory (so a tree that has the somewhat similar position and tree ID)
# and remove it from the dataset 


# tree inventory status == 1 ---------------------------------------------
# for trees that have the status 1 in the second inventory there should be a tree 
# with the same ID and species and an equal or higher dbh










