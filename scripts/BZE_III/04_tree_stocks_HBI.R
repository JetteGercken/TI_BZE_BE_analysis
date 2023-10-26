# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the national soil inventory
# estimating biomass, carbon and nitrogen stock on single tree level

# ----- 0. SETUP ---------------------------------------------------------------

# ----- 0.1. packages and functions --------------------------------------------
source(paste0(getwd(), "/scripts/00_functions_library.R"))

# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

out.path.BZE3 <- ("output/out_data/out_data_BZE/") 

# ----- 0.3 data import --------------------------------------------------------
# LIVING TREES
# hbi BE dataset: this dataset contains the inventory data of the tree inventory accompanying the second national soil inventory
# here we should actually import a dataset called "HBI_trees_update_2.csv" which contains plot area and stand data additionally to 
# tree data
trees <- read.delim(file = here("output/out_data/out_data_BZE/HBI_trees_update_3.csv"), sep = ",", dec = ",") 
# HBI_trees <- read.delim(file = here("data/input/BZE2_HBI/BI_trees_update_3.csv"), sep = ",", dec = ",", stringsAsFactors=FALSE)





# 0.4 data preparation ---------------------------------------------------------
# colnames HBI
# colnames(HBI_trees) <- c("multi_stem", "D_mm", "DBH_class", "DBH_h_cm", "H_dm",
#                          "azi_gon", "SP_code", "tree_ID", "plot_ID", "tree_inventory_status", 
#                          "DBH_cm", "age", "C_layer", "C_h_dm", "Kraft", "Dist_cm", "age_meth")  
# HBI_trees <- HBI_trees %>% select(plot_ID,  tree_ID ,  tree_inventory_status ,  multi_stem ,
#                                   Dist_cm ,  azi_gon ,age ,  age_meth ,  SP_code , DBH_class ,  Kraft ,  
#                                   C_layer , H_dm ,  C_h_dm , D_mm ,   DBH_h_cm ,  DBH_cm )



# create column for with compartiments
  # here we create a column with compartiments, so that we can apply the tprBiomass fucntion without having to pivot the whola dataset later
  # we eill only create the categories: 
    # stw (=stump wood), 
    # stb (=stump bark), 
    # sw (=solid wood with diameter above 7cm over bark), 
    # sb (=bark of component sw), 
    # fwb (=fine wood incl. bark) 
    # ndl (=needles)


trees <- trees %>% mutate(H_m = as.numeric(H_m))

trees.comp.list <- vector("list", length = length(trees$X))

for (i in 1:length(trees$X)) {
  # select every single row of the dataframe and repeat it as many times as we have compartiments
  df <- trees[i,]
  comp <- as.character(c("stw","stb","sw", "sb", "fwb", "ndl" ))
  # https://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
  df <- cbind(df[rep(seq_len(nrow(df)), each = length(comp)), ], 
              comp)
  trees.comp.list[[i]] <- df
}
trees.comp.final <- rbindlist(trees.comp.list)
trees <- as.data.frame(trees.comp.final)


# 1. calculations ---------------------------------------------------------


# 1.1. biomass aboveground compartiments ----------------------------------


B_kg_tapes <- function(spec, dbh, dbh_h, h, comp){
  
  spp = na.omit(spec);
  Dm = na.omit(as.list(as.numeric(dbh)));
  Hm = na.omit(as.list(as.numeric(dbh_h)));
  Ht = na.omit(as.numeric(h));
  obj <- tprTrees(spp, Dm, Hm, Ht, inv = 4);
  
  bio_kg <- ifelse(comp = "stw", tprBiomass(obj, component = "stw"), 
                   ifelse(comp = "stb", tprBiomass(obj, component="stb"), 
                          ifelse(comp = "sw" , tprBiomass(obj, component="sw"), 
                                 ifelse(comp = "sb", tprBiomass(obj, component="sb"), 
                                        ifelse(comp = "fwb" , tprBiomass(obj, component="fwb"),
                                               ifelse(comp = "ndl" , tprBiomass(obj, component="ndl"),
                                                      NA
                                 ))))))
  
 return(bio_kg)
}


B_kg_tapes(1, 2, 3, 4)
trees[1,] %>% mutate(B_kg_tree = case_when()
                       B_kg_tapes(tpS_ID, DBH_cm, DBH_h_cm/100, H_m, comp))


# data export -------------------------------------------------------------

# HBI dataset including estimated heights
write.csv(HBI_trees_update_3, paste0(out.path.BZE3, paste(unique(HBI_trees_update_3$inv)[1], "trees_update_3", sep = "_"), ".csv"))
