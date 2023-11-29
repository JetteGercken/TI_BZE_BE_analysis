# Thuenen Institute - Bodenschutz und Waldzustand
# Analysis of the forest inventory accompanying the peat land soil inventory
# Trees 


# ----- 0. SETUP ---------------------------------------------------------------
# ----- 0.1. Packages & functions  ---------------------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))


# ----- 0.2. working directory -------------------------------------------------
here::here()
getwd()

#output path momok
momok.out.path <-("O:/a7bze/ZZ_BZE3_Bestand_Auswertung/Momok_out_data/")
momok.out.home <-("output/out_data/out_data_momok/")

# ----- 1. DATA ----------------------------------------------------------------
# ----- 1.1. import ------------------------------------------------------------
# this dataset contains the biomass for each plant compartiment and stand component
LT_RG_DW_MOMOK <- read.delim(file = here(paste0(momok.out.home, "LB_RG_DW_Plot_MoMoK.csv")), sep = ",", dec = ".")
LT_MOMOK <- read.delim(file = here(paste0(momok.out.home, "LB_Plot_MoMoK.csv")), sep = ",", dec = ".")

  

#  1.2. subset dataset ----------------------------------------------------
LT_RG_DW_MOMOK %>%
  filter(compartiment == "ag") %>% 
  select(plot_ID, stand_component, B_t_ha, C_t_ha) %>%
  group_by(plot_ID, stand_component) %>% 
  summarize(mean_B_t_ha = mean(B_t_ha), 
            mean_B_t_ha = mean(C_t_ha)) %>% 
  left_join(LT_MOMOK %>% select(plot_ID, dom_SP), 
            multiple = "all",
            by = "plot_ID") %>% 
  distinct() %>% 
  ggplot()+
  geom_bar(aes(x = stand_component, y = mean_B_t_ha, fill = stand_component), 
           stat = "identity", 
           position = "dodge")+
    facet_wrap(dom_SP~plot_ID)+ 
  ggtitle("mean total aboveground Biomass per hectar per stand component at each MOKOM plot, grouped by dominant species")
    



