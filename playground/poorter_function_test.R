# Poorter function
install.packages("here")
library("here")


# set working directory ---------------------------------------------------
here::here()

# ----- 0.1. Packages & functions  ---------------------------------------------------------
source(paste0(getwd(), "/scripts/00_00_functions_library.R"))

# import data -------------------------------------------------------------
HBI_RG_below_1 <- read.delim(file = here("playground/HBI_RG_below_1.csv"), sep = ";", dec = ",") 


# functions ---------------------------------------------------------------
# we need the nthroot to transform the whd-to-h equation
# https://stackoverflow.com/questions/58447310/how-to-use-the-nthroot-function-in-r
nthroot = function(x,n) {
  (abs(x)^(1/n))*sign(x)
}

# input biomass -----------------------------------------------------------
# we have two option to calculate the input biomass: 
#    - Wolff et al. --> wolff.below.1m
#    - National Greenhousegas invenory for trees < 1.3m height --> GHGI_aB_Hb1.3

wolff.below.1m <- function(h, spec_wolff, compartiment){
  # h           is column H_cm
  # spec_wolff  is column RG_Wolff_bio
  # compartiment is in"switch" but to use it as input for the poorter functions one should choose "stem"
  
  a.wdh <- c(BAH = 4.3239, BI = 9.50205, BU= 4.8909, 
         VB = 6.2529, EI = 8.2332, ES =  3.4668, FKD = 10.26,  #in BWI: SLB; FKD = fauliger Kreuzdorn --> Faulbaum --> Rhamnus frangula
         FI = 3.674, GIN = 12.322,  # Ginster = GIN
         HOL =  5.5999,  # holunder
         KI =  7.9661);
  b.wdh <- c(BAH = 1.2531, BI = 1, BU = 1.1404, VB = 1.0844, EI = 1, 
         ES = 1.3483, FKD = 1.0269,  #in BWI: SLB; FKD = fauliger Kreuzdorn --> Faulbaum --> Rhamnus frangula
         FI = 1.0905 , GIN = 1, # Ginster
         HOL =  1.1832,
         KI = 0.9366);
  # diameter at the stem base (Wurzelhalsdurchmesser) in mm
  whd = (nthroot((h/a.wdh[spec_wolff]), b.wdh[spec_wolff]));  
  
  # parameters for total abovground biomass
  a <- c(BAH = -4.116664, BI = -4.374745, BU= -5.329977, 
         VB = - 5.511373, EI = -6.890656, ES = -5.980901,
         FKD = -5.805027, FI = -4.365029, GIN = -5.007328, 
         HOL = -5.596683, KI = -4.054296);
  b <- c(BAH = 2.103417, BI = 1.952172, BU = 1.504128, 
         VB = 1.102974, EI = 0.992884, ES = 1.042600, 
         FKD = 1.268980, FI = 1.873336, GIN = 1.475571, 
         HOL = 1.133249, KI = 1.586179);
  c  <- c(BAH = 0.499551, BI = 0.731565, BU = 1.182288, 
          VB = 1.326973, EI = 1.769866, ES = 1.519227, 
          FKD = 1.293988, FI = 0.977148, GIN = 1.069508, 
          HOL = 1.366231, KI = 0.911674);
  # paremeters for branch + stem compartimen
  d <- c(BAH = -5.255099, BI = 4.586260, BU = -6.160292, 
         VB = -6.050794, EI = -7.338351, ES = -6.623514, 
         FKD = -6.678752, FI = -5.295721, GIN = -5.329752, 
         HOL =  -6.697415, KI = -6.221465);
  e <- c(BAH = 2.055909, BI = 2.317019, BU = 1.719560, 
         VB = 1.218037, EI = 1.100074, ES = 1.241859, 
         FKD = 1.709183, FI = 1.905118, GIN = 1.568195, 
         HOL = 1.685368, KI = 1.506163);
  f <- c(BAH = 0.729275, BI = 0.495968, BU = 1.204329, 
         VB = 1.361229, EI = 1.739085, ES = 1.496878, 
         FKD = 1.231672, FI = 1.044837, GIN = 1.079322, 
         HOL = 1.207866, KI = 1.327712); 
  # paremeters for folliage compartiment
  x <- c(BAH = -3.729638, BI =  -6.108247, BU = -5.385717, 
         VB = -7.216398, EI = -8.201379, ES = -6.584228, 
         FKD =  -5.943471, FI = -4.823170, GIN =  -6.240936,
         HOL =  -4.956678, KI = -3.179742);
  y <- c(BAH = 2.350116, BI = 1.369987, BU = 1.260222, 
         VB = 0.497185, EI = 0.900570, ES = 0.877136, 
         FKD = 0.875969, FI = 1.873277, GIN = 0.701319, 
         HOL = 0.486142, KI = 1.767087);
  z <- c(BAH = -0.067977, BI = 1.175875, BU = 0.945780, 
         VB = 1.581494, EI = 1.833317, ES = 1.42240, 
         FKD = 1.193355, FI = 0.884113, GIN = 1.109381, 
         HOL = 1.248850, KI = 0.392809);
  
  # unfortunately the publication does not display which unit the output biomass has
  switch(
    compartiment,
    "ag" = exp(a[spec_wolff])*whd^b[spec_wolff]*h^c[spec_wolff],
    "stem" = exp(d[spec_wolff])*whd^e[spec_wolff]*h^f[spec_wolff],
    "foliage" = exp(x[spec_wolff])*whd^y[spec_wolff]*h^z[spec_wolff]
  )
  
}



## above ground biomass for trees <1.3m GHGI (Nationale Treibhausgasberichterstattung, equation: 6, coefficient table: 4)
GHGI_aB_Hb1.3 <- function(spec, h){  # here instead of species group i´ll link the formula to a column with he categories broadleafed and coniferous trees
  # spec is in column "LH_NH" (coniferous or broadleafed tree)
  # h is in column H_m (height in meters)
  b0 <- c(NB = 0.23059, LB = 0.04940);
  b1 <- c(NB = 2.20101, LB = 2.54946);
  # aboveground biomass in kg
  return(b0[spec]*h^b1[spec])
}

# poorter -----------------------------------------------------------------
# this function uses poorters root-to-shoot biomass functions transformed by quadratic function
Poorter_quadr_func <- function(ag.kg, spec, compartiment){ # instead of the species we have to put NH_LH here
  # spec is in column "LH_NH" (coniferous or broadleafed tree)
  # ag is the aboveground biomass or stem biomass calculated with GHGI_aB_Hb1.3 or 
  # compartiment can be found in switch function 
  
  # equation to transform aboveground into belowground biomass : stem:root-ratio
  # quadratische ergänzung der Funktion: stem = a + b1*root + b2*root^2
  # sten = b2*root^2 + b1*root + a
  # stem = a*root^2 + b*root + c
  # 0 = a*root^2 + b*root + c-y
  # c = c-y
  # (-b-sqrt(b^2-4*a*c))/2*a = x1
  # (-b + sqrt(b^2-4*a*c))/2*a = x1
  # https://www.mathepanik.de/Klassen/Klasse_10/Lektion_Kl_10_L_parabeln_gleichung_loesen.php
  c <- c(NB = -0.070, LB = -0.097);   # a
  b <- c(NB = 1.236, LB = 1.071);     # b1  
  a <- c(NB = -0.0186, LB = 0.01794); # b2
  # 10-log of belowground biomass in g (*1000)
  ag_g <- ag.kg*1000;
  # withdraw y from c to create a function that equals to 0 so we apply the quadratic function
  cy <- as.data.frame(c[spec]-log10(ag_g))[,1];
  # calculate two possible results for the biomass at the given y
  log.bg.x1 = (-b[spec]-sqrt(b[spec]^2-4*abs(a[spec])*abs(cy)))/(2*a[spec]);
  log.bg.x2 = (-b[spec]+sqrt(b[spec]^2-4*abs(a[spec])*abs(cy)))/(2*a[spec]);
  # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
  # log_a(b) = c ---> b = a^c
  # b) transform leaf biomass in g into kg by dividing by 1000
  bg.kg.x1 = as.data.frame((10^log.bg.x1)/1000)[,1]
  bg.kg.x2 = as.data.frame((10^log.bg.x2)/1000)[,1]
  ag_minus_x1 = ag.kg - bg.kg.x1
  ag_minus_x2 = ag.kg - bg.kg.x2
  
  # if x1 is lower then zero while x2 is higher then zero but below the stem mass choose x2, if not choose x1
  bg_bio_kg = ifelse(bg.kg.x1 >= 0 & ag_minus_x1 < ag_minus_x2, bg.kg.x1, 
                     ifelse(bg.kg.x2 >= 0 & ag_minus_x2 < ag_minus_x1, bg.kg.x2, 
                            NA))
  
  # equation to transform belowground into foliage biomass : leaf:root-ratio
  bg_g <- bg_bio_kg*1000;             # belowground biomass in g (*1000)
  a1 <- c(NB = 0.243, LB =  0.090);
  b1 <- c(NB =  0.924, LB =  0.889);
  b2 <- c(NB = -0.0282, LB = -0.0254);
  log.10.f_bio <- a1[spec]+ b1[spec]* log10(bg_g)+ b2[spec]*log10(bg_g)^2;
  # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
  # log_a(b) = c ---> b = a^c
  # b) transform leaf biomass in g into kg by dividing by 1000
  f_bio_kg <- (10^log.10.f_bio)/1000;
  
  # equation to transform root into stem biomass
  a3 <- c(NB = -0.070, LB = -0.097);   # a
  b3 <- c(NB = 1.236, LB = 1.071);     # b1  
  b4 <- c(NB = -0.0186, LB = 0.01794); # b2
  # 10-log of belowground biomass in g (*1000)
  log.10.stem_bio <- a3[spec]+ b3[spec]*log10(bg_g) + b4[spec]*log10(bg_g)^2;
  # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
  # log_a(b) = c ---> b = a^c
  # b) transform leaf biomass in g into kg by dividing by 1000
  stem_bio_kg <- (10^log.10.stem_bio)/1000;
  
  switch(compartiment, 
         "bg" = bg_bio_kg, 
         "foliage" = f_bio_kg, 
         "stem" = stem_bio_kg, 
         "x1" = bg.kg.x1, 
         "x2" = bg.kg.x2)
}


# this function uses poorters root-to-shoot biomass functions transformed by completing the square
Poorter_quadr_compl <- function(ag, spec, compartiment){ # instead of the species we have to put NH_LH here
    # equation to transform aboveground into belowground biomass : stem:root-ratio
    # quadratische ergänzung der Funktion: stem = a + b1*root + b2*root^2
    # sten = b2*root^2 + b1*root + a
    # stem = a*root^2 + b*root + c
    # https://www.mathepanik.de/Klassen/Klasse_10/Lektion_Kl_10_L_parabeln_gleichung_loesen.php
    c <- c(NB = -0.070, LB = -0.097);   # a
    b <- c(NB = 1.236, LB = 1.071);     # b1  
    a <- c(NB = -0.0186, LB = 0.01794); # b2
    # 10-log of belowground biomass in g (*1000)
    ag_g <- log10(ag*1000)
    # implement trasformed funtion 
    log.10.bg_bio = sqrt(((ag_g-c[spec])/a[spec])+ (((b[spec]/a[spec])/2)^2))+ (b[spec]/a[spec])/2
    # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
    # log_a(b) = c ---> b = a^c
    # b) transform leaf biomass in g into kg by dividing by 1000
    bg_bio_kg <- (10^log.10.bg_bio)/1000; 
    
    # equation to transform belowground into foliage biomass : leaf:root-ratio
    bg_g <- log10(bg_bio_kg*1000);             # 10-log of belowground biomass in g (*1000)
    a1 <- c(NB = 0.243, LB =  0.090);
    b1 <- c(NB =  0.924, LB =  0.889);
    b2 <- c(NB = -0.0282, LB = -0.0254);
    log.10.f_bio <- a1[spec]+ b1[spec]* bg_g+ b2[spec]*(bg_g)^2;
    # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
    # log_a(b) = c ---> b = a^c
    # b) transform leaf biomass in g into kg by dividing by 1000
    f_bio_kg <- (10^log.10.f_bio)/1000;
    
    
    # equation to transform root into stem biomass
    a3 <- c(NB = -0.070, LB = -0.097);   # a
    b3 <- c(NB = 1.236, LB = 1.071);     # b1  
    b4 <- c(NB = -0.0186, LB = 0.01794); # b2
    # 10-log of belowground biomass in g (*1000)
    log.10.stem_bio <- a3[spec]+ b3[spec]*bg_g + b4[spec]*(bg_g)^2;
    # a) backtranform  logarithm: https://studyflix.de/mathematik/logarithmus-aufloesen-4573
    # log_a(b) = c ---> b = a^c
    # b) transform leaf biomass in g into kg by dividing by 1000
    stem_bio_kg <- (10^log.10.stem_bio)/1000;
    
    switch(compartiment, 
           "bg" = bg_bio_kg, 
           "foliage" = f_bio_kg, 
           "stem" = stem_bio_kg)
}




view(HBI_RG_below_1 %>% 
  mutate(
    ag_bio_kg = GHGI_aB_Hb1.3(LH_NH, H_m),
  bg_kg_x1 = Poorter_quadr_func(ag_bio_kg, LH_NH, compartiment = "x1"), 
  bg_kg_x2 = Poorter_quadr_func(ag_bio_kg, LH_NH, compartiment = "x2"), 
  bg_kg = Poorter_quadr_func(ag_bio_kg, LH_NH, compartiment = "bg"),
  x_choosen = ifelse(bg_kg == bg_kg_x1, "x1", 
                     ifelse(bg_kg == bg_kg_x2, "x2", NA)) 
  ) %>% 
    filter( bg_kg_x1 <0 & bg_kg_x2 <0)
  )
