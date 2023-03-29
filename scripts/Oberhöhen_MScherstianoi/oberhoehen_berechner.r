####################################
#### oberhöhen_berechner 1.0 #######
#### Marc Scherstjanoi #############
#### Thünen Institut ###############
#### für Waldökosysteme ############
#### März 2023 #####################
####################################


anzahl_ha <- c(120,91,90,56,121,78,88,78,79,117,109,66,22,98)
hoehe <- c(10,11,13,30,9,24,20,24,25,8,14,33,32,40)
plot_id <- c(1,1,1,1,2,2,2,2,2,3,3,3,3,4)
oberhoehe <- 0

gebiet<-as.data.frame(cbind(anzahl_ha,hoehe,plot_id,oberhoehe))

for (plot in c(unique(gebiet$plot_id)))  #loope durch plots
{
    wald<-gebiet[gebiet$plot_id==plot,] #nur wald des plots mit der entsprechenden id
    wald<-wald[order(wald$hoehe,decreasing=TRUE),] #sortiere nach höhe
    max_b <- min(100,sum(wald$anzahl_ha)) #die höchsten 100, wenn aber weniger da sind, dann die die da sind
    baum<-1
    n_b<-0
    gesamthoehe<-0
    while (n_b < max_b) #breche ab, wenn die höchsten 100 bäume erfasst (oder die die da sind, wenn <100)
    {
        gesamthoehe <- gesamthoehe + wald[baum,]$hoehe*min(100-n_b,wald[baum,]$anzahl_ha) #hoehe der "baum"t-höchsten bäume mal anzahl pro hektar quasi als gewicht in der berechnung
        n_b=n_b+wald[baum,]$anzahl_ha #wie viele bäume waren das bisher
        baum=baum+1 #erst höchster baum, dann zweithöchster baum usw
    }
    gebiet[gebiet$plot_id==plot,]$oberhoehe <- gesamthoehe/max_b # wenn mindestens 100 bäume vorhanden, dann oberhoehe = gesamthoehe/100
}

#test nach korrektheit
(56*30 + 44*13)/100 #=22.52 plot_id 1
(79*25 + 21*24)/100 #=24.79 plot_id 2
(66*33 + 22*32 + 12*14)/100 #=30.5 plot_id 3
98*40/98 #=40 , trivial, plot_id 4

gebiet
