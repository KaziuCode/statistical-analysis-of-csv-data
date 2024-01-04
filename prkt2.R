#!/usr/bin/env Rscript
#punkt1
#install.packages("zoo")
#install.packages("dplyr")
#install.packages("e1071")
#install.packages("car")
#install.packages("dunn.test")
#install.packages("corrplot")
#install.packages("car")
#install.packages("ggpubr")
#install.packages("ggplot2")
library(ggplot2)
library(ggpubr)
library(zoo)
library(dplyr)
library(e1071)
library(car)
library(dunn.test)
library(corrplot)
library(car)

#punkt1
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0){
 stop("Nalezy podac argument wejsciowy")
}
data <- read.csv2(file=args[1], header=TRUE, sep = ";")
#data<-read.csv2("przykladoweDane-Projekt.csv",header = T, sep = ";")
#data<-read.csv2("przykladowe2Grupy.csv",header = T, sep = ";")
yt<-names(dplyr::select_if(data,is.numeric))
yk<-names(dplyr::select_if(data,is.factor))
braki<-which(is.na(data),T)
kolumny<-colnames(data)
bb<-braki[,2]
brakii<-braki
brakii[,2]<-kolumny[bb]
print("indeksy komorek zawierajacych braki")
brakii
dane<-group_by(data,grupa) %>%
  mutate_at(yt,na.aggregate)
dane<-as.data.frame(dane)
new_v<-dane[braki]
brakii<-as.data.frame(brakii)
brakii$new_val<-new_v
print("nowe wartosci wprowadzone w miejsce NA")
brakii
for(i in 1:length(yt)){
  cat("\nParametr: ",yt[i],"\n")
  out<-boxplot.stats(dane[[yt[i]]])$out #wartosci odstaje
  cat("wartosci odstajace:",out,"\n")
 ind <- which(dane[[yt[i]]] %in% c(out))#ich indeksy
 cat("indeksy przypadkow z wartosciami odstajacymi: ", ind,"\n")
 
 cat("cale przypadki z wartosciami odstajacymi\n")
 print(dane[ind,])#caly przypadek
}

#punkt2 
a<-dane %>%
  group_by_if(is.factor)%>%
  summarise_if(is.numeric, mean)
write.csv2(a,"srednie.csv", row.names = F)
b<-dane %>%
  group_by_if(is.factor)%>%
  summarise_if(is.numeric, median) 
write.csv2(b,"mediany.csv", row.names = F)
c<-dane %>%
  group_by_if(is.factor)%>%
  summarise_if(is.numeric, IQR) 
write.csv2(c,"IQR.csv", row.names = F)
d<-dane %>%
  group_by_if(is.factor)%>%
  summarise_if(is.numeric, kurtosis) 
write.csv2(d,"kurtozy.csv", row.names = F)
e<-dane %>%
  group_by_if(is.factor)%>%
  summarise_if(is.numeric, sd)
write.csv2(e,"odchylenie_standardowe.csv", row.names = F)
f<-dane %>%
  group_by_if(is.factor)%>%
  summarise_if(is.numeric, skewness)
write.csv2(f,"miary_skoœnosci.csv", row.names = F)
#wizualizcja
for(i in 1:length(yt)){
  
  p<-ggplot(dane, aes_string(x = "grupa", y=yt[i])) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4)
  boxploty<-p+scale_color_brewer(palette="Dark2")
  ggsave(boxploty, file=paste0("boxplot_", yt[i],".png"), width = 14, height = 10, units = "cm")
  
}
#punkt3

for(i in 1:length(yt)){
  

  gestosc<-ggdensity(dane, x = yt[i],
                     color = "grupa", fill = "grupa",
                     palette = "Accent",
                     main = paste("Wykres gêstoœci dla ", yt[i]))
  ggsave(gestosc, file=paste0("Wykres_gestosci_dla_", yt[i],".png"), width = 14, height = 10, units = "cm")
  
rozklad_n<-group_by(dane, grupa) %>%
summarise(
  p.value= shapiro.test(dane[[yt[i]]])$p.value)
rn<-all(rozklad_n$p.value>0.5)


homo<-leveneTest(dane[[yt[i]]],dane$grupa, data=dane)$"Pr(>F)"[1]
homo<-(homo>0.5)

if(length(levels(dane$grupa))>2){
  if(rn & homo){
    apv<-summary(aov(dane[[yt[i]]]~grupa, data = dane))[[1]][["Pr(>F)"]][[1]]
    if(apv < 0.05){
      cat(apv ,  "< 0.05, sa  roznice  pomiedzy grupami dla ",yt[i],"\n" )
     th<-TukeyHSD(aov(dane[[yt[i]]]~grupa, data = dane))$grupa
     th<-as.data.frame(th)
     th <- tibble::rownames_to_column(th, "grupy")
     rozniceth<-which(th$`p adj`<0.05)
     cat("istnieja istotne statystycznie roznice pomiedzy grupami: ", th$grupy[rozniceth],"\n\n")
    }else{
      cat(apv,  "> 0.05, brak  roznic  pomiedzy grupami ",yt[i],"\n\n" )}
    
  }
   



  if(rn==FALSE){

  kpv<-kruskal.test(dane[[yt[i]]],dane$grupa, data=dane)$p.value
  if(kpv < 0.05){
    cat(kpv ,  "< 0.05, sa  roznice  pomiedzy grupami dla ",yt[i],"\n\n" )
dt<-dunn.test(dane[[yt[i]]],dane$grupa)
roznice<-which(dt$P<0.05)
cat("istnieja istotne statystycznie roznice miedzy: ",dt$comparisons[roznice],"\n\n")

  }else{
  cat(kpv,  "> 0.05, brak  roznic  pomiedzy grupami ",yt[i],"\n\n" )}
  
}}
if(length(levels(dane$grupa))==2){

  if(rn & homo){
  ts<-t.test(dane[[yt[i]]]~grupa, data = dane, var.equal = TRUE)$p.value
  if(ts<0.05){print("istnieja znaczace roznice pomiedzy grupami: ",yt[i],"\n\n")}else{cat("nie istnieja znaczace roznice pomiedzy grupami: ", yt[i],"\n\n")}
}

  if(rn & (homo==FALSE)){
we<-t.test(dane[[yt[i]]]~grupa, data = dane, var.equal = FALSE)$p.value
if(we<0.05){print("istnieja znaczace roznice pomiedzy grupami: ",yt[i],"\n\n")}else{cat("nie istnieja znaczace roznice pomiedzy grupami: ", yt[i],"\n\n")}
  }
if(rn==F){
  wi<-wilcox.test(dane[[yt[i]]]~grupa, data = dane, exact = FALSE)$p.value
  if(wi<0.05){print("istnieja znaczace roznice pomiedzy grupami: ",yt[i],"\n\n")}else{cat("nie istnieja znaczace roznice pomiedzy grupami: ", yt[i],"\n\n")}
    
  }
  
}}
chisq.test(dane$grupa,dane$plec)
pvalueChisqPlec <-chisq.test(dane$grupa,dane$plec)$p.value
png(paste("barplot_dla_plci.png"))
barplot(table(dane$plec,dane$grupa) ,ylim = c(0,20) ,
        beside = TRUE,
        xlab = "grupa" ,
        ylab = "plec" ,
        legend = c( "kobieta" ,  "mezczyzna" )
)
text (7.2,16, paste ("p-value ",round (pvalueChisqPlec,  digits = 3)))

#punkt4

#jpeg(filename="korelacje.jpg", width=1800,height = 600)
#par(mfrow = c(1,3))
for (p in 1:length(levels(dane$grupa))) {
  trax<-with(dane,dane[grupa==levels(dane$grupa)[p],])
ww<-cor(trax[,-c(1,2)]) 
for(i in 2:length(yt)){
  for(k in 1:length(yt)){
    if(i != k) {
      if(k < i) {
      cc<-cor.test(trax[[yt[i]]],trax[[yt[k]]],method = "pearson" )$p.value
       if(cc<0.05){istotnosc="istotny"}else if (cc>0.5){istotnosc="nieistotny"}
      if(ww[i,k] > -1 & ww[i,k] < -0.7) {
        print(paste("w grupie",levels(dane$grupa)[p], " zmienne",yt[i], " a ", yt[k],
                    "wystepuje bardzo silna korelacja ujemna, wspó³czynnik korelacji wynosi", ww[i,k],
                    "i wynik jest ",istotnosc," statystycznie"))
       } else if (ww[i,k] > -0.7 & ww[i,k] < -0.5) {
        print(paste("w grupie", levels(dane$grupa)[p], "zmienne", yt[i], "oraz", yt[k],
                    "wystepuje silna korelacja ujemna, wspó³czynnik korelacji wynosi", ww[i,k],
                    "i wynik jest ", istotnosc,"statystycznie"))
      } else if (ww[i,k] > -0.5 & ww[i,k] < -0.3) {
        print(paste("w grupie", levels(dane$grupa)[p], "zmienne", yt[i], "oraz", yt[k],
                    "wystepuje korelacja ujemna o œrednim natê¿eniu, wspó³czynnik korelacji wynosi", ww[i,k],
                    "i wynik jest", istotnosc,"statystycznie"))
      } else if (ww[i,k] > -0.3 & ww[i,k] < -0.2) {
        print(paste("w grupie", levels(dane$grupa)[p], "zmienne", yt[i], "oraz", yt[k],
                    "wystepuje s³aba korelacja ujemna, wspó³czynnik korelacji wynosi", ww[i,k],
                    "i wynik jest", istotnosc,"statystycznie"))
      } else if (ww[i,k] > -0.2 & ww[i,k] < 0.2) {
        print(paste("w grupie", levels(dane$grupa)[p], "Zmienne", yt[i], "oraz", yt[k],
                    "nie s¹ skorelowane,wspó³czynnik korelacji wynosi",ww[i,k],
                    "i wynik jest", istotnosc,"statystycznie"))
      } else if (ww[i,k] > 0.2 & ww[i,k] < 0.3) {
        print(paste("w grupie", levels(dane$grupa)[p], "zmienne", yt[i], "oraz", yt[k],
                    "wystepuje s³aba korelacja dodatnia, wspó³czynnik korelacji wynosi",ww[i,k],
                    "i wynik jest", istotnosc,"statystycznie"))
      } else if (ww[i,k] > 0.3 & ww[i,k] < 0.5) {
        print(paste("w grupie", levels(dane$grupa)[p], "zmienne", yt[i], "oraz", yt[k],
                    "wystepuje dodatnia korelacja œrednim natê¿eniu, wspó³czynnik korelacji wynosi",ww[i,k],
                    "i wynik jest", istotnosc,"statystycznie"))
      } else if (ww[i,k] > 0.5 & ww[i,k] < 0.7) {
        print(paste("w grupie", levels(dane$grupa)[p], "zmienne", yt[i], "oraz", yt[k],
                    "wystepuje silna korelacja dodatnia, wspó³czynnik korelacji wynosi",ww[i,k],
                    "i wynik jest", istotnosc,"statystycznie"))
      } else if (ww[i,k] > 0.7 & ww[i,k] < 1) {
        print(paste("w grupie", levels(dane$grupa)[p], "zmienne", yt[i], "oraz", yt[k],
                    "wystepuje bardzo silna korelacja dodatnia, wspó³czynnik korelacji wynosi",ww[i,k],
                    "i wynik jest", istotnosc,"statystycznie"))
      }
     
      
            korelacja<-ggscatter(dane, x=yt[k],y=yt[i] ,
                                   add = "reg.line" ,conf.int = TRUE,
                                   cor.coef = TRUE,  cor.method = "pearson" ,
                                   color = "grupa" ,  fill  = "grupa" ,
                                   ylab = yt[i] ,
                                   xlab = yt[k])#+geom_smooth(method = lm,formula = y ~ x)
              ggsave(korelacja, file=paste0("kor_", yt[k],"_",yt[i],".png"), width = 14, height = 10, units = "cm")
           
      
}}}}
png(paste("kor_w_grupie",levels(dane$grupa)[p],".png"))
corrplot(ww, type="upper", order="hclust",addCoef.col = "black",tl.col = "black", tl.srt = 45, method = "color",
                   sig.level = 0.001,  insig = "blank", main = levels(dane$grupa)[p],mar=c(0,0,1,0))

}


