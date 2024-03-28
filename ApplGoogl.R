
# Import date -------------------------------------------------------------

setwd("C:\\Users\\Raluca\\OneDrive\\Desktop\\facultate\\micro manageriala\\curs\\proiect")
preturi <- read.csv("date_proiect.csv", header = T, sep = ",", dec = ".")
attach(preturi)


# Grafice tip linie preturi ADBE, GOOGL, SP -------------------------------

par(mfrow=c(1,3))

#grafic trend preturi Adobe
plot(Pret_ADBE,type='l',col='#4567df',main='Trend preturi Adobe',xlab="Timp")

#grafic trend preturi Google
plot(Pret_GOOGL,type='l',col='#8C66F8',main='Trend preturi Google',xlab="Timp")

#grafic trend preturi indice S&P500
plot(Pret_SP,type='l',col='#478963',main='Trend preturi SP',xlab="Timp")


# Histograme preturi ADBE, GOOGL, SP --------------------------------------

#histograma preturi Adobe
hist(Pret_ADBE,main='Histograma Adobe',col='#4567df')
#histograma preturi Google
hist(Pret_GOOGL,main='Histograma Google',col='#8C66F8')
#histograma preturi S&P500
hist(Pret_SP,main='Histograma SP',col='#478963')


# Creare data frame cu rentabilitati si preturi ADBE, GOOGL, SP -----------

#creare data frame preturi si rentabilitati
rent_ADBE <- c()
for(i in 2:length(Pret_ADBE))
{
  rent_ADBE[length(rent_ADBE)+1] <- (Pret_ADBE[i]/Pret_ADBE[i-1])-1
}

rent_GOOGL <- c()
for(i in 2:length(Pret_GOOGL))
{
  rent_GOOGL[length(rent_GOOGL)+1] <- (Pret_GOOGL[i]/Pret_GOOGL[i-1])-1
}

rent_SP <- c()
for(i in 2:length(Pret_SP))
{
  rent_SP[length(rent_SP)+1] <- (Pret_SP[i]/Pret_SP[i-1])-1
}

rent_pret <- data.frame(Pret_ADBE[-1],rent_ADBE, Pret_GOOGL[-1], rent_GOOGL, Pret_SP[-1], rent_SP)
attach(rent_pret)

summary(rent_pret)
write.table(summary(rent_pret),file='StatisticiDescriptive_APPLE_GOOGLE_SP.xls',row.names=FALSE, col.names=TRUE, sep="\t")

# Statistici preturi ADBE, GOOGL, SP --------------------------------------
library(corrplot)
corelatie_pret <- cor(rent_pret[,c(-2,-4,-6)])
corelatie_rent <- cor(rent_pret[,c(-1,-3,-5)])
par(mfrow=c(1,2))
corrplot(corelatie_pret,method="circle",type="full",bg="white",outline=T,shade.col = "white",addCoef.col=T,  tl.col = "BLACK", number.font = 10)
corrplot(corelatie_rent,method="circle",type="full",bg="white",outline=T,shade.col = "white",addCoef.col=T,  tl.col = "BLACK", number.font = 10)
corrplot(corelatie_pret, method='pie', type='lower',shade.col='white',tl.col='black')

#amplitudini, sd si coeficient de variatie pentru preturi ADBE, GOOGL, SP
aADBE <- diff(range(preturi$Pret_ADBE))
sdADBE <- sd(preturi$Pret_ADBE)
cvADBE <- sd(preturi$Pret_ADBE)/mean(preturi$Pret_ADBE)
aGOOGL <- diff(range(preturi$Pret_GOOGL))
sdGOOGL <- sd(preturi$Pret_GOOGL)
cvGOOGL <- sd(preturi$Pret_GOOGL)/mean(preturi$Pret_GOOGL)
aSP <- diff(range(preturi$Pret_SP))
sdSP <- sd(preturi$Pret_SP)
cvSP <- sd(preturi$Pret_SP)/mean(preturi$Pret_SP)

mat_ampl_sd_cv <- matrix(c(aADBE, aGOOGL, aSP, sdADBE,sdGOOGL, sdSP, cvADBE, cvGOOGL, cvSP),byrow=T,ncol=3)
row.names(mat_ampl_sd_cv) <- c('Amplitudine','Standars dev','Coef variatie')
colnames(mat_ampl_sd_cv) <- c('ADBE','GOOGL','S&P')
mat_ampl_sd_cv

#boltire si coef de asimetrie

library(moments)
skSP <- skewness(preturi$Pret_SP)
skGOOGL <- skewness(preturi$Pret_GOOGL)
skADBE <- skewness(preturi$Pret_ADBE)
kSP <- kurtosis(preturi$Pret_SP)
kGOOGL <- kurtosis(preturi$Pret_GOOGL)
kADBE <- kurtosis(preturi$Pret_ADBE)

sk_kurt <- matrix(c(skADBE,skGOOGL,skSP,kADBE,kGOOGL,kSP),byrow=T,nrow=2)
row.names(sk_kurt) <- c('Coeficient asimetrie','Coeficient boltire')
colnames(sk_kurt) <- c('ADBE','GOOGL','S&P')
sk_kurt

#boxploturi preturi ADBE, GOOGL, SP
par(mfrow=c(3,1))
boxplot(preturi$Pret_ADBE,main='Boxplot preturi ADBE',horizontal=T,col='#4567df')
boxplot(preturi$Pret_GOOGL,main='Boxplot preturi GOOGL',horizontal=T,col='#8C66F8')
boxplot(preturi$Pret_SP,main='Boxplot preturi SP',horizontal=T, col='#478963')

#boxplot rentabilitati
boxplot(rent_ADBE,main='Boxplot rentabilitati ADBE',horizontal=T,col='#4567df')
boxplot(rent_GOOGL,main='Boxplot rentabilitati GOOGL',horizontal=T,col='#8C66F8')
boxplot(rent_SP,main='Boxplot rentabilitati SP',horizontal=T, col='#478963')

#ploturi liniare
plot(rent_ADBE,type='l',main='Trenduri rentabilitati ADBE',col='#4567df')
plot(rent_GOOGL,type='l',main='Trenduri rentabilitati GOOGL',col='#8C66F8')
plot(rent_SP,type='l',main='Trenduri rentabilitati SP', col='#478963')

#histograme rentabilitati
hist(rent_ADBE,main='Histograma rentabilitati Adobe',col='#4567df')
hist(rent_GOOGL,main='Histograma rentabilitati Google',col='#8C66F8')
hist(rent_SP,main='Histograma rentabilitati SP',col='#478963')

#calculare ziua in care s-a inregistrat minim si maxim
minADBE <- which.min(preturi$Pret_ADBE)
minGOOGL <- which.min(preturi$Pret_GOOGL)
minSP <- which.min(preturi$Pret_SP)
dataMinADBE <- preturi$Date[minADBE]
dataMinGOOGL <- preturi$Date[minGOOGL]
dataMinSP <- preturi$Date[minSP]

maxADBE <- which.max(preturi$Pret_ADBE)
maxGOOGL <- which.max(preturi$Pret_GOOGL)
maxSP <- which.max(preturi$Pret_SP)
dataMaxADBE <- preturi$Date[maxADBE]
dataMaxGOOGL <- preturi$Date[maxGOOGL]
dataMaxSP <- preturi$Date[maxSP]

min_matrice <- matrix(c(min(preturi$Pret_ADBE),dataMinADBE,min(preturi$Pret_GOOGL),dataMinGOOGL,min(preturi$Pret_SP),dataMinSP),byrow=T,ncol=2)
row.names(min_matrice) <- c('ADBE','GOOGL','SP')
colnames(min_matrice) <- c('Pret minim','Data pret minim')
min_matrice

max_matrice <- matrix(c(max(preturi$Pret_ADBE),dataMaxADBE,max(preturi$Pret_GOOGL),dataMaxGOOGL,max(preturi$Pret_SP),dataMaxSP),byrow=T,ncol=2)
row.names(max_matrice) <- c('ADBE','GOOGL','SP')
colnames(max_matrice) <- c('Pret maxim','Data pret maxim')
max_matrice

#rentabilitati toate 3
plot.ts(rent_pret[c(-1,-3,-5)],col=c('#4567df','#8C66F8','#478963'),main='ADBE, GOOGL, S&P')

