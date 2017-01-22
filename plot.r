install.packages("zoo", repo="https://cran.univ-paris1.fr/")
library(zoo)

fed = read.csv("FEDFUNDS.csv",header=TRUE,sep=",")
fed$DATE = as.Date(fed$DATE,format="%Y-%m-%d")
plot(fed,type="l",ylim=c(0,22),ylab="Taux directeur (%)",xlab=NA,lwd=2)
grid(0,NULL,col="gray")

# De 1986 à 1993
plot(fed,type="l",ylim=c(0,10),xlim=c(as.Date("1986-01-01"),as.Date("1993-01-01")),ylab="Taux directeur (%)",xlab=NA,lwd=2,xaxt="n")
sequence = seq(as.Date("1986-01-01", format="%Y-%m-%d"),as.Date("1993-01-01", format="%Y-%m-%d"), by = "1 year")
lb = format(sequence, "%Y")
axis(side = 1, at = sequence, labels = lb, las = 2)
grid(0,NULL,col="gray")


croissance = read.csv("DP_LIVE_19012017220247244.csv",header=TRUE,sep=",")
croissance.usa = croissance[croissance$LOCATION=="USA",-1]
croissance.usa = croissance.usa[30:48,]
croissance.usa$TIME = as.Date(croissance.usa$TIME)
plot(croissance.usa$TIME, croissance.usa$Value,ylab="Pourcentage",xlab=NA,type="b",lwd=2)
grid(0,NULL,col="gray")
abline(h=mean(croissance.usa$Value),lty="dashed",lwd=2)
legend(croissance.usa$TIME[15],4,c("Croissance","Moyenne"),lty=c("solid","dashed"),lwd=c(2,2))

# add shading 2008 crisis

# Notre analyse sur 1988-2016
debut = "1988-01-01"
fin = "2015-01-01"
fed.recent = fed[which(fed$DATE==debut):which(fed$DATE==fin),]
plot(fed.recent,type="l",lwd=2,ylim=c(0,12))

rho = 3.5
theta_y = 0.5
theta_pi = 0.5
pi_cible = 2
# inflation
inflation = read.csv("FPCPITOTLZGUSA.csv",header=TRUE,sep=",")
inflation$DATE = as.Date(inflation$DATE,format="%Y-%m-%d")
inflation.recent = inflation[which(inflation$DATE==debut):which(inflation$DATE==fin),]
inflation.mois = rep(inflation.recent$FPCPITOTLZGUSA, each=12)
inflation.lissee = rep(0,length(inflation.mois))
inflation.lissee[1] = inflation.mois[1]
for(i in 2:length(inflation.mois)) {
	inflation.lissee[i] = mean(c(inflation.mois[i],inflation.lissee[i-1]))
}
plot(seq.Date(from=as.Date("1988-01-01"),to=as.Date("2015-12-01"),by="month"),inflation.mois,type="s",lty="dashed",xlab=NA,ylab="Pourcentage")
lines(seq(from=as.Date("1988-01-01"),to=as.Date("2015-12-01"),by="month"),inflation.lissee,lwd=2)
grid(0,NULL,col="gray")
legend(as.numeric(as.Date("1990-01-01")),1,c("Inflation Réelle","Inflation lissée"),lwd=c(1,2),lty=c("dashed","solid"))


# PIB
pib = read.csv("GDPC1.csv",header=TRUE,sep=",")
pib$DATE = as.Date(pib$DATE,format="%Y-%m-%d")
pib$GDPC1 = as.numeric(pib$GDPC1)
pib.recent = pib[which(pib$DATE==debut):which(pib$DATE=="2015-10-01"),]

lm = lm(GDPC1 ~ DATE, data=pib.recent)
plot(pib.recent,type="l",lwd=2,ylab="Millions de $",xlab=NA)
abline(a=lm$coefficients[1],b=as.numeric(lm$coefficients[2]),lty="dashed",lwd=2)
legend(pib.recent$DATE[10],16000,c("PIB","Régression linéaire"),lty=c("solid","dashed"),lwd=c(2,2))

pib.mois = rep(pib.recent$GDPC1,each=3)
pib.lissee = rep(0,length(pib.mois))
pib.lissee[1] = pib.mois[1]
for(i in 2:length(pib.lissee)) {
	pib.lissee[i] = mean(c(pib.lissee[i-1],pib.mois[i]))
}

pib.cible = lm$coefficients[1]+lm$coefficients[2]*seq(from=as.numeric(as.Date(debut)),to=as.numeric(as.Date("2015-12-01")),length=length(pib.lissee))
var.pib = 100*(pib.lissee/pib.cible-1)
plot(var.pib,type="l",lwd=2,ylab="Écart PIB réel/cible (%)",xlab=NA,xaxt="n")
grid(0,NULL,col="gray")

rho = -1.4
t_directeur = inflation.lissee + theta_y*var.pib + theta_pi*(inflation.lissee-pi_cible) + rho
t_final = cbind(seq(from=as.numeric(as.Date(debut)),to=as.numeric(as.Date("2015-12-01")),length=length(pib.lissee)),t_directeur)
t_final = t_final[1:dim(fed.recent)[1],]
plot(fed.recent,type="l",lwd=2,ylim=c(-1,10),ylab="Pourcentage",xlab=NA)
lines(t_final,lwd=2,lty="dashed")
grid(0,NULL,col="gray")
legend(as.numeric(as.Date("1990-01-01")),1.5,c("Taux historique","Règle de Taylor"),lty=c("solid","dashed"),lwd=c(2,2))

# figure PIB et ecart
par(fig=c(0,1,0.2,1), new=TRUE)
plot(pib.recent,type="l",lwd=2,ylab="Millions de $",xlab=NA)
abline(a=lm$coefficients[1],b=as.numeric(lm$coefficients[2]),lty="dashed",lwd=2)
legend(pib.recent$DATE[10],16000,c("PIB","Régression linéaire"),lty=c("solid","dashed"),lwd=c(2,2))
par(fig=c(0,1,0,0.4), new=TRUE)
plot(seq.Date(from=as.Date("1988-01-01"),to=as.Date("2015-12-01"),by="month"),var.pib,type="l",lwd=2,ylab="Écart PIB réel/cible (%)",xlab=NA)
grid(0,NULL,col="gray")


# 1988 à 1992
plot(fed.recent[1:49,],type="l",lwd=2,ylim=c(3,10),ylab="Pourcentage",xlab=NA)
lines(t_final[1:49,],lwd=2,lty="dashed")
grid(0,NULL,col="gray")
legend(as.numeric(as.Date("1988-06-01")),5,c("Taux historique","Règle de Taylor"),lty=c("solid","dashed"),lwd=c(2,2))


# 1993 à 1999
plot(fed.recent[70:130,],type="l",lwd=2,ylim=c(3,7),ylab="Pourcentage",xlab=NA)
lines(t_final[70:130,],lwd=2,lty="dashed")
grid(0,NULL,col="gray")


# 2000 à 2008
erreur = rep(0,length(seq(from=-2,to=0,by=0.05)))
for (rho in seq(from=-2,to=0,by=0.05)) {
	t_directeur = inflation.lissee + theta_y*var.pib + theta_pi*(inflation.lissee-pi_cible) + rho
	t_final = cbind(seq(from=as.numeric(as.Date(debut)),to=as.numeric(as.Date("2015-12-01")),length=length(pib.lissee)),t_directeur)
	t_final = t_final[1:dim(fed.recent)[1],]
	erreur[rho/0.05+41] = sum((t_final[140:250,2]-fed.recent[140:250,2])^2)
}
erreur = sum((t_final[140:250,2]-fed.recent[140:250,2])^2)
plot(fed.recent[140:250,],type="l",lwd=2,ylim=c(0,8),ylab="Pourcentage",xlab=NA)
lines(t_final[140:250,],lwd=2,lty="dashed")
grid(0,NULL,col="gray")
legend(as.numeric(as.Date("2002-06-01")),7.5,c("Taux historique","Règle de Taylor"),lty=c("solid","dashed"),lwd=c(2,2))

plot(seq(from=-2,to=0,by=0.05),erreur,xlab="Taux réel long terme",ylab="Erreur")

theta_pi.calcule = pmax((fed.recent[140:250,2]-inflation.lissee[140:250]-theta_y*var.pib[140:250]-rho)/(inflation.lissee[140:250]-pi_cible),0)
plot(seq.Date(from=as.Date("1999-08-01"),to=as.Date("2008-10-01"),by="month"),theta_pi.calcule,xlab=NA,ylab=NA,type="l",lwd=2)
abline(h=0.5,lty="dashed",lwd=2)
abline(v=c(as.Date("2001-09-25"),as.Date("2006-05-01"),as.Date("2007-12-15")))
legend(as.Date("2003-01-01"),15,c("Theta pi","Theta y"),lwd=c(2,2),lty=c("solid","dashed"))


