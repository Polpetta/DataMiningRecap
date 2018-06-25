#modello lineare
m <- lm(medv ~ lstat, data=Boston)
#riassunto del modello
summary(m)
#componenti del modello
names(m)
#si seleziona una componente con il simbolo dollaro
m$coefficients
#calcolo dei residui classici
residui <- residuals(m)
#divisione della finestra dei plot in una tabella 2x2
par(mfrow=c(2,2))
#istogramma dei residui
hist(residui, prob=TRUE)
#grafico di dispersione
plot(residui)
#i punti del seguente grafico dovrebbero rispettare la premessa secondo la quale l'errore ha media zero
#traccio la linea dello zero
abline(h=0)
#grafico di dispersione dei redisui rispetto alla variabile esplicativa (lstat) inserita nelle ascisse
plot(Boston$lstat, residui, cex.lab=1.4, cex.axis=1.4)
abline(h=0)
#grafico di dispersione dei residui stimati del modello
plot(fitted(m), residui, cex.lab=1.4, cex.axis=1.4)
abline(h=0)
#se nei grafici precedenti c'e' qualche osservazione anomala o si sospetta che ci siano dei problemi allora si vanno a guardare i residui standardizzati
r.standard <- rstandard(m)
#e si plottano con:
plot(r.standard)
abline(h=0)
#grafico di dispersione dei redisui standardizzati rispetto alla variabile esplicativa (lstat) inserita nelle ascisse
plot(Boston$lstat, r.standard, cex.lab=1.4, cex.axis=1.4)
abline(h=0)
#i residui standardizzati dovrebbero trovarsi nell'intervallo [-2,2]
abline(h=-2, col='red')
abline(h=2, col='red')
#dando i seguenti due comandi si ottengono 4 grafici:
par(mfrow=c(2,2))
plot(m)
#intervallo di confidenza per beta1 al livello 0.95
#il primo parametro indica i quantili, il secondo i gradi di liberta'
qt(0.025, df=506-2)
#verifichiamo che lo 0 rientri in [stima - tDiStudent * SE, stima + tDiStudent * SE]
-0.95005 -qt(0.975, df=506-2)* 0.03873
-0.95005 +qt(0.975, df=506-2)* 0.03873
#confint riassume tutto questo, e se non altrimenti specificato imposta il livello a 0.95
confint(m)
confint(m, level=0.90)

