library('ISLR')
data(Hitters)
hitters <- na.omit(Hitters) # elimina i dati mancanti
dim(hitters)
names(hitters)
library(glmnet) # per usare i metodi ridge e lasso
y <- hitters$Salary
X <- model.matrix(Salary ~ ., data=hitters)[,-1] # tolta l'intercetta
m.ridge <- glmnet(X, y, alpha=0)
names(m.ridge) # per vedere che parametri contiene il nuovo modello
length(m.ridge$lambda) # quanti valori di lambda sono stati esaminati?
plot(m.ridge, xvar='lambda', xlab=expression(log(lambda))) # grafico dell'andamento dei coefficienti dei covarianti al variare di lambda
# si cerca il valore ottimo di lambda tramite cross validation
set.seed(2906)
cv.ridge <- cv.glmnet(X, y, alpha=0) # di default, cv usa 10-folds cv (k = 10)
plot(cv.ridge, xlab=expression(log(lambda))) # mostra MSE ottenuto dalla cv per ogni lambda
best.lambda <- cv.ridge$lambda.min # il lambda migliore e' quello che minimizza MSE
m.ridge.min <- glmnet(X, y, alpha=0, lambda=best.lambda) # ri-stima del modello ridge con il miglior lambda
max(m.ridge$dev.ratio) # devianza spiegata in corrispondenza del lambda minimo
# disegna il grafico della devianza spiegata al variare di lambda
plot(log(m.ridge$lambda), m.ridge$dev.ratio, type='l', xlab=expression(log(lambda)), ylab='Devianza spiegata')
# traccia una retta verticale in corrispondenza del miglior lambda
abline(v=log(best.lambda), lty=2)
m.lasso <- glmnet(X, y, alpha=1) # stima del modello tramite lasso
plot(m.lasso, xvar='lambda', xlab=expression(log(lambda))) # grafico dell'andamento dei coefficienti dei covarianti al variare di lambda
# si ripete la ricerca del lambda ottimo con la cross validation
set.seed(2906)
cv.lasso <- cv.glmnet(X, y, alpha=1)
best.lambda.lasso <- cv.lasso$lambda.min
m.lasso.min <- glmnet(X, y, alpha=1, lambda=best.lambda.lasso) # ri-stima del modello lasso con il miglior lambda
max(m.lasso$dev.ratio) # calcolo della miglior devianza spiegata

