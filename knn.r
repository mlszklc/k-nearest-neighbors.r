#KNN

diyabet <- read.table(file.choose(), header = T, sep = ";")
str(diyabet)

# hedef nitelik faktore cevrilir
diyabet$Diabetes <- as.factor(diyabet$Diabetes)
class(diyabet$Diabetes) #sınıfına bakıyoruz ve artık faktor olduğunu goruyoruz.
str(diyabet) #ozetine bakalım

# duzeylerin kac kez tekrar ettigini frekans sayilarini bulalim
table(diyabet$Diabetes) #0 55 defa 1 ise 349 defa tekrar etmis

# burada 1, 2 nin karsilik gelen anlamlarini atayalim
#bunun icin plyr() paketini yuklemeliyiz
install.packages("plyr")
library(plyr)
diyabet$Diabetes <- revalue(diyabet$Diabetes, c("0"="no","1"="yes"))
table(diyabet$Diabetes)

# veri setinin ozetine bakalim
summary(diyabet)

#veri setini egitim ve test veri seti olarak ayiralim.
#%70 egitim %30 test olacak sekilde ayırıyorum. Bunun için #caret paketini kuralım.

library(caret)
set.seed(1)
egitimIndisleri <- createDataPartition(y=diyabet$Diabetes, p=.07, list=FALSE)
head(egitimIndisleri)

egitim <- diyabet[egitimIndisleri,]
test <- diyabet[-egitimIndisleri,]

testnitelikleri <- test[,-17]
testhedefnitelik <- test[[17]]

egitimnitelikleri <- egitim[,-17]
egitimhedefnitelik <- egitim[[17]]

# k degeri 1 den 10 a kadar deneyelim
k_degeri <- 10
dogruluk <- NULL

#KNN için class paketini cagiriyorum
install.packages("class")
library(class)

for(i in 1:k_degeri)
{
  set.seed(1)  
  (tahminisiniflar=knn(egitimnitelikleri,testnitelikleri,egitimhedefnitelik, k=i))
  dogruluk[i] <- mean(tahminisiniflar==testhedefnitelik)
  dogruluk[i] <- round(dogruluk[i],2)
}
for(i in 1:k_degeri)
  print(paste("k=",i, "icin elde edilen dogruluk=", dogruluk[i]))


tablom <- table(tahminisiniflar,testhedefnitelik,dnn = c("tahmini siniflar","gercek siniflar"))
tablom



