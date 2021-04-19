getwd()

setwd("C:/Users/satis/OneDrive/Desktop/Edvancer/R prog/Real_estate_project_1")

h_train=read.csv("housing_train.csv",stringsAsFactors = F)
h_test=read.csv("housing_test.csv",stringsAsFactors = F)

h_train$Price
h_test$Price=NA

h_train$data='train'
h_test$data='test'


h_all=rbind(h_train,h_test)

library(dplyr)
glimpse(h_all)

library(tidyr)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

#finding the name which contain character 

names(h_all)[sapply(h_all, function(x) is.character(x))]

#taking all the character name to one vari
cat_col=c("Suburb", "Type","Method","SellerG","CouncilArea")

#now creating dummy 
for(cat in cat_col){
  h_all=CreateDummies(h_all,cat,100)
}

#now removing address bcoz it contain large number of distinct value
h_all=h_all %>%
  select(-Address)

#checking now we have character value
glimpse(h_all)

h_all=h_all[!((is.na(h_all$Price)) & h_all$data=='train'), ]


for(col in names(h_all)){
  if(sum(is.na(h_all[,col]))>0 & !(col %in% c("data","Price"))){
    h_all[is.na(h_all[,col]),col]=mean(h_all[h_all$data=='train',col],na.rm=T)
  }
}

names(h_all)[sapply(h_all, function(x) is.na(x))]

#till this point no na is present 

#now filtering 

h_train=h_all %>% 
  filter(data=='train') %>% 
  select(-data)

h_test=h_all%>% 
  filter(data=='test') %>% 
  select(-data,-Price)

#we have separated our data and removed unnecessary column 

#now we are building model on training data

library(car)

for_vif=lm(Price~.-CouncilArea_,data=h_train)

sort(vif(for_vif),decreasing = T)[1:3]

rm(for_vif)
fit=lm(Price~.-CouncilArea_,data=h_train)

fit=step(fit)

summary(fit)

formula(fit)

fit=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
         Landsize + BuildingArea + YearBuilt + Suburb_Doncaster + 
         Suburb_AscotVale + Suburb_Thornbury + Suburb_Hampton + Suburb_Balwyn + 
         Suburb_MalvernEast + Suburb_Camberwell + Suburb_PortMelbourne + 
          + Suburb_BrightonEast + Suburb_Hawthorn + 
         Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + Suburb_Essendon + 
         Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
         Suburb_Reservoir + Type_u + Type_h + Method_SP  + 
         Method_S + SellerG_Kay  + SellerG_Miles + SellerG_Greg + 
         SellerG_RT + SellerG_Biggin  + SellerG_Marshall + 
         SellerG_hockingstuart + SellerG_Jellis + CouncilArea_Whitehorse + 
         CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Banyule + 
         CouncilArea_PortPhillip + CouncilArea_Yarra + CouncilArea_Maribyrnong + 
         CouncilArea_Stonnington + CouncilArea_Darebin + CouncilArea_Moreland + 
         CouncilArea_Boroondara,data = h_train)

summary(fit)

rmse= mean((h_train$Price-predict(fit,newdata=h_train))**2) %>%
  sqrt()
rmse #382380.8





test.predictions=predict(fit,newdata=h_test)
write.csv(test.predictions,'firstName_LastName_P1_part2.csv',row.names = F)

library(pROC)

val.score=predict(fit,newdata = h_test,type = 'response')
auc_score=auc(roc(h_train$Price,val.score))
auc_score


