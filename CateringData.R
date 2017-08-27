# Shreya Ganeshan 
# Catering Data

# ----------------- Importing Libraries ------------------
library(MASS)
library(car) # for Durbin Watson Test and Variance Inflation Factor (VIF)
library(leaps) # for regsubsets
library(plyr) # for charts

# ----------------- Reading Raw Data # ----------------- 
rm(list=ls()) # clear all previous variables
rawdata = read.csv("Data1.csv", header = T, sep =",")
rawdata # print all observations in dataset 
head(rawdata) # print first six observations in dataset 
attach(rawdata) # attaching dataset so variables can be called by name
detach(rawdata)
length(REQUESTS.Id)
summary(rawdata) # summary statistics for all numerical and categorical variables 
# total number of observations (quotes): 8502 
factor(REQUESTS.Id)
# total number of distinct requests: 3935 
# some requests will have multiple quotes 

summary(Event_Type) 
factor(Event_Type) # 11 factor levels total
# 10 "event types" (8224 obsv) + 1 blanks (278)
summary(REQUESTS.Category)
factor(REQUESTS.Category)
summary(REQUESTS.LOCATION.Continent.Name)
summary(REQUESTS.LOCATION.Region)
# all REQUESTS.Category are for "catering" coming from 10 regions on "Westeros" continent
table(REQUESTS.LOCATION.Region)
# most orders coming from "Dorne" region (1904), then "Beyond the Wall" region (1119), then "The Westerlands" (1052)

summary(rawdata$event_baby_shower) # 19 remainder
summary(Event_Type)
# there is a discrepancy between those who reported "baby shower" under event type and those who ultimately reported that it was a baby shower on the next day 
# some folks may have left the first "event type" question blank and then specified baby shower later ???????
# therefore, take the # of "true variables" under "event_baby_shower" summary = 456 ????????
summary(rawdata$event_anniversary) #35 remainder
summary(rawdata$event_birthday) #121 remainder
summary(rawdata$event_cocktail_party) #136
summary(rawdata$event_corporate)

# ----------------- Organizing Request IDs ----------------- 
IDs = table(rawdata$REQUESTS.Id)
class(IDs)
IDslist = as.data.frame(IDs) # will display which request IDs have multiple quotes
head(IDslist)
summary(IDslist) # at minimum, every ID maps to 1 quote, at most an ID maps to 5 quotes, and on average, it maps to 2.161 quotes ~ 2

# ----------------- Organizing Event Type ----------------- 
# the Event_Type variable is unreliable because often times people leave it blank and then specify event type later
eventtype = table(Event_Type)
events = as.data.frame(eventtype)
head(events)
events

# thus, store all "true" event variables in data frame 
# changing variable responses to binary categorical variables 
summary(event_anniversary) #same info as "table" but "summary" dislays NA's

summary(event_anniversary)
anniversary = as.vector((event_anniversary[event_anniversary==TRUE]))
numanniversary = length(anniversary)
numanniversary

summary(event_baby_shower)
babyshower = as.vector((event_baby_shower[event_baby_shower==TRUE]))
numbabyshower = length(babyshower)
numbabyshower

summary(event_birthday)
birthday = as.vector(event_birthday[event_birthday==TRUE])
numbirthday = length(birthday)
numbirthday

summary(event_cocktail_party)
cocktail = as.vector(event_cocktail_party[event_cocktail_party == TRUE])
numcocktail = length(cocktail)
numcocktail

summary(event_corporate)
corporate = as.vector(event_cocktail_party[event_corporate == TRUE])
numcorporate = length(corporate)
numcorporate

summary(event_graduation)
graduation = as.vector(event_graduation[event_graduation == TRUE])
numgraduation = length(graduation)
numgraduation

summary(event_other)
other = as.vector(event_other[event_other == TRUE])
numother = length(other)
numother

summary(event_reunion)
reunion = as.vector(event_reunion[event_reunion == TRUE])
numreunion = length(reunion)
numreunion

summary(event_wedding)
wedding = as.vector(event_wedding[event_wedding == TRUE])
numwedding = length(wedding)
numwedding

summary(event_wedding_rehersal)
weddingrehe = as.vector(event_wedding_rehersal[event_wedding_rehersal == TRUE])
numweddingrehe = length(weddingrehe)
numweddingrehe


# events = data.frame(anniversary, babyshower, birthday, cocktail, corporate, graduation, other,
                    reunion, wedding, weddingrehe)

numevents = as.vector(c(numanniversary, numbabyshower, numbirthday, numcocktail, numcorporate, numgraduation,
                       numother, numreunion, numwedding, numweddingrehe))

labels = c("Anniversary", "Baby Shower", "Birthday", "Cocktail Party", "Corporate", "Graduation", "Other", "Reunion", "Wedding", "Wedding Rehersal")
pct = round(numevents/sum(numevents)*100)
labels = paste(labels, pct) # add percents to labels 
labels = paste(labels,"%",sep="") # ad % to labels 
# col = c("blue", "green", "pink", "purple", "light purple")
pie(numevents, labels = labels, main= "Breakdown of Catering by Event Type")

barplot(numevents)

# ----------------- Formatting Dates ----------------- 
# When the Order was Placed
dateorder1 = as.Date(REQUESTS.Created.Date, format = "%m/%d/%Y")
dateorder1
dayorder1 = format(dateorder1, format="%a")
dayorder2 = data.frame(table(dayorder1))
dayorder2 # most people place order on Tuesdays

monthorder1 = format(dateorder1, format="%b")
monthorder2 = data.frame(table(monthorder1))
monthorder2 # most people place order on October

# When the Event Takes Place
dateevent1 = as.Date(REQUESTS.Needed.Date, format = "%m/%d/%Y")
dateevent1
dayevent1 = format(dateevent1, format="%a")
dayevent2 = data.frame(table(dayevent1))
dayevent2 # most events take place on Sundays

monthevent1 = format(dateevent1, format="%b")
monthevent2 = data.frame((table(monthevent1)))
monthevent2 # most people events take place in September

rawdata2 = cbind(rawdata, monthevent1, monthorder1, dayorder1, dayevent1)
# new dataset with months and days added as columns
head(rawdata2)

summary(REQUESTS.Days.until.Event) 
# on average, people completed a request 81.8 days in advance of the event

# ----------------- Organizing Cuisine Type ----------------- 
summary(cuisine_american_casual)
amercas = as.vector((cuisine_american_casual[cuisine_american_casual==TRUE]))
numamercas = length(amercas)
numamercas

summary(cuisine_american_formal)
amerform = as.vector((cuisine_american_formal[cuisine_american_formal==TRUE]))
numamerform = length(amerform)
numamerform

summary(cuisine_asian)
asian = as.vector(cuisine_asian[cuisine_asian==TRUE])
numasian = length(asian)
numasian

summary(cuisine_bbq)
bbq = as.vector(cuisine_bbq[cuisine_bbq == TRUE])
numbbq = length(bbq)
numbbq

summary(cuisine_healthy)
healthy = as.vector(cuisine_healthy[cuisine_healthy == TRUE])
numhealthy = length(healthy)
numhealthy

summary(cuisine_italian)
italian = as.vector(cuisine_italian[cuisine_italian == TRUE])
numitalian = length(italian)
numitalian

summary(cuisine_mediterranean)
med = as.vector(cuisine_mediterranean[cuisine_mediterranean == TRUE])
nummed = length(med)
nummed

summary(cuisine_mexican_latin)
mex = as.vector(cuisine_mexican_latin[cuisine_mexican_latin == TRUE])
nummex = length(mex)
nummex

summary(cuisine_other)
othercui = as.vector(cuisine_other[cuisine_other == TRUE])
numothercui = length(othercui)
numothercui

summary(cuisine_veg_vegan)
veg = as.vector(cuisine_veg_vegan[cuisine_veg_vegan == TRUE])
numveg = length(veg)
numveg

numcui = as.vector(c(numveg, numothercui, nummex, nummed, numitalian, numhealthy,
                        numbbq, numasian, numamerform, numamercas))

labels = c("Other", "Vegetarian/Vegan", "Mexican/Latin", "Mediterranean", 
           "Italian", "Healthy", "BBQ", "Asian", "American (Formal)", "American (Casual)")
pct = round(numcui/sum(numcui)*100)
labels = paste(labels, pct) # add percents to labels 
labels = paste(labels,"%",sep="") # ad % to labels 
pie(numcui, labels = labels, main= "Breakdown of Catering by Cuisine Type")

# ----------------- Organizing Food Type ----------------- 
summary(food_alcoholic_beverages)
alcoholic = as.vector((food_alcoholic_beverages[food_alcoholic_beverages==TRUE]))
numalcoholic = length(alcoholic)
numalcoholic

summary(food_breakfast)
breakfast = as.vector((food_breakfast[food_breakfast==TRUE]))
numbreakfast = length(breakfast)
numbreakfast

summary(food_brunch)
brunch = as.vector(food_brunch[food_brunch==TRUE])
numbrunch = length(brunch)
numbrunch

summary(food_dessert)
dessert = as.vector(food_dessert[food_dessert == TRUE])
numdessert = length(dessert)
numdessert

summary(food_dinner)
dinner = as.vector(food_dinner[food_dinner == TRUE])
numdinner = length(dinner)
numdinner

summary(food_hors_doeurves)
horsdoe = as.vector(food_hors_doeurves[food_hors_doeurves == TRUE])
numhorsdoe = length(horsdoe)
numhorsdoe

summary(food_lunch)
lunch = as.vector(food_lunch[food_lunch == TRUE])
numlunch = length(lunch)
numlunch

summary(food_non_alcoholic_beverages)
noalcohol = as.vector(food_non_alcoholic_beverages[food_non_alcoholic_beverages == TRUE])
numnoalcohol = length(noalcohol)
numnoalcohol

summary(food_other)
other = as.vector(food_other[food_other == TRUE])
numother = length(other)
numother

numfood = as.vector(c(numother, numnoalcohol, numlunch, numhorsdoe, numdinner, numdessert,
                      numbrunch, numbreakfast, numalcoholic))

labels = c("Other", "Non-Alcoholic Drinks", "Lunch", "Hors D'oeurves", 
           "Dinner", "Dessert", "Brunch", "Breakfast", "Alcoholic Drinks")
pct = round(numfood/sum(numfood)*100)
labels = paste(labels, pct) # add percents to labels 
labels = paste(labels,"%",sep="") # ad % to labels 
pie(numfood, labels = labels, main= "Breakdown of Catering by Food Type")

# ----------------- Organizing by Catering Type ------------------
summary(catering_type_boxed_meal)
boxedmeal = as.vector((catering_type_boxed_meal[catering_type_boxed_meal==TRUE]))
numboxedmeal = length(boxedmeal)
numboxedmeal

summary(catering_type_buffet_self_service)
buffetss = as.vector((catering_type_buffet_self_service[catering_type_buffet_self_service==TRUE]))
numbuffetss = length(buffetss)
numbuffetss

summary(catering_type_buffet_with_servers)
buffetserv = as.vector(catering_type_buffet_with_servers[catering_type_buffet_with_servers==TRUE])
numbuffetserv = length(buffetserv)
numbuffetserv

summary(catering_type_cocktail_party)
cocktailcater = as.vector(catering_type_cocktail_party[catering_type_cocktail_party == TRUE])
numcocktailcater = length(cocktailcater)
numcocktailcater

summary(catering_type_not_sure_yet)
notsure = as.vector(catering_type_not_sure_yet[catering_type_not_sure_yet == TRUE])
numnotsure = length(notsure)
numnotsure

summary(catering_type_Other)
othercater = as.vector(catering_type_Other[catering_type_Other == TRUE])
numothercater = length(othercater)
numothercater

summary(catering_type_plated_meal)
platedmeal = as.vector(catering_type_plated_meal[catering_type_plated_meal == TRUE])
numplatedmeal = length(platedmeal)
numplatedmeal

numcater = as.vector(c(numplatedmeal, numothercater, numnotsure, numcocktailcater,
                      numbuffetserv, numbuffetss, numboxedmeal))

labels = c("Plated Meal", "Other", "Not Sure", "Cocktail Party", 
           "Served Buffet", "Self Service Buffet", "Boxed Meal")
pct = round(numcater/sum(numcater)*100)
labels = paste(labels, pct) # add percents to labels 
labels = paste(labels,"%",sep="") # ad % to labels
pie(numcater, labels = labels, main= "Breakdown of Catering by Presentation Type")

# -----------------  Analyzing Price ----------------- 
# definitely not uniformly distributed or normally distributed 
# Average Price Per Unit: Group
pricedataG = subset(rawdata, BIDS.Group.Person == "Group", 
                  select=c(number_of_guests, BIDS.Group.Person,
                           BIDS.Estimate.Price.Per.Unit))
head(pricedataG)
mean(pricedataG$BIDS.Estimate.Price.Per.Unit) #1915.041
median(pricedataG$BIDS.Estimate.Price.Per.Unit) #1300
summary(pricedataG$number_of_guests) 
# most "group" quotes for expected attendance 20-30ppl(1003), then 40-60ppl (876)

# Average Price Per Unit: Person
pricedataP = subset(rawdata, BIDS.Group.Person == "Per person", 
                   select=c(number_of_guests, BIDS.Group.Person,
                            BIDS.Estimate.Price.Per.Unit))
head(pricedataP)
mean(pricedataP$BIDS.Estimate.Price.Per.Unit) #29.41237
median(pricedataP$BIDS.Estimate.Price.Per.Unit) #25
summary(pricedataP$number_of_guests) 
# most "per person" quotes for expected attendance 20-30ppl(656), then 40-60ppl (598)

# ----------------- Price in Relation to Other Categories ----------------- 
plot(BIDS.Estimate.Price.Per.Unit ~ Event_Type, cex.axis = 0.7, 
     xlab = "Event Type", ylab = "Overall Price per Unit ($)", 
     main = "Overall Price Distributions by Event Type")
plot(BIDS.Estimate.Price.Per.Unit ~ Event_Type, data = pricedataPfull,
     cex.axis = 0.7, xlab = "Event Type", ylab = "Price per Person ($)", 
     main = "Distributions of Price per Person by Event Type")
plot(BIDS.Estimate.Price.Per.Unit ~ Event_Type, data = pricedataGfull,
     cex.axis = 0.7, xlab = "Event Type", ylab = "Price per Group ($)", 
     main = "Distributions of Price per Group by Event Type")

# ----------------- Converting Categorical Variables to Numeric ----------------- 
cols = sapply(rawdata, is.logical)
rawdata[,cols] = lapply(rawdata[,cols], as.numeric)
head(rawdata)

# Price per Person
pricedataPfull = subset(rawdata2,BIDS.Group.Person == "Per person", 
                        select=REQUESTS.Id:number_of_guests) # use later in reg
head(pricedataPfull)

pricedataPnum = subset(rawdata2,BIDS.Group.Person == "Per person", 
                        select=c(event_wedding:catering_type_buffet_with_servers,BIDS.Estimate.Price.Per.Unit, 
                                 monthevent1, monthorder1, dayorder1, dayevent1))# use later in reg
head(pricedataPnum)

# Price per Group
pricedataGfull = subset(rawdata2,BIDS.Group.Person == "Group", 
                        select=REQUESTS.Id:number_of_guests) # use later in reg
head(pricedataGfull)

pricedataGnum = subset(rawdata2,BIDS.Group.Person == "Group", 
                       select=c(event_wedding:catering_type_buffet_with_servers,BIDS.Estimate.Price.Per.Unit,
                                monthevent1, monthorder1, dayorder1, dayevent1))# use later in reg
head(pricedataGnum)

# for (var in 1:8502){
  #(var[var == T]) {
    #var[var = "1"]
  #}
#}

# ----------------- Price Distributions ----------------- 
# Price per Person
density = density(pricedataPnum$BIDS.Estimate.Price.Per.Unit,  bw=1, na.rm=T) # uniform density
density = density(pricedataPnum$BIDS.Estimate.Price.Per.Unit,  bw=4, na.rm=T) # uniform density
density = density(pricedataPnum$BIDS.Estimate.Price.Per.Unit,  bw=7, na.rm=T) # uniform density
density = density(pricedataPnum$BIDS.Estimate.Price.Per.Unit,  bw=100, na.rm=T) # uniform density
plot(density, main = 'Density Distribution Price per Person')
#logdensity = dlnorm(pricedataPnum$BIDS.Estimate.Price.Per.Unit)
#hist(logdensity)
summary(pricedataPfull$BIDS.Estimate.Price.Per.Unit)

# Price per Group
density = density(pricedataGnum$BIDS.Estimate.Price.Per.Unit,  bw=1, na.rm=T) # uniform density
plot(density, main = 'Density Distribution Price per Group')
summary(pricedataGfull$BIDS.Estimate.Price.Per.Unit)

# Average Price per Unit: Both 
summary(rawdata$BIDS.Estimate.Price.Per.Unit)

# Plotting Unit Price : General
barplot(BIDS.Estimate.Price.Per.Unit, ylab = "Price per Unit ($)", 
        main = "Fixed Price per Unit (Overall)")
# Plotting Unit Price : Group
par(mfrow=c(2,2))
barplot(pricedataG$BIDS.Estimate.Price.Per.Unit, ylab = "Price per Group ($)", 
        main = "Fixed Price per Group")
hist(pricedataG$BIDS.Estimate.Price.Per.Unit, xlab = "Price per Group ($)",
     ylab = "Number of Orders", main = "Frequency of Prices per Group")
# Plotting Unit Price : Per Person
barplot(pricedataP$BIDS.Estimate.Price.Per.Unit, ylab = "Price per Person ($)",
        main = "Fixed Price per Person") # what people are paying
hist(pricedataP$BIDS.Estimate.Price.Per.Unit, xlab = "Price per Person ($)",
     ylab = "Number of Orders", main = "Frequency of Prices per Person")

# ----------------- Price Regressions ----------------- 
# Per Person
modelP = glm(BIDS.Estimate.Price.Per.Unit ~  
          (event_anniversary) + (event_baby_shower) + 
              (event_birthday) + (event_corporate) + (event_graduation) + 
              (event_reunion) + (event_wedding) + (event_wedding_rehersal) + 
            (food_alcoholic_beverages) + (food_breakfast) + (food_brunch) + 
              (food_dessert) + (food_dinner) + (food_hors_doeurves) + (food_lunch) + 
              (food_non_alcoholic_beverages) +(food_other) +
              (catering_type_boxed_meal) + (catering_type_buffet_self_service) + 
              (catering_type_buffet_with_servers) + (catering_type_cocktail_party) + 
              (catering_type_Other) + (catering_type_plated_meal) + (catering_type_not_sure_yet) +
            (cuisine_american_casual) + (cuisine_american_formal) + (cuisine_asian) + (cuisine_bbq) +
            (cuisine_healthy) + (cuisine_italian) + (cuisine_mediterranean) + (cuisine_mexican_latin) + 
            (cuisine_other) + (cuisine_veg_vegan) + (monthevent1) + (monthorder1),
               data = pricedataPnum)
summary(modelP)
plot(modelP)

allP = regsubsets( BIDS.Estimate.Price.Per.Unit ~ 
                    ((event_anniversary) + (event_baby_shower) + 
                    (event_birthday) + (event_corporate) + (event_graduation) + 
                    (event_reunion) + (event_wedding) + (event_wedding_rehersal) + 
                    (food_alcoholic_beverages) + (food_breakfast) + (food_brunch) + 
                    (food_dessert) + (food_dinner) + (food_hors_doeurves) + (food_lunch) + 
                    (food_non_alcoholic_beverages) +(food_other) +
                    (catering_type_boxed_meal) + (catering_type_buffet_self_service) + 
                    (catering_type_buffet_with_servers) + (catering_type_cocktail_party) + 
                    (catering_type_Other) + (catering_type_plated_meal) + (catering_type_not_sure_yet) +
                    (cuisine_american_casual) + (cuisine_american_formal) + (cuisine_asian) + (cuisine_bbq) +
                    (cuisine_healthy) + (cuisine_italian) + (cuisine_mediterranean) + (cuisine_mexican_latin) + 
                    (cuisine_other) + (cuisine_veg_vegan) + (monthevent1) + (monthorder1)),
                  data = pricedataPnum, nvmax = 10, really.big = T)
summary(allP)
summary(allP)$bic
# plot(1:34, summary(all)$bic,type="b",pch=19,col="sienna")
# subsets(all, statistic = "bic", main = 'Best Model to Predict T8') 

stepP = stepAIC(modelP, direction = 'both', data = pricedataPnum)
stepP$anova

# Per Group
modelG = glm(BIDS.Estimate.Price.Per.Unit ~  
               (event_anniversary) + (event_baby_shower) + 
               (event_birthday) + (event_corporate) + (event_graduation) + 
               (event_reunion) + (event_wedding) + (event_wedding_rehersal) + 
               (food_alcoholic_beverages) + (food_breakfast) + (food_brunch) + 
               (food_dessert) + (food_dinner) + (food_hors_doeurves) + (food_lunch) + 
               (food_non_alcoholic_beverages) +(food_other) +
               (catering_type_boxed_meal) + (catering_type_buffet_self_service) + 
               (catering_type_buffet_with_servers) + (catering_type_cocktail_party) + 
               (catering_type_Other) + (catering_type_plated_meal) + (catering_type_not_sure_yet) +
               (cuisine_american_casual) + (cuisine_american_formal) + (cuisine_asian) + (cuisine_bbq) +
               (cuisine_healthy) + (cuisine_italian) + (cuisine_mediterranean) + (cuisine_mexican_latin) + 
               (cuisine_other) + (cuisine_veg_vegan) + (monthevent1) + (monthorder1),
             data = pricedataGnum)
summary(modelG)
plot(modelG)

allG = regsubsets( BIDS.Estimate.Price.Per.Unit ~ 
                    ((event_anniversary) + (event_baby_shower) + 
                       (event_birthday) + (event_corporate) + (event_graduation) + 
                       (event_reunion) + (event_wedding) + (event_wedding_rehersal) + 
                       (food_alcoholic_beverages) + (food_breakfast) + (food_brunch) + 
                       (food_dessert) + (food_dinner) + (food_hors_doeurves) + (food_lunch) + 
                       (food_non_alcoholic_beverages) +(food_other) +
                       (catering_type_boxed_meal) + (catering_type_buffet_self_service) + 
                       (catering_type_buffet_with_servers) + (catering_type_cocktail_party) + 
                       (catering_type_Other) + (catering_type_plated_meal) + (catering_type_not_sure_yet) +
                       (cuisine_american_casual) + (cuisine_american_formal) + (cuisine_asian) + (cuisine_bbq) +
                       (cuisine_healthy) + (cuisine_italian) + (cuisine_mediterranean) + (cuisine_mexican_latin) + 
                       (cuisine_other) + (cuisine_veg_vegan) + (monthevent1) + (monthorder1)),
                  data = pricedataGnum, nvmax = 10, really.big = T)
summary(allG)
summary(allG)$bic
# plot(1:34, summary(all)$bic,type="b",pch=19,col="sienna")
# subsets(all, statistic = "bic", main = 'Best Model to Predict T8') 

stepG = stepAIC(modelG, direction = 'both', data = pricedataGnum)
stepG$anova

# Overall
modelO = glm(BIDS.Estimate.Price.Per.Unit ~  
               (event_anniversary) + (event_baby_shower) + 
               (event_birthday) + (event_corporate) + (event_graduation) + 
               (event_reunion) + (event_wedding) + (event_wedding_rehersal) + 
               (food_alcoholic_beverages) + (food_breakfast) + (food_brunch) + 
               (food_dessert) + (food_dinner) + (food_hors_doeurves) + (food_lunch) + 
               (food_non_alcoholic_beverages) +(food_other) +
               (catering_type_boxed_meal) + (catering_type_buffet_self_service) + 
               (catering_type_buffet_with_servers) + (catering_type_cocktail_party) + 
               (catering_type_Other) + (catering_type_plated_meal) + (catering_type_not_sure_yet) +
               (cuisine_american_casual) + (cuisine_american_formal) + (cuisine_asian) + (cuisine_bbq) +
               (cuisine_healthy) + (cuisine_italian) + (cuisine_mediterranean) + (cuisine_mexican_latin) + 
               (cuisine_other) + (cuisine_veg_vegan) + (monthevent1) + (monthorder1),
             data = rawdata2)
summary(modelO)
plot(modelO)

stepO = stepAIC(modelO, direction = 'both', data = rawdata2)
stepO$anova


