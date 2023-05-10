
#  View(spaceship_titanic_test)

#install.packages('stringr')
library(stringr)


#####################  Separating Cabin into deck / cabin no. / side  ########


# deck_pat <- '\w\b'
# cabin_num_pat <- '\d'
# side_pat <- '\b\w'

# deck <- str_view(Cabin,deck_pat)
# cabin_num <- str_view(Cabin,cabin_num_pat)
# side <- str_view(side_pat)

# train <- mutate(
#   train,
#   deck = NULL,
#   cabin_vect = NULL
# )

# trying to split Cabin column into deck / cabin / side columns
# train <- mutate(
#   train,
#   #cabin_vect = strsplit(Cabin, split = "/")
#   # deck = strsplit(Cabin, "/")
#   #deck = cabin_vect[[1]]
#   #cabin_vect = strsplit(Cabin, split = "\\w")
#   str_split_fixed(Cabin,)
# )

train <- mutate(
  train,
  deck = 'deck',
  cabin_no = 'cabin_no',
  side = 'side'
)

train[c('deck', 'cabin_no', 'side')] <- str_split_fixed(train$Cabin,'/', 3)
View(train)

select(train,deck, cabin_no, side)

train <- mutate(
  train,
  cabin_no = as.numeric(cabin_no)
)

select(train,deck, cabin_no, side)
View(train)
colnames(train)

################################################################################
##### Trying to re-arrange columns, none of these worked.

# View(arrange(train,"PassengerId",  "HomePlanet",   "CryoSleep",    "Cabin", 'deck', 'cabin_no', 'side', %everything()))
# 
# View(arrange(train,PassengerId, HomePlanet, CryoSleep, Cabin, deck, cabin_no, side, %everything()))
# 
# View(arrange(train,c("PassengerId",  "HomePlanet",   "CryoSleep",    "Cabin", 'deck', 'cabin_no', 'side', %everything())))
# View(arrange((train, c(PassengerId, HomePlanet, CryoSleep, Cabin, deck, cabin_no, side, %everything()))))


################################################################################
####################  Plotting to get some insights ############################

# ggplot(data = train, mapping = aes(x = deck, y = cabin_no )) + 
#   geom_point(mapping = aes(color = Transported))
# ^^^ This came close to crashing my laptop ^^^

ggplot(data = train, mapping = aes(x = side, y = deck)) + 
  geom_point(mapping = aes(color = Transported))
##  ^^ Not as simple as one side or the other (of course not)

ggplot(data = train, mapping = aes(x = CryoSleep, y = deck)) +
  geom_point(mapping = aes(color = Transported))
## ^^ Not a simple connection to CrySleep

ggplot(data = train, mapping = aes(x = cabin_no, y = deck)) + 
  geom_contour(aes(z = side)) +
  geom_point(mapping = aes(color = Transported))
# F deck has most number of cabins
# B deck seems to have highest % of Transported, need to check
  
perc_tr_by_deck <- train %>% group_by(deck) %>% 
  filter(!is.na(deck), !is.na(Transported)) %>%
  summarise( 
    n = n(),
    percent = sum(Transported) / n(),
    )

View(perc_tr_by_deck)
# ^^^^ B & C deck had much higher % of Transported than other decks.
# ^^^^ Other decks still had significant losses

ggplot(data = perc_tr_by_deck, mapping = aes(x = deck, y = percent)) +
  geom_point() +
  geom_smooth()

perc_tr_by_Cabin <- train %>% group_by(Cabin) %>% 
  filter(!is.na(Cabin), !is.na(Transported)) %>%
  summarise( 
    n = n(),
    percent = sum(Transported) / n(),
  )

View(perc_tr_by_Cabin)

avg_cab_loss <- perc_tr_by_Cabin %>%
  group_by(n) %>%
  summarize(
    mean_loss = mean(percent)
  )

view(avg_cab_loss)
# ^^^ no correlation as to if others in cabin were Transported.
#     i.e. those in cabin weren't all Transported together

perc_by_age <- train %>%
  filter(!is.na(Age), !is.na(Transported)) %>%
  filter(Age > 0) %>%
  group_by(Age) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss_by_age = 100 * sum(Transported) / n()
  )
  
  View(perc_by_age)
  
ggplot(data = perc_by_age, mapping = aes(x = Age, y = percent_loss_by_age)) +
    geom_point() +
    geom_smooth()
##  ^^^ This might show some correlation to age.  But there are too many age 0 
#       to be believed, this could be due to data corruption.
#       Adding filter for age > 0 shows there is a valid trend by age. 
#       0 - 10 year old's have the highest rate of being Transported.
#       Other ages still have ~50% chance of Transport, 
#       until 70+ where the count makes conclusions invalid

perc_by_home <- train %>%
  filter(!is.na(HomePlanet), !is.na(Transported)) %>%
  group_by(HomePlanet) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss_by_HomePlanet = 100 * sum(Transported) / n()
  )

View(perc_by_home)

ggplot(data = perc_by_home, mapping = aes(x = HomePlanet, y = percent_loss_by_age)) +
  geom_point() +
  geom_smooth()
## ^^^^ HomePlanet seems to have an influence. Earthlings are less 
##      likely to have been Transported.  Europans are the most likely.
##      Martians are about 50 - 50, so little to no influence on them.

colnames(train)

perc_by_Destination <- train %>%
  filter(!is.na(Destination), !is.na(Transported)) %>%
  group_by(Destination) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss_by_Destination = 100 * sum(Transported) / n()
  )

View(perc_by_Destination)

ggplot(data = perc_by_Destination, mapping = aes(x = Destination, y = percent_loss_by_age)) +
  geom_point() +
  geom_smooth()
### ^^ No correlation with Destination. Likely too few destinations to matter.


perc_by_VIP <- train %>%
  filter(!is.na(VIP), !is.na(Transported)) %>%
  group_by(VIP) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss_by_VIP = 100 * sum(Transported) / n()
  )

View(perc_by_VIP)

ggplot(data = perc_by_VIP, mapping = aes(x = VIP, y = percent_loss_by_VIP)) +
  geom_point() +
  geom_smooth()
###  ^^^ VIP's seem much less likely to be Transported, but this could just 
#       be a factor of sample size.  
#       Or it could be the old truth...poor folk get the short end of the stick

perc_by_VRDeck <- train %>%
  filter(!is.na(VRDeck), !is.na(Transported)) %>%
  group_by(VRDeck) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss_by_VRDeck = 100 * sum(Transported) / n()
  )

View(perc_by_VRDeck)

ggplot(data = perc_by_VRDeck, mapping = aes(x = VRDeck, y = percent_loss_by_VRDeck)) +
  geom_point()
### ^^^  No correlation to VRDeck

colnames(train)

perc_by_RoomService <- train %>%
  filter(!is.na(RoomService), !is.na(Transported)) %>%
  group_by(RoomService) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss_by_RoomService = 100 * sum(Transported) / n()
  )

View(perc_by_RoomService)

ggplot(data = perc_by_RoomService, mapping = aes(x = RoomService, y = percent_loss_by_RoomService)) +
  geom_col()
##^^^ No obvious correlation to RoomService.  
#     Need to bin by amount spent (100) to see better

perc_by_FoodCourt <- train %>%
  filter(!is.na(FoodCourt), !is.na(Transported)) %>%
  group_by(FoodCourt) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss_by_FoodCourt = 100 * sum(Transported) / n()
  )

View(perc_by_FoodCourt)

ggplot(data = perc_by_FoodCourt, mapping = aes(x = FoodCourt, y = percent_loss_by_FoodCourt)) +
  geom_col()
##^^^ No obvious correlation to FoodCourt.  Need to bin by amount spent (100) to see better

perc_by_Spa <- train %>%
  filter(!is.na(Spa), !is.na(Transported)) %>%
  group_by(Spa) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss_by_Spa = 100 * sum(Transported) / n()
  )

View(perc_by_Spa)

ggplot(data = perc_by_Spa, mapping = aes(x = Spa, y = percent_loss_by_Spa)) +
  geom_point()
##^^^ No obvious correlation to Spa. Fewer number transported at higher spending
##    is likely a function of sample size. Binning by 100's spent would show better


perc_by_ShoppingMall <- train %>%
  filter(!is.na(ShoppingMall), !is.na(Transported)) %>%
  group_by(ShoppingMall) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss_by_ShoppingMall = 100 * sum(Transported) / n()
  )

View(perc_by_ShoppingMall)

ggplot(data = perc_by_ShoppingMall, mapping = aes(x = ShoppingMall, y = percent_loss_by_ShoppingMall)) +
  geom_point()
##^^^ No obvious correlation to ShoppingMall Fewer number transported at higher spending
##    is likely a function of sample size. Binning by 100's spent would show better


perc_by_CryoSleep <- train %>%
  filter(!is.na(CryoSleep), !is.na(Transported)) %>%
  group_by(CryoSleep) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss = 100 * sum(Transported) / n()
  )

View(perc_by_CryoSleep)

ggplot(data = perc_by_CryoSleep, mapping = aes(x = CryoSleep, y = percent_loss)) +
  geom_point()
### ^^^^  MUCH higher percent Transported for those in CryoSleep!!

################################################################################
###### Each individual variable shown have no full, direct influence ###########
################################################################################

colnames(train)

perc_by_grp1 <- train %>%
  filter(!is.na(Transported), !is.na(CryoSleep), !is.na(Age)) %>%
  group_by(CryoSleep, Age) %>%
  summarize(
    n = n(),
    count_transp = sum(Transported),
    percent_loss = 100 * sum(Transported) / n()
  )

View(perc_by_grp1)

ggplot(data = perc_by_grp1, mapping = aes(x = CryoSleep, y = Age)) +
  geom_point(mapping = aes(color = percent_loss))
