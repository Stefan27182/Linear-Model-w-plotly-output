

#*********************************

#Author: Stefan Skinner (GitHub - Stefan27182)
#License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)

#*********************************

Crude <- read_delim("data from Yahoo finance/Crude Oil prices _ weekly _ 2000 - 2010 .tsv", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(`Adj Close**` = col_skip()), 
                    na = "null", trim_ws = TRUE)
#View(head(Crude))
#summary(Crude)


tmp_vector <- tibble()

for(x in 1:nrow(Crude))
{
  tmp_vector[x,1] <- as_date(strptime(Crude[x, 1], format = "%b%d,%Y"))
  
} # end for

Crude[, 1] <- tmp_vector

Crude$Date <- as.Date(Crude$Date)

Crude <- Crude[order(Crude$Date),]

Crude$Volume <- as.numeric(gsub(",","",Crude$Volume))


for(i in 1:nrow(Crude)) # replace dashes and null's with NA
{
  for(m in 2:ncol(Crude))
  {
    if(as.character(Crude[i,m]) == "-" || as.character(Crude[i,m]) == "null")
    {
      Crude[i,m] <- NA
    } # end if
  } # end inner for
} # end outer for

#-----#

for(i in 6:(nrow(Crude)-5)) # replace NA's w a moving average 5x5 (will miss first and last five rows in data frame, which must then be handled some other way e.g. MICE)
{
  for(m in 2:ncol(Crude))
  {
    if(is.na(Crude[i,m]) == TRUE)
    { 
      vec_1 <- vector(length = 5)
      vec_2 <- vector(length = 5)
      for(h in 1:5)
      {
        vec_1[h] <- Crude[(i+h), m]
        vec_2[h] <- Crude[(i-h), m]
        Crude[i,m] <- ((mean(unlist(vec_1), na.rm = TRUE) + mean(unlist(vec_2), na.rm = TRUE))/2)
      } # end inner for
    } # end na if
  } # end column cycle for
} # end row cycle for

#-----#

testr <- sum(as.integer(is.na(Crude)))

if(testr > 0)
{
  
  test_mice <- mice(data = Crude[, c(2:6)], m = 5, method = "cart", maxit = 50, seed = 500)
  
  post_mice <- complete(test_mice, 5)
  
  Crude$Open <- post_mice$Open
  
  Crude$High <- post_mice$High
  
  Crude$Low <- post_mice$Low
  
  Crude$Close <- post_mice$Close
  
  Crude$Volume <- post_mice$Volume
  
} # end if

#-----#

Crude_02_through_05 <- Crude %>% filter(grepl('2002|2003|2004|2005', Date))
colnames(Crude_02_through_05)[5] <- c("Close")
#plot(Crude$Date, Crude$Close)

#View(head(Crude_Oil_02_through_05))

#-----#

DJIA <- read_delim("./data from Yahoo finance/DJIA weekly 2000 - 2010 .tsv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(`Adj Close**` = col_skip()), 
                   trim_ws = TRUE)


colnames(DJIA)[5] <- c("Close")
#View(head(DJIA))
#View(DJIA)
#summary(DJIA)


tmp_vector <- tibble()

for(x in 1:nrow(DJIA))
{
  tmp_vector[x,1] <- as_date(strptime(DJIA[x, 1], format = "%b%d,%Y"))
  
} # end for

DJIA[, 1] <- tmp_vector

DJIA$Date <- as.Date(DJIA$Date)

DJIA <- DJIA[order(DJIA$Date),]
# 
# DJIA$Volume <- as.numeric(gsub(",","",DJIA$Volume))


for(i in 1:nrow(DJIA)) # replace dashes and null's with NA
{
  for(m in 2:ncol(DJIA))
  {
    if(as.character(DJIA[i,m]) == "-" || as.character(DJIA[i,m]) == "null")
    {
      DJIA[i,m] <- NA
    } # end if
  } # end inner for
} # end outer for

#-----#

for(i in 6:(nrow(DJIA)-5)) # replace NA's w a moving average 5x5 (will miss first and last five rows in data frame, which must then be handled some other way e.g. MICE)
{
  for(m in 2:ncol(DJIA))
  {
    if(is.na(DJIA[i,m]) == TRUE)
    { 
      vec_1 <- vector(length = 5)
      vec_2 <- vector(length = 5)
      for(h in 1:5)
      {
        vec_1[h] <- DJIA[(i+h), m]
        vec_2[h] <- DJIA[(i-h), m]
        DJIA[i,m] <- ((mean(unlist(vec_1), na.rm = TRUE) + mean(unlist(vec_2), na.rm = TRUE))/2)
      } # end inner for
    } # end na if
  } # end column cycle for
} # end row cycle for

#-----#

testr <- sum(as.integer(is.na(DJIA)))

if(testr > 0)
{
  
  test_mice <- mice(data = DJIA[, c(2:6)], m = 5, method = "cart", maxit = 50, seed = 500)
  
  post_mice <- complete(test_mice, 5)
  
  DJIA$Open <- post_mice$Open
  
  DJIA$High <- post_mice$High
  
  DJIA$Low <- post_mice$Low
  
  DJIA$Close <- post_mice$Close
  
  DJIA$Volume <- post_mice$Volume
  
} # end if

#-----#

DJIA_02_through_05 <- DJIA %>% filter(grepl('2002|2003|2004|2005', Date))

#View(DJIA_02_through_05)

#-----#

E_Mini <- read_delim("././data from Yahoo finance/E-Mini S&P 500 _ weekly _ 2001 - 2010 .tsv", 
                     delim = "\t", escape_double = FALSE, 
                     col_types = cols(`Adj Close**` = col_skip()), 
                     trim_ws = TRUE)

#View(head(E_Mini))
colnames(E_Mini)[5] <- c("Close")

#View(head(E_Mini))
#summary(E_Mini)


tmp_vector <- tibble()

for(x in 1:nrow(E_Mini))
{
  tmp_vector[x,1] <- as_date(strptime(E_Mini[x, 1], format = "%b%d,%Y"))
  
} # end for

E_Mini[, 1] <- tmp_vector

E_Mini$Date <- as.Date(E_Mini$Date)

E_Mini <- E_Mini[order(E_Mini$Date),]
# 
# E_Mini$Volume <- as.numeric(gsub(",","",E_Mini$Volume))


for(i in 1:nrow(E_Mini)) # replace dashes and null's with NA
{
  for(m in 2:ncol(E_Mini))
  {
    if(as.character(E_Mini[i,m]) == "-" || as.character(E_Mini[i,m]) == "null")
    {
      E_Mini[i,m] <- NA
    } # end if
  } # end inner for
} # end outer for

#-----#

for(i in 6:(nrow(E_Mini)-5)) # replace NA's w a moving average 5x5 (will miss first and last five rows in data frame, which must then be handled some other way e.g. MICE)
{
  for(m in 2:ncol(E_Mini))
  {
    if(is.na(E_Mini[i,m]) == TRUE)
    { 
      vec_1 <- vector(length = 5)
      vec_2 <- vector(length = 5)
      for(h in 1:5)
      {
        vec_1[h] <- E_Mini[(i+h), m]
        vec_2[h] <- E_Mini[(i-h), m]
        E_Mini[i,m] <- ((mean(unlist(vec_1), na.rm = TRUE) + mean(unlist(vec_2), na.rm = TRUE))/2)
      } # end inner for
    } # end na if
  } # end column cycle for
} # end row cycle for

#-----#

testr <- sum(as.integer(is.na(E_Mini)))

if(testr > 0)
{
  
  test_mice <- mice(data = E_Mini[, c(2:6)], m = 5, method = "cart", maxit = 50, seed = 500)
  
  post_mice <- complete(test_mice, 5)
  
  E_Mini$Open <- post_mice$Open
  
  E_Mini$High <- post_mice$High
  
  E_Mini$Low <- post_mice$Low
  
  E_Mini$Close <- post_mice$Close
  
  E_Mini$Volume <- post_mice$Volume
  
} # end if

#-----#

E_Mini_02_through_05 <- E_Mini %>% filter(grepl('2002|2003|2004|2005', Date))

#View(E_Mini_02_through_05)

#-----#

Silver <- read_delim("./data from Yahoo finance/Silver prices _ weekly _ 2000 - 2010 .tsv", 
                     delim = "\t", escape_double = FALSE, 
                     col_types = cols(`Adj Close**` = col_skip()), 
                     trim_ws = TRUE) 


colnames(Silver)[5] <- c("Close")

#View(head(Silver))
#summary(Silver)


tmp_vector <- tibble()

for(x in 1:nrow(Silver))
{
  tmp_vector[x,1] <- as_date(strptime(Silver[x, 1], format = "%b%d,%Y"))
  
} # end for

Silver[, 1] <- tmp_vector

Silver$Date <- as.Date(Silver$Date)

Silver <- Silver[order(Silver$Date),]

Silver$Volume <- as.numeric(gsub(",","",Silver$Volume))


for(i in 1:nrow(Silver)) # replace dashes and null's with NA
{
  for(m in 2:ncol(Silver))
  {
    if(as.character(Silver[i,m]) == "-" || as.character(Silver[i,m]) == "null")
    {
      Silver[i,m] <- NA
    } # end if
  } # end inner for
} # end outer for

#-----#

for(i in 6:(nrow(Silver)-5)) # replace NA's w a moving average 5x5 (will miss first and last five rows in data frame, which must then be handled some other way e.g. MICE)
{
  for(m in 2:ncol(Silver))
  {
    if(is.na(Silver[i,m]) == TRUE)
    { 
      vec_1 <- vector(length = 5)
      vec_2 <- vector(length = 5)
      for(h in 1:5)
      {
        vec_1[h] <- Silver[(i+h), m]
        vec_2[h] <- Silver[(i-h), m]
        Silver[i,m] <- ((mean(unlist(vec_1), na.rm = TRUE) + mean(unlist(vec_2), na.rm = TRUE))/2)
      } # end inner for
    } # end na if
  } # end column cycle for
} # end row cycle for

#-----#

testr <- sum(as.integer(is.na(Silver)))

if(testr > 0)
{
  
  test_mice <- mice(data = Silver[, c(2:6)], m = 5, method = "cart", maxit = 50, seed = 500)
  
  post_mice <- complete(test_mice, 5)
  
  Silver$Open <- post_mice$Open
  
  Silver$High <- post_mice$High
  
  Silver$Low <- post_mice$Low
  
  Silver$Close <- post_mice$Close
  
  Silver$Volume <- post_mice$Volume
  
} # end if

#-----#

Silver_02_through_05 <- Silver %>% filter(grepl('2002|2003|2004|2005', Date))

#-----#

Shenzhen <- read_csv("data from Yahoo finance/Shenzhen .csv", 
                     col_types = cols(Open = col_double(), 
                                      High = col_double(), Low = col_double(), 
                                      Close = col_double(), `Adj Close` = col_skip(), 
                                      Volume = col_skip()))

#View(tail(Shenzhen, 25))
#summary(Shenzhen)
#View(Shenzhen)
#
# tmp_vector <- tibble()
# 
# for(x in 1:nrow(Shenzhen))
# {
#   tmp_vector[x,1] <- as_date(strptime(Shenzhen[x, 1], format = "%b%d,%Y"))
#   
# } # end for
# 
# Shenzhen[, 1] <- tmp_vector
# 
# Shenzhen$Date <- as.Date(Shenzhen$Date)
# 
# Shenzhen <- Shenzhen[order(Shenzhen$Date),]
# 
# Shenzhen$Volume <- as.numeric(gsub(",","",Shenzhen$Volume))

counter <- 0
for(i in 1:nrow(Shenzhen)) # replace dashes and null's with NA
{
  for(m in 2:ncol(Shenzhen))
  {
    if(as.character(Shenzhen[i,m]) == "-" || as.character(Shenzhen[i,m]) == "null")
    {
      counter <- counter + 1
      print(paste("in dash and null replacement if, counter = ", counter))
      Shenzhen[i,m] <- NA
    } # end if
  } # end inner for
} # end outer for

#-----#

for(i in 6:(nrow(Shenzhen)-5)) # replace NA's w a moving average 5x5 (will miss first and last five rows in data frame, which must then be handled some other way e.g. MICE)
{
  for(m in 2:ncol(Shenzhen))
  {
    if(is.na(Shenzhen[i,m]) == TRUE)
    { 
      vec_1 <- vector(length = 5)
      vec_2 <- vector(length = 5)
      for(h in 1:5)
      {
        vec_1[h] <- Shenzhen[(i+h), m]
        vec_2[h] <- Shenzhen[(i-h), m]
        if(h == 5){Shenzhen[i,m] <- ((mean(unlist(vec_1), na.rm = TRUE) + mean(unlist(vec_2), na.rm = TRUE))/2)}
      } # end inner for
    } # end na if
  } # end column cycle for
} # end row cycle for

#-----#

testr <- sum(as.integer(is.na(Shenzhen)))

if(testr > 0)
{
  
  test_mice <- mice(data = Shenzhen[, c(2:6)], m = 5, method = "cart", maxit = 50, seed = 500)
  
  post_mice <- complete(test_mice, 5)
  
  Shenzhen$Open <- post_mice$Open
  
  Shenzhen$High <- post_mice$High
  
  Shenzhen$Low <- post_mice$Low
  
  Shenzhen$Close <- post_mice$Close
  
} # end if

#-----#
#View(Shenzhen_02_through_05)
Shenzhen_02_through_05 <- Shenzhen %>% filter(grepl('2002|2003|2004|2005', Date))

#View(head(Shenzhen_02_through_05))

#-----#

TSE <- read_csv("data from Yahoo finance/^GSPTSE.csv", 
                col_types = cols(Open = col_double(), 
                                 High = col_double(), Low = col_double(), 
                                 Close = col_double(), `Adj Close` = col_skip(), 
                                 Volume = col_double()))

#View(head(TSE))
#summary(TSE)

#
# tmp_vector <- tibble()
# 
# for(x in 1:nrow(TSE))
# {
#   tmp_vector[x,1] <- as_date(strptime(TSE[x, 1], format = "%b%d,%Y"))
#   
# } # end for
# 
# TSE[, 1] <- tmp_vector
# 
# TSE$Date <- as.Date(TSE$Date)
# 
# TSE <- TSE[order(TSE$Date),]
# 
# TSE$Volume <- as.numeric(gsub(",","",TSE$Volume))


for(i in 1:nrow(TSE)) # replace dashes and null's with NA
{
  for(m in 2:ncol(TSE))
  {
    if(as.character(TSE[i,m]) == "-" || as.character(TSE[i,m]) == "null")
    {
      TSE[i,m] <- NA
    } # end if
  } # end inner for
} # end outer for

#-----#

for(i in 6:(nrow(TSE)-5)) # replace NA's w a moving average 5x5 (will miss first and last five rows in data frame, which must then be handled some other way e.g. MICE)
{
  for(m in 2:ncol(TSE))
  {
    if(is.na(TSE[i,m]) == TRUE)
    { 
      vec_1 <- vector(length = 5)
      vec_2 <- vector(length = 5)
      for(h in 1:5)
      {
        vec_1[h] <- TSE[(i+h), m]
        vec_2[h] <- TSE[(i-h), m]
        TSE[i,m] <- ((mean(unlist(vec_1), na.rm = TRUE) + mean(unlist(vec_2), na.rm = TRUE))/2)
      } # end inner for
    } # end na if
  } # end column cycle for
} # end row cycle for

#-----#

testr <- sum(as.integer(is.na(TSE)))

if(testr > 0)
{
  
  test_mice <- mice(data = TSE[, c(2:6)], m = 5, method = "cart", maxit = 50, seed = 500)
  
  post_mice <- complete(test_mice, 5)
  
  TSE$Open <- post_mice$Open
  
  TSE$High <- post_mice$High
  
  TSE$Low <- post_mice$Low
  
  TSE$Close <- post_mice$Close
  
  TSE$Volume <- post_mice$Volume
  
} # end if

#-----#

TSE_02_through_05 <- TSE %>% filter(grepl('2002|2003|2004|2005', Date))

#-----#  #-----#

nrow(Crude_02_through_05)
nrow(DJIA_02_through_05)
nrow(E_Mini_02_through_05)
nrow(Silver_02_through_05)
nrow(Shenzhen_02_through_05)
nrow(TSE_02_through_05)

lm_frame <- as.data.frame(cbind(Crude_02_through_05$Close, DJIA_02_through_05$Close, E_Mini_02_through_05$Close, Silver_02_through_05$Close, Shenzhen_02_through_05$Close, TSE_02_through_05$Close))
#View(Shenzhen_02_through_05)
colnames(lm_frame)[1:6] <- c("Crude", "DJIA", "E_Mini (S&P 500)", "Silver", "Shenzhen", "TSE")

#View(head(lm_frame))

Com_ind_test.lm <- lm(formula = E_Mini_02_through_05$Close ~ DJIA_02_through_05$Close + TSE_02_through_05$Close+ Crude_02_through_05$Close+ Silver_02_through_05$Close+ Shenzhen_02_through_05$Close, data = lm_frame)

#summary(Com_ind_test.lm)

Com_ind_w_Shenzhen.lm <- lm(formula = E_Mini_02_through_05$Close ~ DJIA_02_through_05$Close + TSE_02_through_05$Close + Shenzhen_02_through_05$Close, data = lm_frame)

#summary(Com_ind_w_Shenzhen.lm)

Com_ind_.lm <- lm(formula = E_Mini_02_through_05$Close ~ DJIA_02_through_05$Close + TSE_02_through_05$Close, data = lm_frame)

#summary(Com_ind_.lm)

new_lm_frame <- as.data.frame(matrix(nrow = nrow(TSE_02_through_05), ncol = 7))

new_lm_frame[ , 1] <- c(DJIA_02_through_05$Date)

new_lm_frame[,2:7] <- lm_frame_df[,1:6]

colnames(new_lm_frame)[1:7] <- c("Date", "Crude", "DJIA", "E_Mini", "Silver", "Shenzhen", "TSE")

#View(new_lm_frame_df)

#-----***-----#

TSE_DJIA_intersection <- ggplot(new_lm_frame_df, aes(Date)) +
  geom_line(aes(y = DJIA_02_through_05$Close, color = "DJIA")) + 
  geom_line(aes(y = TSE_02_through_05$Close, color = "TSE")) +
  ylab("DJIA & TSE") +
  guides(color = guide_legend(title = "Indexes"))
ggplotly(TSE_DJIA_intersection)

E_Mini_plot <- ggplot(new_lm_frame_df, aes(Date)) +
  geom_line(aes(y = E_Mini_02_through_05$Close), colour = "violetred") + ylab("E_Mini (S&P 500)") 


TSE_plot <- ggplot(new_lm_frame_df, aes(Date, TSE)) +
  geom_line(aes(y = TSE_02_through_05$Close), colour = 'turquoise3') + ylab("TSE in CAD (Canadian Dollars)") 


DJIA_plot <- ggplot(new_lm_frame_df, aes(Date)) +
  geom_line(aes(y = DJIA_02_through_05$Close), colour = "lightcoral") + ylab("DJIA") 


Shenzhen_plot <- ggplot(new_lm_frame_df, aes(Date)) +
  geom_line(aes(y = Shenzhen_02_through_05$Close), colour = "aquamarine4") + ylab("Shenzhen in Renminbi ¥") 


# plot_grid(E_Mini_plot, DJIA_plot, TSE_plot, Shenzhen_plot, labels = c('E_Mini (S&P 500)', '          DJIA', '          TSE', 'Shenzhen'))
# 
# ggplotly(E_Mini_plot)
# E_Mini_plot
# 
# ggplotly(TSE_plot)
# TSE_plot
# 
# ggplotly(DJIA_plot)
# DJIA_plot
# 
# ggplotly(Shenzhen_plot)
# Shenzhen_plot

