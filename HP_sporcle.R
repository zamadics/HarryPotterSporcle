### data project ###

setwd("C:/Users/diein/Dropbox/Semester X/sicss/HP")
library(rvest)
library(VGAM)
library(ggplot2)

main.page <- html(x = "https://www.sporcle.com/games/GeoGod/allharrypotter/results")

# Get link text
mentions <- main.page %>% # feed `main.page` to the next step
  html_nodes(".name") %>% # get the CSS nodes
  html_text() # extract the link text

character <- main.page %>% # feed `main.page` to the next step
  html_nodes(".col a") %>% # get the CSS nodes
  html_text() # extract the link text

percent <- main.page %>% # feed `main.page` to the next step
  html_nodes(".percent") %>% # get the CSS nodes
  html_text() # extract the link text

mentions1 <- NA
for(i in 1:length(mentions)){
  mentions1[i] <- as.numeric(gsub(',','',mentions[i]))
}

percent1 <- NA
for(i in 1:length(percent)){
  percent1[i] <- as.numeric(gsub('%','',percent[i]))
}

df <- data.frame(character, mentions1, percent1)

# write.csv(df, file = "HPsporcle.csv")

df <- read.csv("HPsporcle.csv")


# m <- lm(percent1 ~ log(mentions1), data = df)
m <- vglm(percent1 ~ log(mentions1), tobit(Upper = 100), data = df)
summary(m)
df$res <- df$percent1 - fitted(m)

low <- tail(sort(df$res),10)[1]
hi <- head(sort(df$res),10)[10]

ggplot(aes(x=log(mentions1), y=percent1), data = df) + geom_point() + geom_smooth(method='lm', se = F) +
  geom_text(data=subset(df, res < hi | res > low), aes(label=character), size=3, nudge_x = 0, nudge_y = -3) + 
  xlab("Mentions (Natural Log)") + ylab("Sporcle Percent Guessed") 


# worst usage
worst <- head(df[order(df$res),], 10)
worst <- worst[, 2:5]
colnames(worst) <- c("Character", "Mentions", "Percent", "Residual")

# best usage
best <- head(df[order(df$res, decreasing = T),], 10)
best <- best[, 2:5]
colnames(best) <- c("Character", "Mentions", "Percent", "Residual")

best
worst
