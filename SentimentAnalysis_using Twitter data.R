library(shiny)

library(shiny)
ui<-fluidPage(
  headerPanel("Twitter Analysis"),
  sidebarPanel(
    textInput("main","Enter the topic",value=" "),
    submitButton("Run")),
  mainPanel(
    h4("Pie Chart"),
    plotOutput("plot"),
    tableOutput("table")
  )
)

options(shiny.sanitize.errors = TRUE)
server<-function(input,output){
  doit<-reactive({
    library(twitteR)
    library(ROAuth)
    library(plyr)
    library(dplyr)
    library(stringr)
    library(ggplot2) 
    URL.req <- 'https://api.twitter.com/oauth/request_token'
    URL.access <- 'https://api.twitter.com/oauth/access_token'
    URL.authenticate <- 'https://api.twitter.com/oauth/authorize'
    Key.consumer <- 'lbZhiAmm8ix9c7h6QuyHWx6VR' #Input your consumer key
    Secret.consumer <- 'AXcSs2aacYtbyIsAzmRnhh59bWtYFURWUsk6eQCkIscI9PJZ26'  #Input your consumer secret 
    Token.access<-'825414268577161216-wVKtEnYuO3dfsEbC3MCkQeAsuPRuk1M'#Input your access token 
    Secret.access<-'B22bPBrrBu6v0ZDSUthFxIm3wUUothzik052Zp9yNe2eg'#Input your access secret
    setup_twitter_oauth(Key.consumer, Secret.consumer, Token.access, Secret.access)
    
    
    count.tweet <- 50
    tweets<- searchTwitter(input$main, n=count.tweet, lang="en",retryOnRateLimit=1)
    
    df<-twListToDF(tweets)
    
    data<-df
    
    support.words<-c("well","breach", "good", "congratulations","learn","glad","happy","better","smart","develop","thank", "benifit","solve","intiative","innovative") 
    against.words<-c("no","blame","fuck","worst","useless","problem","poor","error","dull","bad","destroy")
    
    
    
    #support.words<-as.vector(read.table("F:/project/twitter/hello/sentimentanalysis/supports.txt",col.names="value")$value)
    #against.words<-as.vector(read.table("F:/project/twitter/hello/sentimentanalysis/against.txt",col.names="value")$value)
    temp <- subset(data, !duplicated(data$text))
    position<-sample(1:length(temp$text),10,replace=T)
    temp<-temp[position,]
    information <- temp
    
    tweetcount <- function(tweettext, support.words, against.words, .progress='none')
    {
      require(plyr)
      require(stringr)
      counts <- laply(tweettext, function(tweets, support.words, against.words){       
        tweets <- gsub('[[:punct:]]', "", tweets)
        tweets <- gsub('[[:cntrl:]]', "", tweets)
        tweets<-gsub("iâ???Tm","",tweets)
        tweets <- tolower(tweets)
        words <- unlist(str_split(tweets," "))
        supportcount <- match(words, support.words)
        againstcount <- match(words, against.words)
        supportcount <- !is.na(supportcount)
        againstcount <- !is.na(againstcount)
        score <- sum(supportcount) - sum(againstcount)
        return(score)
      }, support.words, against.words, .progress=.progress)
      counts.df <- data.frame(score=counts, text=tweettext)
      return(counts.df)
    }
    
    counts <- tweetcount(information$text, support.words, against.words, .progress='text')
    
    stat <- counts
    stat$created <- temp$created
    stat$created <- as.Date(stat$created)
    stat <- transform(stat, tweet=ifelse(stat$score > 0, 'support', ifelse(stat$score < 0, 'against', 'neutral')))
    by.tweet <- group_by(stat, tweet, created)
    by.tweet <- summarize(by.tweet, number=n())
    
    
    data1<-by.tweet
    data1<-data1[,-c(2)]
    against<-data1[which(data1$tweet=="against"),]$number
    support<-data1[which(data1$tweet=="support"),]$number
    neutral<-data1[which(data1$tweet=="neutral"),]$number
    
    all<-sum(against,support,neutral)
    
    against1<-100*sum(against)/all
    support1<-100*sum(support)/all
    neutral1<-100*sum(neutral)/all
    
    df <- data.frame(
      group = c("Against", "Support", "neutral"),
      value = c(against1, support1, neutral1)
    )
    bp<- ggplot(df, aes(x="", y=value, fill=group))+
      geom_bar(width = 1, stat = "identity")
    
    piechart <- bp + coord_polar("y", start=0)+
      ggtitle(input$main)
    piechart
  })
  output$plot<-renderPlot({doit()})
  output$table<-renderDataTable({df})
}

shinyApp(ui,server)