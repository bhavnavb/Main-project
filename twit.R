f1 <-function(){
consumer_key <- 'ZwPK8D9hTRGVJrPc4msHQdrMH'
consumer_secret <- 'BV8eHfDGRxE8LhWn17qL26bYKBzA9juL4kL6ohEEv3CtwkD4Ji'
access_token <- '3814635013-WBR42YvxroCxmrApWJOdMJwpSc0Oa11GY9ACtmB'
access_secret <- 'nBLNAJaviI0oPlpXszvFa14BnpU1et1qlPPsEInZ6lJLc'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

pos.words <- scan('/home/bhavna/rprog/positive-words.txt', what='character', comment.char=';')
neg.words <- scan('/home/bhavna/rprog/negative-words.txt', what='character', comment.char=';');
iphone.tweets <- searchTwitter('#iphone', n=15, lang="en")
nexus.tweets <- searchTwitter('#nexus', n=15, lang="en")
iphone.text=laply(iphone.tweets,function(t) t$getText())
nexus.text=laply(nexus.tweets,function(t) t$getText())
df_nx <- as.data.frame(nexus.text)
df_ip <- as.data.frame(iphone.text)
json_nx <- toJSON(iphone.text)
json_ip <- toJSON(nexus.text)
m <- mongo("iphonecollection",db="project")
m$insert(df_ip)
df_ip_out<-m$find()
ip.txt<- as.character(df_ip_out$iphone.text)
m<- mongo("nexuscollection",db="project")
m$insert(df_nx)
df_nx_out<-m$find()
nx.txt<- as.character(df_nx_out$nexus.text)
iphone.scores <- score.sentiment(ip.txt, pos.words,neg.words, .progress='text')
nexus.scores <- score.sentiment(nx.txt, pos.words,neg.words, .progress='text')
iphone.scores$team = 'iphone'
iphone.scores$code = 'BAL'
nexus.scores$team = 'nexus'
nexus.scores$code = 'NYA'
aleast.scores = rbind(iphone.scores, nexus.scores)
ggplot(data=aleast.scores) +
  geom_bar(mapping=aes(x=score, fill=team), binwidth=1) + 
  facet_grid(team~.) +
  theme_bw() + scale_color_brewer() +
  labs(title="Comparison of sentiment analysis of Nexus and iphones")
}
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}