
install.packages("twitteR")
library(twitteR) 
install.packages("plyr")
library(plyr) 
#twitter api informacije za oauth
api_key <- " "

api_secret <- " "

access_token <- " "

access_token_secret <- " "

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

 

delta.tweets=searchTwitter('@delta',n=1500)
   
tweet=delta.tweets[[1]]

hu.liu.pos=scan('positive-words.txt',what='character',comment.char=';')
hu.liu.neg=scan('negative-words.txt',what='character',comment.char=';')



score.sentiment=function(sentences, pos.words,neg.words, progress='none' )
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
    require(plyr)
    require(stringr)
     
    #kad imamo vektor rečenica, plyr će se pobrinuti za listu

       
    
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
         
        
        #priprema rečenica sa R-ovom funkcijom za zamjenu gsub() regex:
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        # pretvaranje u lower case
        sentence = tolower(sentence)
 
        # podjela riječi, str_split funkcija je u stringr paketu
        word.list = str_split(sentence, '\\s+')
        
        words = unlist(word.list)
 
        
        # poređenje riječi sa riječima iz riječnika (pozitivne i negativne riječi)
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
     
        #match() vraća poziciju poređene riječi ili ništa
        #nama treba samo TRUE/FALSE:
        
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
 
        # TRUE/FALSE sum() funkcija tretira kao 1/0 respektivno
        score = sum(pos.matches) - sum(neg.matches)
 
        return(score)
    }, pos.words, neg.words, .progress=.progress )
 
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}






#delta
delta.tweets = searchTwitter('@delta', n=1500)
delta.text = laply(delta.tweets, function(t) t$getText() )
delta.scores = score.sentiment(delta.text, pos.words,neg.words, .progress='text')
delta.scores$airline = 'Delta'
delta.scores$code = 'DL'

#american
american.tweets = searchTwitter('@americanair', n=1500)
american.text = laply(american.tweets, function(t) t$getText() )
american.scores = score.sentiment(american.text, pos.words,neg.words, .progress='text')
american.scores$airline = 'American'
american.scores$code = 'AA'
#southwest
southwest.tweets = searchTwitter('@southwestair', n=1500)
southwest.text = laply(southwest.tweets, function(t) t$getText() )
southwest.scores = score.sentiment(southwest.text, pos.words,neg.words, .progress='text')
southwest.scores$airline = 'Southwest'
southwest.scores$code = 'WN'
#united
united.tweets = searchTwitter('@united', n=1500)
united.text = laply(united.tweets, function(t) t$getText() )
united.scores = score.sentiment(united.text, pos.words,neg.words, .progress='text')
united.scores$airline = 'united'
united.scores$code = 'UA'
#jetblue
jetblue.tweets = searchTwitter('@jetblue', n=1500)
jetblue.text = laply(jetblue.tweets, function(t) t$getText() )
jetblue.scores = score.sentiment(jetblue.text, pos.words,neg.words, .progress='text')
jetblue.scores$airline = 'jetblue'
jetblue.scores$code = 'B6'

all.scores = rbind( american.scores,   delta.scores,
jetblue.scores, southwest.scores, united.scores  )

 ggplot(data=all.scores) +  geom_histogram(mapping=aes(x=score, fill=airline), binwidth=1) +
 facet_grid(airline~.) + 
 #theme_bw() + scale_fill_brewer() 

all.scores$very.pos = as.numeric( all.scores$score >= 2 )
all.scores$very.neg = as.numeric( all.scores$score <= -2 )

twitter.df = ddply(all.scores, c('airline', 'code'), summarise, pos.count = sum( very.pos ), neg.count = sum( very.neg ) )
twitter.df$all.count = twitter.df$pos.count + twitter.df$neg.count
twitter.df$score = round( 100 * twitter.df$pos.count / twitter.df$all.count )

#doBy paket
orderBy(~-score, twitter.df)

library(XML)
acsi.url = 'http://www.theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines'
acsi.df = readHTMLTable(acsi.url, header=T, which=1,stringsAsFactors=F)
# only keep column #1 (name) and #18 (2010 score)
#acsi.df = acsi.df[,c(1,18)]
acsi.df = acsi.df[,c(1,23)] #kolona za 2015 godinu
head(acsi.df,1)

colnames(acsi.df) = c('airline', 'score') #scrapping podataka, redukcija dimenzija da odgovaraju objema tabelama
acsi.df$code = c('B6', 'WN', NA, NA, NA,'DL', 'AA',NA,'UA', NA, NA,NA,NA,NA)
acsi.df$score = as.numeric(acsi.df$score)

compare.df = merge(twitter.df, acsi.df, by='code', suffixes=c('.twitter', '.acsi'))

#compare.df = subset(compare.df, all.count > 40)
compare.df = subset(compare.df, score.twitter > 40) #southwest problem, 9 score za twitter a 70 sa stranice

ggplot( compare.df ) +
geom_point(aes(x=score.twitter,
y=score.acsi,
color=airline.twitter), size=5) +
geom_smooth(aes(x=score.twitter,
y=score.acsi, group=1), se=F,
method="lm") +
theme_bw()  
