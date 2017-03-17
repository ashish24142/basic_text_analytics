
# Author<ashish>
# Here we are extracting reviews from IMDB
# Constructing WordCloud, COG from the negative and positive
# reviews of the movie.
# How this is helpful?
# This can be used as a basis by the movie team for planning the sequel, they can
# understand what people liked and what could have been better.

setwd('<SET YOUR WORKING DIRECTORY HERE')

require(rvest) || install.packages("rvest")
require(XML) || install.packages("XML")
require(tm) || install.packages("tm")
require(text2vec) || install.packages("text2vec")
require(slam) || install.packages("slam")
require(ggplot2) || install.packages("ggplot2")
require(wordcloud) || install.packages("wordcloud")
require(textir) || install.packages("textir")
require(stringr) || install.packages("stringr")
require(qdap) || install.packages("qdap") # ensure java is up to date!


library(rvest)
library(XML)
library(tm)
library(text2vec)
library(slam)
library(ggplot2)
library(wordcloud)
library(textir)
library(stringr)
library(qdap)
library(igraph)

#--------------------------------------------------------#
# Step 0 - Reading  reviews/rating from IMDB                    #
#--------------------------------------------------------#

counts = c(0,10,20,30,40)
reviews = NULL
ratings = NULL
for (j in counts){
  url1 = paste0("http://www.imdb.com/title/tt0499549/reviews?filter=love;filter=love;start=",j)
  url2 = paste0("http://www.imdb.com/title/tt0499549/reviews?filter=hate;filter=hate;start=",j)
  
  page1 = read_html(url1)
  page2 = read_html(url2)
  reviews1 = html_text(html_nodes(page1,'#tn15content div+ p'))
  reviews2 = html_text(html_nodes(page2,'#tn15content div+ p'))
  
  movie.nodes = html_nodes(page1,'h2+ img')
  rating1 = substr(html_attr(movie.nodes,  name='alt'), 
            0, 
            (str_locate(html_attr(movie.nodes,  name='alt'), '/')[1,1]-1))
  movie.nodes = html_nodes(page2,'h2+ img')
  rating2 = substr(html_attr(movie.nodes,  name='alt'), 
            0, 
            (str_locate(html_attr(movie.nodes,  name='alt'), '/')[1,1]-1))
  
  
  reviews.positive = setdiff(reviews1, c("*** This review may contain spoilers ***","Add another review"))
  reviews.negative = setdiff(reviews2, c("*** This review may contain spoilers ***","Add another review"))
  
  reviews = c(reviews,reviews.positive,reviews.negative)
  ratings = c(ratings, rating1, rating2)
}
ratings = as.numeric(ratings)
reviews = gsub("\n",' ',reviews)
reviews.ratings = data.frame(cbind(reviews, ratings))

write.csv(x=reviews.ratings,file='Avatar-Review.txt')



##----------------------------------------------------------#
##  Function to Clean Data                                  #
##----------------------------------------------------------#

text.clean = function(x)                    # text data
{ 
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}


##----------------------------------------------------------#
##  Function to display Word Cloud                          #
##----------------------------------------------------------#

displayWordCloud = function(dtm, display.name ){
  
  tsum = colSums(as.matrix(dtm))
  tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
  #head(tsum)
  #tail(tsum)
  
  windows()  # New plot window
  wordcloud(names(tsum), tsum,                        # words, their freqs 
            scale = c(4, 0.5),                        # range of word sizes
            1,                                        # min.freq of words to consider
            max.words = 200,                          # max #words
            colors = brewer.pal(8, "Dark2"))          # Plot results in a word cloud 
  display.name = paste(display.name, " - Wordcloud")
  title(sub = display.name)                           # title for the wordcloud display
}

#-----------------------------------------------------------#
# Cleaned up or 'distilled' COG PLot            #
#-----------------------------------------------------------#

distill.cog = function(mat1,  # input TCM ADJ MAT
                       title, # title for the graph
                       s,     # no. of central nodes
                       k1){   # max no. of connections  
  library(igraph)
  a = colSums(mat1) 		  # collect colsums into a vector obj a
  b = order(-a)               # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]           # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
} # func ends


#--------------------------------------------------------#
# Step 1 - Reading and Cleaning Corpus                   #
#--------------------------------------------------------#
review.df = read.csv("Avatar-Review.txt",
                     stringsAsFactors = FALSE, col.names = c("id", "text", "ratings") )
head(review.df$text, 5)
data = review.df[,-3]

# Read Stopwords list
stopwords.txt = "stopwords.txt"
stpw1 = readLines(stopwords.txt)            # read-in stopwords.txt
stpw2 = tm::stopwords('english')            # tm package stop word list
comn  = unique(c(stpw1, stpw2))             # Union of two list
stopwords = unique(gsub("'"," ",comn))      # final stop word lsit after removing punctuation

corpus  =  text.clean(data$text)            # applying func defined above to pre-process text corpus
corpus  =  removeWords(corpus, stopwords)   # removing stopwords created above
corpus  =  stripWhitespace(corpus)          # removing white space

#--------------------------------------------------------#
## Step 2: Create DTM using text2vec package             #
#--------------------------------------------------------#

t1 = Sys.time()

it_0 = itoken( corpus,
               #preprocessor = text.clean,
               tokenizer = word_tokenizer,  # using word & not space tokenizers
               ids = data$id)

#  func collects unique terms & corresponding statistics
vocab = create_vocabulary(it_0, ngram = c(2L, 2L))

# prune_vocabulary = filters input vocab & throws out v frequent & v infrequent terms
# here pruning only infrequent terms
pruned_vocab = prune_vocabulary(vocab, term_count_min = 10)

# creates a text vectorizer func used in constructing a dtm/tcm/corpus
vectorizer = vocab_vectorizer(pruned_vocab) 

# high-level function for creating a document-term matrix
dtm_0  = create_dtm(it_0, vectorizer) 

# Sort bi-gram with decreasing order of freq
tsum = as.matrix(t(rollup(dtm_0, 1, na.rm=TRUE, FUN = sum))) # find sum of freq for each term
tsum = tsum[order(tsum, decreasing = T),]       # terms in decreasing order of freq
#head(tsum)
#tail(tsum)

#-------------------------------------------------------#
# Code bi-grams as unigram in clean text corpus         #
#-------------------------------------------------------#

text2 = corpus
text2 = paste("",text2,"")

pb <- txtProgressBar(min = 1, max = (length(tsum)), style = 3) ; i = 0

for (term in names(tsum)){
  i = i + 1
  focal.term = gsub("_", " ",term)        # in case dot was word-separator
  replacement.term = term
  text2 = gsub(paste("",focal.term,""),paste("",replacement.term,""), text2)
  setTxtProgressBar(pb, i)
}

it_m = itoken(text2,     # function creates iterators over input objects to vocabularies, corpora, DTM & TCM matrices
              # preprocessor = text.clean,
              tokenizer = word_tokenizer,
              ids = data$id,
              progressbar = T)

vocab = create_vocabulary(it_m)     # vocab func collects unique terms and corresponding statistics
                          
pruned_vocab = prune_vocabulary(vocab, term_count_min = 1)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_m  = create_dtm(it_m, vectorizer)

print(difftime(Sys.time(), t1, units = 'sec'))

#--------------------------------------------------------#
## Step 2a - Using Term frequency(tf)  Build word cloud ##
#--------------------------------------------------------#

dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
dtm = dtm[a0,]                  # drop empty docs
corpus.nonempty.bool = a0
display.name = "Term Frequency"

#Word Cloud
displayWordCloud(dtm, display.name )


vectorizer = vocab_vectorizer(pruned_vocab, 
                              grow_dtm = FALSE, 
                              skip_grams_window = 5L)

tcm = create_tcm(it_m, vectorizer) # func to build a TCM

# COG
tcm.mat = as.matrix(tcm)                 # use tcm to view

adj.mat = tcm.mat + t(tcm.mat)             # since adjacency matrices are symmetric

z = order(colSums(adj.mat), decreasing = T)
adj.mat = adj.mat[z,z]

# Plot Simple Term Co-occurance graph
adj = adj.mat[1:30,1:30]

cog = graph.adjacency(adj, mode = 'undirected')
cog = simplify(cog)  
cog = delete.vertices(cog, V(cog)[ degree(cog) == 0 ])

windows()
display.name = "Term frequency(tf) - COG"

plot(cog, layout = layout.kamada.kawai, 
     main = display.name)


windows()
distill.cog(tcm.mat, 'Distilled COG',  10,  5)

# --------------------------------------------------------------------- #
# Using Term frequency inverse document frequency (tfidf)     #        
# --------------------------------------------------------------------- #

dtm.tfidf = tfidf(dtm,  normalize = FALSE)

display.name = "Term Frequency Inverse Document Frequency(tfidf)"

displayWordCloud(dtm.tfidf, display.name)

## adj.mat and distilled cog for tfidf DTMs ##

adj.mat = t(dtm.tfidf) %*% dtm.tfidf
diag(adj.mat) = 0
a0 = order(apply(adj.mat, 2, sum), decreasing = T)
adj.mat = as.matrix(adj.mat[a0[1:50], a0[1:50]])

windows()
distill.cog(adj.mat, 'Distilled COG',  10,  10)

#--------------------------------------------------------#
#             Sentiment Analysis                         #
#--------------------------------------------------------#

corpus.non.empty = corpus[corpus.nonempty.bool]    # remove empty docs from corpus

t1 = Sys.time()   # set timer

pol = polarity(corpus.non.empty)         # Calculate the polarity from qdap dictionary
wc = pol$all[,2]           # Word Count in each doc
val = pol$all[,3]          # average polarity score
p  = pol$all[,4]           # Positive words info
n  = pol$all[,5]           # Negative Words info  

timediff = Sys.time() - t1  # how much time did the above take?

head(pol$all)
head(pol$group)

plot(review.df$ratings, val, xlab= "Movies Ratings", ylab= "Average polarity score")

cor(review.df$ratings, val)


print(c("Time taken to calculated the polarity : ", timediff))

#pdf <- data.frame(table(unlist(p)))
#ndf <- data.frame(table(unlist(n)))


#positive_words = unique(setdiff(unlist(p),"-"))  # Positive words list
#negative_words = unique(setdiff(unlist(n),"-"))  # Negative words list

#print(positive_words)       # Print all the positive words found in the corpus
#print(negative_words)       # Print all neg words


#--------------------------------------------------------#
#   Create Postive Words wordcloud                       #
#--------------------------------------------------------#

pos.tdm = dtm[,which(colnames(dtm) %in% positive_words)]
m = as.matrix(pos.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows() # opens new image window
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))
title(sub = "Positive Words - Wordcloud")

# plot barchart for top tokens
test = as.data.frame(v[1:15])
windows() # opens new image window
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#--------------------------------------------------------#
#  Create Negative Words wordcloud                       #
#--------------------------------------------------------#

neg.tdm = dtm[,which(colnames(dtm) %in% negative_words) ]
m = as.matrix(neg.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))         
title(sub = "Negative Words - Wordcloud")

# plot barchart for top tokens
test = as.data.frame(v[1:15])
windows()
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##--------------------------------------------------------------------##
for (i in 1:10) 
  gc(reset = T)


