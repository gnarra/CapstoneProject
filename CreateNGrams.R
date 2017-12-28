# Sourcing common functions
setwd("C:/Gopal/Coursera/DataScienceSpecialization-John Hopkins/Capstone Project")
source("CommonFunctions.R")

# The FilePath
filePath <- "C:/Gopal/Coursera/DataScienceSpecialization-John Hopkins/Capstone Project/final/en_US/"

# Read the files 
twitterLines <- readFile(filePath, "en_US.twitter.txt")
blogLines <- readFile(filePath, "en_US.blogs.txt")
newsLines <- readFile(filePath, "en_US.news.txt")

# Set the Seed for reproducibility
set.seed(3333)

# Sample the 10% Data
sampleSize <- 0.10
sampleData <- c(sample(blogLines, length(blogLines) * sampleSize),
                sample(newsLines, length(newsLines) * sampleSize),
                sample(twitterLines, length(twitterLines) * sampleSize))

Encoding(sampleData)  <- "UTF-8"

# Remove the Temporary variables
rm(twitterLines, blogLines, newsLines)

# TextMining Code -- Does not work
# Create the tm VCorpus
# myTMCorpus <- Corpus(VectorSource(sampleData))
# myTMCorpus <- cleanCorpus(myTMCorpus)
# inspect(myTMCorpus[[4]])
# out <- sapply(myTMCorpus, function(x){x$content})
# out
#
# Tri-Grams Plots
# trigramsTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
# tdm <- TermDocumentMatrix(myTMCorpus, control = list(tokenize = trigramsTokenizer))
# rsparse <- removeSparseTerms(tdm, 0.9999)
# Terms(rsparse)[1:100]
# trigramsWords <- getFreq(rsparse)
# trigramsWords <- getFreq(removeSparseTerms(TermDocumentMatrix(myTMCorpus, control = list(tokenize = trigramsTokenizer)), 0.9999))
# makePlot(trigramsWords, "Top 40 Tri-Grams", 40)

# Get the TM Corpus
tmCorpus <- getTMCorpus(sampleData)

# Tokenize
qEdaTokens <- tokenizeData(tmCorpus)

# Build the NGrams
# BiGrams (QuantEDA)
biGramsDfm <- dfm(tokens_ngrams(qEdaTokens, 2, 0:3))
biGramsDF <- getNGramsDF(biGramsDfm)
rm(biGramsDfm) # Clear up the Memory

# TriGrams
triGramsDfm <- dfm(tokens_ngrams(qEdaTokens, 3, 0:3))
triGramsDF <- getNGramsDF(triGramsDfm)
rm(triGramsDfm) # Clear up the Memory

# BiTriGrams
biTriGramsDfm <- getDFM(sampleData, 2:3, 0:3)
biTriGramsDF <- getNGramsDF(biTriGramsDfm)
rm(biTriGramsDfm)

# Save the BiGrams and TriGrams for processing later quickly
saveRDS(biGramsDF, file="biGrams.rds")
saveRDS(triGramsDF, file="triGrams.rds")
saveRDS(biTriGramsDF, file="biTriGrams.rds")

# Simplify the Data Frames for faster processing
# Split the Content column into 2 new columns for all biGrams
biGramsDF$CurrentWord <- sapply(word(biGramsDF$Content, 1, sep=fixed("_")), "[", 1)
biGramsDF$NextWord <- sapply(word(biGramsDF$Content, -1, sep=fixed("_")), "[", 1)

# Add the Columns by splitting the contents to the TriGrams
triGramsDF$CurrentWord <- sapply(word(triGramsDF$Content, 1, 2, sep=fixed("_")), "[", 1)
triGramsDF$NextWord <- sapply(word(triGramsDF$Content, -1, sep=fixed("_")), "[", 1)

saveRDS(biGramsDF, file="biGrams-markov.rds")
saveRDS(triGramsDF, file="triGrams-markov.rds")

# Split the Content column into 2 new columns for all bi and triGrams
biTriGramsDF$CurrentWord <- sapply(word(biTriGramsDF$Content, 1, 2, sep=fixed("_")), "[", 1)
biTriGramsDF$NextWord <- sapply(word(biTriGramsDF$Content, -1, sep=fixed("_")), "[", 1)


