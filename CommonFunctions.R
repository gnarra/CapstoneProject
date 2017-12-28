library(stringr)
library(stringi)
library(ggplot2)
library(tm)
library(quanteda)

# options(mc.cores=1)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Read the file and return the lines
readFile <- function(filePath, fileName) {
    fileConn <- file(paste(filePath, fileName, sep=""), open = "rb")
    lines <- readLines(fileConn, encoding="UTF-8", skipNul = TRUE)
    close(fileConn)
    
    return (lines)
}

# Get the TM Corpus
getTMCorpus <- function(data) {
    # Create a TM Corpus Object    
    tmCorpus <- VCorpus(VectorSource(data))
    
    # Clean up the TM Corpus Data
    # http://stat.ethz.ch/R-manual/R-patched/library/base/html/regex.html
    toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    tmCorpus <- tm_map(tmCorpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
    tmCorpus <- tm_map(tmCorpus, toSpace, "@[^\\s]+")
    tmCorpus <- tm_map(tmCorpus, toSpace, "[^[:graph:]]")
    tmCorpus <- tm_map(tmCorpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
    tmCorpus <- tm_map(tmCorpus, removePunctuation)
    tmCorpus <- tm_map(tmCorpus, removeNumbers)
    tmCorpus <- tm_map(tmCorpus, removeWords, stopwords("english"))
    tmCorpus <- tm_map(tmCorpus, stripWhitespace)
    tmCorpus <- tm_map(tmCorpus, content_transformer(tolower))
    # tmCorpus <- tm_map(tmCorpus, tolower)
    # tmCorpus <- tm_map(tmCorpus, PlainTextDocument)
    
    return (tmCorpus)
}

# Tokenize the Data using the quanteda Package
tokenizeData <- function(tmCorpus) {
    # Convert the tm corpus to quanteda corpus because of DFM problems in newer versions of tm
    quantEdaCorpus <- corpus(tmCorpus)
    
    # Clean the Corpus Tokens
    tokensCorpus <- tokens(quantEdaCorpus, what = "word", 
                           remove_numbers = TRUE, remove_punct = TRUE, 
                           remove_symbols = TRUE, remove_twitter = TRUE,
                           remove_hyphens = TRUE, remove_url = TRUE)
    
    # removing stopwords before constructing ngrams
    tokensCorpus <- tokens_remove(tokensCorpus, stopwords("english"))
    
    # removing swear words
    tokensCorpus <- tokens_select(tokensCorpus, curseWords, selection = "remove", padding = FALSE)
    
    return (tokensCorpus)
}

# convert to a DFM to ngrams data.frame with count decreasing
getNGramsDF <- function(dfm) {
    ngramsDF <- data.frame(Content = featnames(dfm), Frequency = colSums(dfm), 
                              row.names = NULL, stringsAsFactors = FALSE)
    ngramsDF <- ngramsDF[order(ngramsDF$Frequency, decreasing=TRUE), ]
    
    # Drop all the Rows that only have 1 match
    ngramsDF <- ngramsDF[(ngramsDF$Frequency > 1),]
    
    return (ngramsDF)
}

# Get the N-Grams Search String from the input string
getSearchStrFromTokens <- function(tokens) {
    # Check the Length of the input
    if(is.null(tokens)) {
        return (NULL)
    }
    
    wordsLength <- length(tokens)
    if(wordsLength == 0) {
        return ("")
    }
    
    backwardsLen <- 1
    if(wordsLength > 2) {
        backwardsLen <- (wordsLength - 1)
    }
    # message (paste("Number of Words = ", wordsLength, sep=""))
    # message (paste("Grep Number of Words = ", backwardsLen, sep=""))
    
    # Create the Search Str
    grepStr <- ""
    for (i in wordsLength:backwardsLen) {
        word <- tokens[i]
        grepStr <- paste(word, "_", grepStr, sep="")
    }
    
    # Strip the last "_" added due to the for loop
    grepStr <- substr(grepStr, 1, (nchar(grepStr)-1))
    
    return (grepStr)
}

# Get the N-Grams Search String from the input string
getSearchStr <- function(input) {
    # Check the Length of the input
    if(is.null(input)) {
        return (NULL)
    }
    
    # Tokenize the Data to drop the stopwords, punctuation etc
    tmCorpus <- getTMCorpus(input)
    corpusSearch <- tokenizeData(tmCorpus)
    str <- getSearchStrFromTokens(corpusSearch$text1)
    
    return (str)
}

# Get all possible Next Words from the search string
predictWords <- function(searchStr, triGrams, biGrams) {
    # Get the Number of Words in the SearchStr
    searchWords <- unlist(strsplit(searchStr, "_"))
    wordsLength <- length(searchWords)
    if(wordsLength == 0) {
        return ()
    }
    
    # Check the Bigrams Only if the words length is 1
    if(wordsLength == 1) {
        resultsDF <- biGrams[biGrams$CurrentWord == searchStr, ]
        message (paste("Search Str = ", searchStr, ", Bigram results count is ", nrow(resultsDF), ".",  sep=""))
    } else if (wordsLength >= 2) {
        # Check the Trigrams First
        resultsDF <- triGrams[grep(paste("^", searchStr, sep=""), triGrams$Content), ]
    
        # If we get zero Results in Trigrams then check the biGrams
        if(nrow(resultsDF) == 0 ) {
            endWord <- word(searchStr, -1, sep=fixed("_"))
            resultsDF <- biGrams[biGrams$CurrentWord == endWord, ]
            message (paste("Search Str = ", searchStr, ", Trigram results count = 0, Bigram results counts for '", endWord, "' is ", nrow(resultsDF), ".",  sep=""))
        } else {
            # Combine the Results now
            # resultsDF <- rbind(triGramResultsDF, biGramResultsDF) 
            message (paste("Search Str = ", searchStr, ", Trigram results count is ", nrow(resultsDF), ".",  sep=""))
        }
    }
    
    # Return the Results
    return (resultsDF)
}

# getFreq <- function(tdm) {
#     freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
#     return(data.frame(word = names(freq), freq = freq))
# }
# 
# makePlot <- function(data, label, count) {
#     ggplot(data[1:count,], aes(reorder(word, -freq), freq)) +
#         labs(x = label, y = "Frequency") +
#         theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
#         geom_bar(stat = "identity", fill = I("blue"))
# }

# Cursewords to Remove from Corpus (This list is from www.noswearing.com)
curseWords <- c("anus", "arse", "arsehole", "ass", "ass-hat", "ass-jabber", "ass-pirate", "assbag", "assbandit", 
                "assbanger", "assbite", "assclown", "asscock", "asscracker", "asses", "assface", "assfuck", "assfucker", 
                "assgoblin", "asshat", "asshead", "asshole", "asshopper", "assjacker", "asslick", "asslicker", "assmonkey", 
                "assmunch", "assmuncher", "assnigger", "asspirate", "assshit", "assshole", "asssucker", "asswad", 
                "asswipe", "axwound", "bampot", "bastard", "beaner", "bitch", "bitchass", "bitches", "bitchtits", 
                "bitchy", "blow job", "blowjob", "bollocks", "bollox", "boner", "brotherfucker", "bullshit", 
                "bumblefuck", "butt plug", "butt-pirate", "buttfucka", "buttfucker", "camel toe", "carpetmuncher", 
                "chesticle", "chinc", "chink", "choad", "chode", "clit", "clitface", "clitfuck", "clusterfuck", "cock", 
                "cockass", "cockbite", "cockburger", "cockface", "cockfucker", "cockhead", "cockjockey", "cockknoker", 
                "cockmaster", "cockmongler", "cockmongruel", "cockmonkey", "cockmuncher", "cocknose", "cocknugget", 
                "cockshit", "cocksmith", "cocksmoke", "cocksmoker", "cocksniffer", "cocksucker", "cockwaffle", 
                "coochie", "coochy", "coon", "cooter", "cracker", "cum", "cumbubble", "cumdumpster", "cumguzzler", 
                "cumjockey", "cumslut", "cumtart", "cunnie", "cunnilingus", "cunt", "cuntass", "cuntface", "cunthole", 
                "cuntlicker", "cuntrag", "cuntslut", "dago", "damn", "deggo", "dick", "dick-sneeze", "dickbag", 
                "dickbeaters", "dickface", "dickfuck", "dickfucker", "dickhead", "dickhole", "dickjuice", "dickmilk", 
                "dickmonger", "dicks", "dickslap", "dicksucker", "dicksucking", "dicktickler", "dickwad", "dickweasel", 
                "dickweed", "dickwod", "dike", "dildo", "dipshit", "doochbag", "dookie", "douche", "douche-fag", 
                "douchebag", "douchewaffle", "dumass", "dumb ass", "dumbass", "dumbfuck", "dumbshit", "dumshit", "dyke",
                "fag", "fagbag", "fagfucker", "faggit", "faggot", "faggotcock", "fagtard", "fatass", "fellatio", "feltch", 
                "flamer", "fuck", "fuckass", "fuckbag", "fuckboy", "fuckbrain", "fuckbutt", "fuckbutter", "fucked", 
                "fucker", "fuckersucker", "fuckface", "fuckhead", "fuckhole", "fuckin", "fucking", "fucknut", "fucknutt", 
                "fuckoff", "fucks", "fuckstick", "fucktard", "fucktart", "fuckup", "fuckwad", "fuckwit", "fuckwitt", 
                "fudgepacker", "gay", "gayass", "gaybob", "gaydo", "gayfuck", "gayfuckist", "gaylord", "gaytard", 
                "gaywad", "goddamn", "goddamnit", "gooch", "gook", "gringo", "guido", "handjob", "hard on", "heeb", 
                "hell", "ho", "hoe", "homo", "homodumbshit", "honkey", "humping", "jackass", "jagoff", "jap", 
                "jerk off", "jerkass", "jigaboo", "jizz", "jungle bunny", "junglebunny", "kike", "kooch", "kootch", 
                "kraut", "kunt", "kyke", "lameass", "lardass", "lesbian", "lesbo", "lezzie", "mcfagget", "mick", 
                "minge", "mothafucka", "mothafuckin", "motherfucker", "motherfucking", "muff", "muffdiver", "munging",
                "negro", "nigaboo", "nigga", "nigger", "niggers", "niglet", "nut sack", "nutsack", "paki", "panooch", 
                "pecker", "peckerhead", "penis", "penisbanger", "penisfucker", "penispuffer", "piss", "pissed", 
                "pissed off", "pissflaps", "polesmoker", "pollock", "poon", "poonani", "poonany", "poontang", 
                "porch monkey", "porchmonkey", "prick", "punanny", "punta", "pussies", "pussy", "pussylicking", "puto", 
                "queef", "queer", "queerbait", "queerhole", "renob", "rimjob", "ruski", "sand nigger", "sandnigger", 
                "schlong", "scrote", "shit", "shitass", "shitbag", "shitbagger", "shitbrains", "shitbreath", 
                "shitcanned", "shitcunt", "shitdick", "shitface", "shitfaced", "shithead", "shithole", "shithouse", 
                "shitspitter", "shitstain", "shitter", "shittiest", "shitting", "shitty", "shiz", "shiznit", "skank", 
                "skeet", "skullfuck", "slut", "slutbag", "smeg", "snatch", "spic", "spick", "splooge", "spook", "suckass",
                "tard", "testicle", "thundercunt", "tit", "titfuck", "tits", "tittyfuck", "twat", "twatlips", "twats", 
                "twatwaffle", "unclefucker", "va-j-j", "vag", "vagina", "vajayjay", "vjayjay", "wank", "wankjob", 
                "wetback", "whore", "whorebag", "whoreface", "wop")
