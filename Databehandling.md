Scraping
========

OBS: grundet samtykkeerklæringen er det kun selve koden, der er
tilgængelig.

Preprossesing
=============

D1 - likes og sponsoreret indhold
---------------------------------

    #omdøb NAs
    d1 <- d1 %>% replace(is.na(.), 0)

    #ændre og standardiser kolonnenavne 
    names(d1) %<>% stringr::str_replace_all("\\s","_") %>% tolower
    names(d1)[4] <- paste("sponsor") 

    #lav sponsor til simpel
    d1$sponsor_s <- NA
    d1$sponsor_s <- ifelse(d1$sponsor == 0, "Nej", "Ja")
    # skriv ingen i stedet for 0 ved ingen sponsor
    d1$sponsor <- gsub("0", "Ingen", d1$sponsor)

    #over tid - fjern komma, konverter til variablen dato
    d1$time_stamp <- gsub("January", "Januar", d1$time_stamp)
    d1$time_stamp <- gsub("February", "Februar", d1$time_stamp)
    d1$time_stamp <- gsub("March", "Marts", d1$time_stamp)
    d1$time_stamp <- gsub("May", "Maj", d1$time_stamp)
    d1$time_stamp <- gsub("June", "Juni", d1$time_stamp)
    d1$time_stamp <- gsub("July", "Juli", d1$time_stamp)
    d1$time_stamp <- gsub("October", "Oktober", d1$time_stamp)

    # ret fejl i timestamp
    d1$time_stamp <- gsub("  ", " ", d1$time_stamp)
    d1$time_stamp <- gsub("April 7 2021", "April 7, 2021", d1$time_stamp) #en hvor der ikke var komma
    d1$time_stamp <- as.Date(d1$time_stamp, format="%B %d, %Y")

    # tilføj kolonne med før og efter corona
    d1$corona <- ifelse(d1$time_stamp < as.Date("2020-03-11"), "Før", "Efter")


    # ret sponsor kolonne der er gået galt
    d1$sponsor <- gsub("tulipdkdkdk", "Tulipdk", d1$sponsor)
    d1$sponsor <- gsub("unlimitbags", "Unlimit", d1$sponsor)
    d1$sponsor <- gsub("stoffstil", "Stof og Stil", d1$sponsor)
    d1$sponsor <- gsub("Sostrenegrene", "Søstrene Grene", d1$sponsor)
    d1$sponsor <- gsub("Ukendt", "Gave", d1$sponsor) # skal måske ikke være der.

    #gør alle sponsorer til små bogstaver
    d1$sponsor <- str_to_title(d1$sponsor)

D2 - kommentarer
----------------

    #ændre kolonnenavne
    names(d2) %<>% stringr::str_replace_all("\\s","_") %>% tolower

    #over tid - ændre til dansk - lav timestamp
    d2$time_stamp <- gsub("May", "Maj", d2$time_stamp)
    d2$time_stamp <- gsub("Oct", "Okt", d2$time_stamp)
    d2$time_stamp <- gsub("  ", " ", d2$time_stamp)
    d2$time_stamp <- as.Date(d2$time_stamp, format = "%B %d, %Y")

    # før og efter corona
    d2$corona <- ifelse(d2$time_stamp < as.Date("2020-03-11"), "Før", "Efter")

    #fjerne linjer ved hashtags
    d2$hashtags <- gsub("\\|", " ", d2$hashtags)


    ########## KOMMENTARER MED OG UDEN EMOJIS ###########

    #fjerne de steder der står varified (personer, der har varified markat)
    d2$kommentarer <- gsub("\\Verified", "", d2$kommentarer)

    # konverter til format der kan arbejdes med
    d2$kommentarer_test <- d2$kommentarer %>% iconv(from = "latin1", to = "ascii", sub = "byte")
    d2$kommentarer_test <- iconv(d2$kommentarer, from = "latin1", to = "ascii", sub = "byte")

    #indsæt æøå igen
    d2$kommentarer_test <- gsub("<c3><a6>", "æ", d2$kommentarer_test)
    d2$kommentarer_test <- gsub("<c3><a4>", "æ", d2$kommentarer_test)
    d2$kommentarer_test <- gsub("<c3><b8>", "ø", d2$kommentarer_test)
    d2$kommentarer_test <- gsub("<c3><98>", "Ø", d2$kommentarer_test)
    d2$kommentarer_test <- gsub("<c3><98>", "Ø", d2$kommentarer_test)
    d2$kommentarer_test <- gsub("<c3><a5>", "å", d2$kommentarer_test)
      
    # kolonne kun med emojis som bytes
    d2$kommentarer_emojis_raw <- str_extract(d2$kommentarer_test, "<.*>")
    d2$kommentarer__emojis_raw1 <- str_extract(d2$kommentarer_emojis_raw, '\\S+$') #fjerner text efter >
    #d2$kommentarer__emojis_raw1 <- stringr::str_replace_all(kommentarer__emojis_raw1, "[:punct:]", "")

    d2$kommentarer_uden_emoji <- d2$kommentarer_test %>% gsub("&amp;", "", .) %>% # remove &
        gsub("@\\w+", "", .) %>% # remove at people
        gsub("[[:digit:]]", "", .) %>% # remove digits
        gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
        gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces 
        gsub("<.*>", "", .) %>% 
        gsub("\\.[[:alpha:]]", "", .)

    # kolonne kun med emojis som figurer
    d2$kommentarer_emojis_print <- stringr::str_replace_all(d2$kommentarer, "[:alnum:]", "")
    d2$kommentarer_emojis_print <- stringr::str_replace_all(d2$kommentarer_emojis_print, "[:punct:]", "")
    d2$kommentarer_emojis_print <- gsub("[[:space:]]", "", d2$kommentarer_emojis_print)

    #andet forsøg på raw emojis
    d2$kommentarer_emojis_raw2 <- iconv(d2$kommentarer_emojis_print, from = "latin1", to = "ascii", sub = "byte")


    ########## ANTAL KOMMENTARER PR POST ###########
    #antal kommentarer pr. post i egen dataframe
    kommentarer <- d2 %>% group_by(source_urls) %>% summarise(
      antal_kommentarer = n() -1) # -1, da den første kommentar altid er Marias egen

    #tilføje det som kolonne i den origniale
    d2 <- merge(d2, kommentarer, by = "source_urls")

    # tilføjes antal kommentarer til d1
    d1 <- merge(d1, kommentarer, by = "source_urls")

    # lav subsets
    subset_maria <- d2 %>% subset(brugernavn=="mariamollerdk")
    subset_folgere <- d2 %>% subset(brugernavn!="mariamollerdk")

    #ekskluder videoer fra datasættet (da disse ikke kan sammenlignes med billeder da de ikke har likes)
    d1 <- d1 %>% filter(likes != "0")

Lister og tabeller
------------------

    # brugere der stille spørgsmål
    liste_spørgsmål <- d2 %>% filter(brugernavn != "mariamollerdk") %>% 
      select(source_urls,kommentarer_uden_emoji) %>%
      filter(grepl("\\?", kommentarer_uden_emoji))
    write.csv(liste_spørgsmål, "/Users/rebeccakjeldsen/Documents/virksomhedsprojekt/Virkomshedsprojekt/liste_spørgsmål.csv")

    # Maria der stiller spørgsmål i sine opslag
    liste_spørgsmål_maria <- d2 %>% filter(brugernavn == "mariamollerdk") %>% 
      select(source_urls, kommentarer_uden_emoji) %>%
      filter(grepl("\\?", kommentarer_uden_emoji))
    write.csv(liste_spørgsmål_maria, "/Users/rebeccakjeldsen/Documents/virksomhedsprojekt/Virkomshedsprojekt/liste_spørgsmål_maria.csv")

    # kommentarer, der bruger hashtag
    liste_hashtag <- d2 %>% 
      select(brugernavn, kommentarer) %>%
      filter(brugernavn != "mariamollerdk") %>% 
      filter(grepl("#", kommentarer))

    # konkurrencer
    konkurrencer <- d2 %>% 
      select(source_urls, brugernavn, kommentarer) %>% 
      filter(grepl("\\konkurrence", kommentarer))
    # --> der er ingen konkurrencer på hendes profil

    #dataframe med emojis
    emojis_df <- d2 %>% drop_na(kommentarer_emojis_raw) %>% 
      select(kommentarer, kommentarer_emojis_print) %>% 
      group_by(kommentarer_emojis_print) %>% 
      arrange(desc(kommentarer_emojis_print))
    write.csv(emojis_df, "/Users/rebeccakjeldsen/Documents/virksomhedsprojekt/Virkomshedsprojekt/emojis_df.csv")


    # antal post og likes sorteret efter spnsoreret indhold med antal kommentarer
    likes_by_sponsor <- d1 %>% group_by(sponsor_s) %>% summarise(
      "antal posts" = n(),
      "likes i gennemsnit" = round(mean(likes), 0),
      "kommentarer i gennemsnit" = round(mean(antal_kommentarer), 1))
    names(likes_by_sponsor)[1] <- paste("sponsor") 

    # liste med sponsoreret indhold
    liste_med_sponsorerne <- d1 %>% group_by(sponsor) %>% summarise(
      "antal posts" = n(),
      "gennemsnit af likes" = round(mean(likes), 0)) 
    # sæt dem i rækkefølge efter antal opslag.
    liste_med_sponsorerne <- liste_med_sponsorerne[order(liste_med_sponsorerne$`antal posts`, decreasing = TRUE),]

    #top 10 likes
    top_10 <- d1 %>% top_n(10, likes) %>% arrange(desc(likes))
    top_10 <- top_10[,c(1,2,3,5,4,7, 6, 8, 9)] # ændre rækkefølge
    top_10 <- top_10[c(1,4,6,5,7,8, 9)] # sorter ubrugelige kolonner fra
    top_10_likes <- top_10 %>% rename( # gør det pænt
        "source url" = source_urls,
        "sponsor" = sponsor_s,
        "sponsor navn" = sponsor,
        "dato" = time_stamp,
        "antal kommentarer" = antal_kommentarer
        )
    #download som csv
    write.csv(top_10_likes, "/Users/rebeccakjeldsen/Documents/virksomhedsprojekt/Virkomshedsprojekt/top_10_likes.csv")


    #top 10 kommentarer
    top_10_kommentarer <- d1 %>% top_n(10, antal_kommentarer) %>% arrange(desc(antal_kommentarer))
    top_10_kommentarer <- top_10_kommentarer[,c(1,9,2,3,5,4,7, 6, 8)] # ændre rækkefølge
    top_10_kommentarer <- top_10_kommentarer[c(1,2,4,6,5,7,8,9)] # sorter ubrugelige kolonner fra
    top_10_kommentarer <- top_10_kommentarer %>% rename( # gør det pænt
        "source url" = source_urls,
        "sponsor" = sponsor_s,
        "sponsor navn" = sponsor,
        "dato" = time_stamp,
        "antal kommentarer" = antal_kommentarer
        )
    #download som csv
    write.csv(top_10_kommentarer, "/Users/rebeccakjeldsen/Documents/virksomhedsprojekt/Virkomshedsprojekt/top_10_kommentarer.csv")

Plot data
---------

    ### likes ###

    # antal likes over tid sorteret efter sponsoreret indhold
    g1 <- ggplot(d1,aes(d1$time_stamp, d1$likes, fill=sponsor_s)) + 
      geom_bar(stat = "summary",fun.y=mean) +
      facet_wrap(~ sponsor_s) +
      ylab("antal likes") +
      xlab("tid") + 
      ggtitle("Antal likes over tid sorteret efter sponsoreret indhold")

    # antal likes over tid - scatterplot med simpel linear regression
    g2 <- ggplot(d1,aes(d1$time_stamp, d1$likes, color=sponsor_s)) + 
      geom_point() +
      geom_smooth(method=lm,aes(group=sponsor_s), se=F) +
      facet_wrap(~ sponsor_s) +
      ylab("antal likes") +
      xlab("tid") 

    # antal likes sorteret efter sponsoreret indhold og corona        ------ brugt i opgaven
    g3 <- ggplot(d1,aes(corona, likes, fill=sponsor_s)) + 
      geom_bar(stat = "summary",fun.y=mean) +
      facet_wrap(~ sponsor_s) +
      geom_errorbar(stat = "summary") +
      ylab("antal likes") +
      xlab("corona") + 
      ggtitle("Antal likes før/efter corona sorteret efter sponsoreret indhold")


    # kommentarer over tid efter sponsor
    g4 <- ggplot(d1, aes(corona, antal_kommentarer, fill=sponsor_s)) + 
      geom_bar(stat = "summary",fun.y=mean) +
      facet_wrap(~ sponsor_s) +
      geom_errorbar(stat = "summary") +
      ylab("antal kommentarer") +
      xlab("corona") + 
      ggtitle("Antal kommentarer før/efter corona sorteret efter sponsoreret indhold")


    # antal kommentarer i gennemsnit før/efter corona                   ----- brugt i opgaven
    g5 <- ggplot(d1, aes(corona, antal_kommentarer, fill=corona)) + 
      geom_bar(stat = "summary",fun.y=mean) +
      geom_errorbar(stat = "summary", fun.data = mean_se) +
      ylab("antal kommentarer") +
      xlab("corona") + 
      ggtitle("Antal kommentarer i gennemsnit før/efter corona")

Wordclouds
----------

Lavet udfra
<a href="https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a" class="uri">https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a</a>

    #Vektor kun med tekst
    # alle
    text <- d2$kommentarer_uden_emoji
    #kun følgere
    text <- subset_folgere$kommentarer_uden_emoji
    # ud fra hashtags
    text <- d2$hashtags

    # Lav tekst-korpus 
    docs <- VCorpus(VectorSource(text))

    #rengør tekst
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)

    # fjern overskydende ord removeWords ikke kunne
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("danish"))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # fjern overskydende ord removeWords ikke kunne
    docs <- tm_map(docs, removeWords, c("kan", "the", "dit", "ogs", "gang", "bare", "synes", "blevet", "lidt", "lavet", "mariamollerdk", "moda", "lige", "ser", "dine", "lave", "virkelig", "ret", "oneline", "ndlavet", "ved", "laver", "finde", "can", "varified", "gerne", "god", "helt", "bruger", "samme", "look", "henne", "make", "brugt", "likelike", "lille", "netop", "see", "igen", "set", "tror", "nok", "str", "kommer", "mon", "hedder", "stadig", "hvilke", "hvilken", "sendt", "get", "alts", "altså", "strikk", "igang", "får", "flere", "sååå", "vores", "evt", "andre", "måde", "nogen", "slags", "går", "gået", "hele", "hver", "tænke", "nej", "komme", "kun", "well", "sted", "såå", "klart"))


    # lav til matrix, dernæst til df
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)


    # lav wordclouds
    set.seed(1234) # for reproducibility 

    #alle
    wc_alle <- wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=300, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(3,1))

    #kun følgere 
    wc_følgere <- wordcloud(words = df$word, freq = df$freq, min.freq = 8, max.words=300, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(4,1))

    # hashtags
    wc_hashtags <- wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=300, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(3,1))

D3 - Spørgeskema databehandling
===============================

Preprocessing
-------------

    # rediger navnene til kortere versioner
    colnames(d3) <- c("time_stamp", "køn", "alder", "Hvorfor_insta", "tid_fulgt", "beskrive_M", "lydhør", "lydhør_uddyb", "se_indhold", "personligt", "hvorfor_følge", "mere_af_hvad", "sponsor_ikke_sponsor", "sponsor_uddyb", "klassisk_influencer", "klassisk_influencer_uddyb", "hvorfor_følge_generelt", "tjene_penge", "ansvar" )


    # fjern mellemrum ved tid fulgt svar
    d3$tid_fulgt %<>% stringr::str_replace_all("\\s","_") %>% tolower

Statistik
---------

    # gennemsnit og median af hvor personligt følgerne synes indholdet er
    mean(d3$personligt)

    ## [1] 3.645161

    median(d3$personligt)

    ## [1] 4

    # fjern dem under et halv år da mange havde fulgt hende i et døgn grundet DR3-videon
    personligt_u_0 <- d3 %>% select(tid_fulgt, personligt) %>% filter(tid_fulgt != "under_et_halvt_år") 
    mean(personligt_u_0$personligt)

    ## [1] 3.714286

    median(personligt_u_0$personligt)

    ## [1] 4

    # gennemsnit og median af hvor lydhør følgerne synes indholdet er
    mean(d3$lydhør)

    ## [1] 4.096774

    median(d3$lydhør)

    ## [1] 4

    # fjern dem under et halv år da mange havde fulgt hende i et døgn grundet DR3-videon
    lydhør_u_0 <- d3 %>% select(tid_fulgt, lydhør) %>% filter(tid_fulgt != "under_et_halvt_år") 
    mean(lydhør_u_0$lydhør)

    ## [1] 4.5

    median(lydhør_u_0$lydhør)

    ## [1] 5

    # hvorfor følge? - se nærmere på dem der gør det af personlige årsager.
    hvorfor_følge_df <- d3 %>% group_by(hvorfor_følge) %>% summarise(antal = n())
    følge_personligt <- d3 %>% filter(grepl("Det personlige", hvorfor_følge))

Wordcloud
---------

    #Vektor kun med tekst
    text <- d3$beskrive_M

    # Lav tekst-korpus 
    docs <- VCorpus(VectorSource(text))

    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("danish"))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # fjern overskydende ord removeWords ikke kunne
    docs <- tm_map(docs, removeWords, c("kan", "the", "dit", "ogs", "gang", "bare", "synes", "blevet", "lidt", "lavet", "mariamollerdk", "moda", "lige", "ser", "dine", "lave", "virkelig", "ret", "oneline", "ndlavet", "ved", "laver", "finde", "can", "varified", "gerne", "god", "helt", "bruger", "samme", "look", "henne", "make", "brugt", "likelike", "lille", "netop", "see", "igen", "set", "tror", "nok", "str", "kommer", "mon", "hedder", "stadig", "hvilke", "hvilken", "sendt", "get", "alts", "altså", "strikk", "igang", "får", "flere", "sååå", "vores", "evt", "andre", "måde", "nogen", "slags", "går", "gået", "hele", "hver", "tænke", "nej", "komme", "kun", "well", "sted", "såå", "klart", "profil", "forskellige", "gør", "mest", "generelt"))

    # lav til matrix, dernæst til df
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)

    set.seed(1234) # for reproducibility 

    # lav wordcloud
    wc_beskriv_m <- wordcloud(words = df$word, freq = df$freq, min.freq = 3, max.words=300, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(3,1))
