
library(httr);
library(jsonlite);

rm(list = ls());



### seznam tvítů z Hlídače státu

politici <- read.csv('../data/list-politiku.csv', header = T, encoding = 'UTF-8')

token <- '9a4be571d85a4c429a18660965bc73fa'

# url <- 'https://www.hlidacstatu.cz/Api/v1/Datasets/'
# url <- 'https://www.hlidacstatu.cz/Api/v1/Datasets/vyjadreni-politiku'
url <- paste0('https://www.hlidacstatu.cz/Api/v1/DatasetSearch/vyjadreni-politiku?q=osobaid:',
               politici$link)

link <- 'https://www.hlidacstatu.cz/Api/v1/DatasetSearch/vyjadreni-politiku?q=osobaid:marketa-adamova';
raw <- GET(url = link, accept_json(),
           add_headers('Authorization' = 'Token 9a4be571d85a4c429a18660965bc73fa'));
json <- content(raw, 'text');    
dataframe <- fromJSON(json);

df <- dataframe$results;
df <- df[-1:-50,];

for(i in url[1:100]) {
  for(j in 8:8) {
    link <- paste0(i, '&page=', j);
    raw <- GET(url = link, accept_json(),
               add_headers('Authorization' = 'Token 9a4be571d85a4c429a18660965bc73fa'));
    json <- content(raw, 'text');    
    dataframe <- fromJSON(json);
    if(length(dataframe$results) == 0) {break};
    results <- dataframe$results;
    if('pocetSlov' %in% names(results) & !'pocetslov' %in% names(results)) {
      names(results) <- gsub('pocetSlov', 'pocetslov', names(results));
    }
    if(length(results$pocetSlov) > 0 & length(results$pocetslov) > 0) {
      results$pocetslov <- rowSums(data.frame(results$pocetslov, results$pocetSlov), na.rm = T);
    }
    results <- results[, !names(results) == 'pocetSlov'];
    if(!'titulek' %in% names(results)) {
      results$titulek <- NA;
    };
    if(!'politicizminky' %in% names(results)) {
      results$politicizminky <- NA;
    }
    if(!'temata' %in% names(results)) {
      results$temata <- NA;
    }
    df <- rbind(df, results);
    print(paste(i, j));
  }
}

rm(i); rm(j); rm(json); rm(link); rm(raw); rm(dataframe); rm(results)

for(i in 1:nrow(df)) {
  df$politicizminky[i] <- paste(unlist(df$politicizminky[i]), collapse = ', ');
  print(i)
}

df$politicizminky <- unlist(df$politicizminky);

write.table(df, '../data/politici-tvity.csv', row.names = F, fileEncoding = 'UTF-8', sep = '\t');

dflite <- df[df$server == 'Twitter', c(2, 7)]; 
write.csv(dflite, '../data/dflite.csv', row.names = F, fileEncoding = 'UTF-8');

rm(i); rm(token); rm(politici); rm(url)



### počty RT

library(twitteR);
library(lubridate);
library(crayon);

apikey <- '83uEetFRiZ62q9kZCsCVjTmzv';
apisecretkey <- 'tHPfTm7ckE77SAFthRJNB6l9TtxNybIHViMdnwE5K8liaW4rCu';
acctoken <- '15442791-4DztVM5F6bts0OjEiOg6XKD6Lphue5prH1K7bl7Ax';
accsecrettoken <- 'ltYNaQH6pI2TQCPIaWg70SYptiyoFGVWIJ9uty9hmg3AD';

setup_twitter_oauth(apikey, apisecretkey, acctoken, accsecrettoken);

dflite$retweets <- NA;
dflite$favorites <- NA;

ztraceno <- 0;

j <- 1;
reftime <- Sys.time();

while(j <= nrow(dflite)) {
  tryCatch(
    for (i in j:nrow(dflite)) {
      id <- dflite[i, 1];
      status <- showStatus(id);
      retweets <- status$retweetCount;
      favorites <- status$favoriteCount;
      dflite[i, 3] <- retweets;
      dflite[i, 4] <- favorites;
      if(retweets >= 0) {
        reftime <- Sys.time();
      }
      j <- i + 1;
      print(paste0('tvít: ', i));
      cat(green('HIT!!!\n'));
      print(paste0('příští: ', j, ', účet: ', dflite[i, 2], ', rt/fav: ', retweets, '/', favorites, ', url: ', dflite$url[i]));
      print('----------------')
      if(i %% 10000 == 0) {
        filesave <- paste0('../data/dflite', i, '.csv');
        write.csv(dflite, file = filesave, row.names = F, fileEncoding = 'UTF-8');
      }
    }, error = function(e){
      time <- Sys.time();
      if(grepl('Not Found', conditionMessage(e))) {
        print(paste0('tvít: ', j));
        cat(red('NENALEZEN!\n'));
        ztraceno <<- c(ztraceno, j);
        j <<- j + 1;
      }
      if(grepl('replacement has length zero', conditionMessage(e))) {
        print(paste0('tvít: ', j));
        cat(blue('ČEKÁME NA SPOJENÍ...\n'));
      } else {
        print(paste0('tvít: ', j));
        cat(red(conditionMessage(e)));
        ztraceno <<- c(ztraceno, j);
        j <<- j + 1;
      }
      print(paste('akt. čas: ', hour(time), minute(time), substr(second(time), 1, 2), sep = ':'));
      print(paste('pos. hit: ', hour(reftime), minute(reftime), substr(second(reftime), 1, 2), sep = ':'));
      print('----------------')
    });
  Sys.sleep(5);
}

rm(i); rm(j); rm(k); rm(id); rm(favorites); rm(retweets); rm(status); rm(reftime); 
rm(accsecrettoken); rm(acctoken); rm(apikey); rm(apisecretkey); rm(filesave);



### analýza

library(lubridate);

df$datum <- strptime(df$datum, "%Y-%m-%dT%H:%M:%S");
df$rok <- year(df$datum);
df$mesic <- month(df$datum);
df$den <- day(df$datum);
df$hodina <- hour(df$datum);

dftw <- df[df$server == 'Twitter',];

dftw <- cbind(dftw, dflite);

df19 <- dftw[dftw$rok == 2019,];

df19 <- data.frame(df19$osobaid, df19$datum, df19$mesic, df19$den, df19$hodina,
                df19$url, df19$text, df19$pocetslov, df19$politicizminky, df19$retweets, df19$favorites);
colnames(df19) <- c('osobaid', 'datum', 'mesic', 'den', 'hodina',
                    'url', 'text', 'pocetslov', 'politicizminky', 'retweets', 'favorites');

df19$engagement <- df19$retweets + df19$favorites; 

rm(dftw);

write.csv(df19, '../data/df19.csv', row.names = F, fileEncoding = 'UTF-8');

x <- df[df$osobaid == 'milos-zeman',]
x <- x[x$server == 'Twitter',]
y <- dflite[grepl('MZemanOficialni', dflite$url),]

