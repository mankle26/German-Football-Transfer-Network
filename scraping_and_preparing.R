
library(rvest)

#getting all columns of one transfer
getTransferFromTr <- function(tr){
  playerName <- html_node(tr, "td") %>%
                  html_node("div") %>%
                  html_node("a") %>%
                  html_text()
  age <-        html_text(html_node(tr, ".alter-transfer-cell"))
  position <-   html_text(html_node(tr, ".pos-transfer-cell"))
  value <-      html_node(tr, ".mw-transfer-cell") %>%
                  html_text()
  clubName <-   html_node(tr, ".no-border-rechts") %>% 
                  html_node("a")%>% 
                  html_node("img")%>%
                  html_attr("alt")
  transferFee <-html_nodes(tr, "td")[9] %>%
                  html_node("a") %>%
                  html_text()
  transferVector <- c(playerName, age, position, value, clubName, transferFee)
}

#Getting all transfers of each table (in and outbound transfers are in distinct tables)
getTransfersFromTable <- function(table){
  tbody <- html_node(table, "tbody")
  trs <- html_nodes(tbody, "tr")

  #Tables may be empty e.g. Bundesliga season 64/65 Borussia Neunkirchen no outbound transfers
  #Table exists nontheless and must be filtered (by amount of tr children)
  
  amountTrs <- length(trs)
  
  #identify valid trs
  amountValidTrs <- amountTrs
  for(index in 1:amountTrs){
    tr <- trs[index]
    if(length(html_children(tr)) <= 1){
      amountValidTrs <- amountValidTrs - 1
    }
  }
  
  transfersInTable <- vector()
  
  for(index in 1:amountValidTrs){
    tr <- trs[index]
    if(length(html_children(tr)) > 1){ #valid transfer tables with more than one child
      transferInRow <- getTransferFromTr(tr)
      transfersInTable <- rbind(transfersInTable, transferInRow)
    }
  }
  transfersInTable
}

#Getting Club name from each box (which contains both in and outbound transfers)
getclubNameOfBox <- function(box){
  tableHeader <- html_node(box, ".table-header") %>% 
    html_node("a") %>%
    html_node("img") %>% 
    html_attr("alt")
}

#handling each box (Club), containing club name, as well as in and outbound transfers
handleSingleBox <- function(box, season){
  clubName <- getclubNameOfBox(box)

  comingPlayerResponsiveTable <- html_nodes(box, ".responsive-table")[1]
  comingPlayerTable <- html_nodes(comingPlayerResponsiveTable, "table")[1]
  
  leavingPlayerResponsiveTable <- html_nodes(box, ".responsive-table")[2]
  leavingPlayerTable <- html_nodes(leavingPlayerResponsiveTable, "table")[1]
  
  comingPlayers <- getTransfersFromTable(comingPlayerTable)
  leavingPlayers <- getTransfersFromTable(leavingPlayerTable)
  #adding constant transfer columns only for non-empty transfer tables
  if(length(comingPlayers > 0)){
    comingPlayers <- cbind(comingPlayers, season, "Zugang", clubName)
    }else{
      comingPlayers <- NULL}
  
  if(length(leavingPlayers > 0)){
    leavingPlayers <- cbind(leavingPlayers, season, "Abgang", clubName)
    }else{
      leavingPlayers <- NULL
    }
  allTransfersFromOneClub <- rbind(comingPlayers, leavingPlayers)
  
  allTransfersFromOneClub
}

#cleaning dataset
cleandf <- function(allTransfers){
  #assign colnames 
  colnames(allTransfers) <- c("playerName", "age", "position", "value", "clubName", "transferFee", "season", "move", "clubList")
  
  #change to character variables
  allTransfers$playerName <- as.character(allTransfers$playerName)
  allTransfers$clubName <- as.character(allTransfers$clubName)
  allTransfers$clubList <- as.character(allTransfers$clubList)
  
  #create variables source (old club) and target (new club)
  allTransfers$sourceOriginal <- ifelse(allTransfers$move == "Zugang", allTransfers$clubName, allTransfers$clubList)
  allTransfers$targetOriginal <- ifelse(allTransfers$move == "Abgang", allTransfers$clubName, allTransfers$clubList)
  allTransfersclean <- subset(allTransfers, select=-c(clubName, move, clubList))
  
  #standardize position column
  def <- c("Abwehr|RV|Rechter Verteidiger|LV|Linker Verteidiger|IV|Innenverteidiger|Libero")
  mid <- c("Mittelfeld|RM|Rechtes Mittelfeld|LM|Linkes Mittelfeld|DF|Defensives Mittelfeld|ZM|Zentrales Mittelfeld|OM|Offensives Mittelfeld")
  fwd <- c("Sturm|Stürmer|LA|Linksaußen|RA|Rechtsaußen|MS|Mittelstürmer|HS|Hängende Spitze")
  gkp <- c("Torwart|TW")
  allTransfersclean$position <- gsub(def, "def", allTransfersclean$position)
  allTransfersclean$position <- gsub(mid, "mid", allTransfersclean$position)
  allTransfersclean$position <- gsub(fwd, "fwd", allTransfersclean$position)
  allTransfersclean$position <- gsub(gkp, "gkp", allTransfersclean$position)
  
  #standardize value column
  allTransfersclean$value <- as.character(allTransfersclean$value)
  allTransfersclean$value <- gsub(",", "", allTransfersclean$value)
  allTransfersclean$value <- gsub(" Mio. €", "0000", allTransfersclean$value) # replace '_Mio._€' with "0,000"
  allTransfersclean$value <- gsub(" Tsd. €", "000", allTransfersclean$value) # replace '_Tsd._€' with ",000"
  allTransfersclean$value <- as.numeric(allTransfersclean$value)
  
  #create Leihe-Dummy
  allTransfersclean$transferFeeOriginal <- allTransfersclean$transferFee
  allTransfersclean$loanEnd <- ifelse(grepl("Leih-Ende", allTransfersclean$transferFee), 1, 0)
  allTransfersclean$transferFee <- gsub("Leih-Ende[0-9]{2}.[0-9]{2}.[0-9]{4}", "0", allTransfersclean$transferFee)
  allTransfersclean$loanStart <- ifelse(grepl("Leih", allTransfersclean$transferFee), 1, 0)
  
  #standardize transferFee column
  allTransfersclean$transferFee <- gsub(",", "", allTransfersclean$transferFee)
  allTransfersclean$transferFee <- gsub(" Mio. €", "0000", allTransfersclean$transferFee) # replace '_Mio._€' with "0,000"
  allTransfersclean$transferFee <- gsub(" Tsd. €", "000", allTransfersclean$transferFee) # replace '_Tsd._€' with ",000"
  allTransfersclean$loanFee <- ifelse(grepl("Leihgebühr", allTransfersclean$transferFee), 
                                      gsub("Leihgebühr:", "", allTransfersclean$transferFee), NA)
  allTransfersclean$transferFee <- gsub("Leihgebühr:", "", allTransfersclean$transferFee)
  #determine which strings remain
  #allLevelsTransferFee <- levels(as.factor(allTransfersclean$transferFee))
  #allLevelsTransferFee
  allTransfersclean$transferFee <- gsub("Leihe|ablösefrei|-", "0", allTransfersclean$transferFee)
  allTransfersclean$transferFee <- ifelse(grepl("\\?", allTransfersclean$transferFee), NA, allTransfersclean$transferFee)
  
  #collapse different sections of each club
  #determine which ends to delete:
  #allClubs <- as.factor(allTransfersclean$sourceOriginal)
  #levels(allClubs)
  clubEnds <- c(" U17$| U18$| U19$| U20$| U21$| U23$| II$| B$")
  allTransfersclean$source <- gsub(clubEnds, "", allTransfersclean$sourceOriginal)
  allTransfersclean$target <- gsub(clubEnds, "", allTransfersclean$targetOriginal)
  allTransfersclean <- allTransfersclean[, c("playerName", "age", "position", "value", "transferFee", 
                                             "season", "source","target", "loanEnd", "loanStart", "loanFee", 
                                             "sourceOriginal", "targetOriginal", "transferFeeOriginal")]
  rownames(allTransfersclean) <- 1:nrow(allTransfersclean)
  allTransfersclean
}

#Entry in Scraper
main <- function(league, competition, season){
  url <- paste('https://www.transfermarkt.de/', league, '/transfers/wettbewerb/', competition, '/plus/?saison_id=', season, '&s_w=&leihe=1&intern=0&intern=1', sep='')
  mainContainer <-  read_html(url) %>%
                    html_node("body") %>%
                    html_node(".large-8")
  #every container contains three useless boxes, followed by one box for each club 
  amountBoxes <- length(html_children(mainContainer))
  transfers <<- vector()
  if(amountBoxes <= 3){
    print("[INFO] No Transfers in Season");
  }else{
    #Box contains at least one transfer, thus handling every box after the first three
    for(index in 4:amountBoxes){
      allTransfersFromOneClubFinal <- handleSingleBox(html_children(mainContainer)[index], season)
      transfers <- as.data.frame(rbind(transfers, allTransfersFromOneClubFinal))
    }
  }
  transfers
}

#Run scraper by manually specifiying years, competition and league here
#!!!Every year takes approx. 20-60 seconds to scrape!!!
transferlist <- list()
for(i in 1963:2019){
  transfers <- main('1-bundesliga', 'L1', i)
  transferlist[[i]] <- transfers
}

#rbind all years together in one dataframe
allTransfers <- do.call(rbind, transferlist)

#clean dataset
allTransfersclean <- cleandf(allTransfers)

#save as csv
write.csv2(allTransfersClean, "transfers.csv", fileEncoding = "UTF-8")
