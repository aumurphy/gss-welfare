library(foreign)
  read.dct <- function(dct, labels.included = "yes") {
      temp <- readLines(dct)
      temp <- temp[grepl("_column", temp)]
      switch(labels.included,
             yes = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
                 classes <- c("numeric", "character", "character", "numeric", "character")
                 N <- 5
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
             },
             no = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
                 classes <- c("numeric", "character", "character", "numeric")
                 N <- 4
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
             })
      temp_metadata <- setNames(lapply(1:N, function(x) {
          out <- gsub(pattern, paste("\\", x, sep = ""), temp)
          out <- gsub("^\\s+|\\s+$", "", out)
          out <- gsub('\"', "", out, fixed = TRUE)
          class(out) <- classes[x] ; out }), NAMES)
      temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
      temp_metadata
  }

  read.dat <- function(dat, metadata_var, labels.included = "yes") {
      read.fwf(dat, widths = metadata_var[["ColWidth"]], col.names = metadata_var[["ColName"]])
  }

getwd()
GSS_metadata <- read.dct("data/gss_download/GSS.dct")
GSS_ascii <- read.dat("data/gss_download/GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii # has 64814 observations

names(GSS) <- tolower(names(GSS))
names(GSS)[names(GSS)=='id_'] <- 'id'

GSS <- GSS[GSS$year>=1986,]
# GSS <- GSS[GSS$year!=1987,]
GSS = GSS[GSS$year <= 2010,]

# sanity check
all(table(GSS$year) == table(df$year))
sum(table(GSS$id) != table(df$id))      # IDs match up
sum(table(GSS$year) != table(df$year))  # years match up

# natfare: 
# 0 - not applicable
# 3 - Too much
table(GSS$natfare)
GSS$natfare[GSS$natfare %in% c(1,2,8,9)] <- 9
GSS$natfare[GSS$natfare == 3] <- 1
GSS$natfare[GSS$natfare == 0] <- -999
GSS$natfare[GSS$natfare == 9] <- 0
table(GSS$natfare)


# natfarey: 
# 0 - not applicable
# 3 - Too much
table(GSS$natfarey)
GSS$natfarey[GSS$natfarey %in% c(1,2,8,9)] <- 9
GSS$natfarey[GSS$natfarey == 3] <- 1
GSS$natfarey[GSS$natfarey == 0] <- -999
GSS$natfarey[GSS$natfarey == 9] <- 0
table(GSS$natfarey)

# natrace: 
# 0 - not applicable
# 3 - too much
table(GSS$natrace)
GSS$natrace[GSS$natrace %in% c(1,2,8,9)] <- 9
GSS$natrace[GSS$natrace == 3] <- 1
GSS$natrace[GSS$natrace == 0] <- -999
GSS$natrace[GSS$natrace == 9] <- 0
table(GSS$natrace)

# natracey: 
# 0 - not applicable
# 3 - too much
table(GSS$natracey)
GSS$natracey[GSS$natracey %in% c(1,2,8,9)] <- 9
GSS$natracey[GSS$natracey == 3] <- 1
GSS$natracey[GSS$natracey == 0] <- -999
GSS$natracey[GSS$natracey == 9] <- 0
table(GSS$natracey)

head(GSS[, c("natrace", "natracey", "natfare", "natfarey")],10)

# sanity check: number of -999 in natracey and natfarey should be equal, and should be the same rows
sum(GSS$natracey==-999) == sum(GSS$natfarey==-999)
dim(GSS[GSS$natracey==-999 & GSS$natfarey!=-999, ])[1] == 0 # should be zero
dim(GSS[GSS$natracey==-999 & GSS$natfare==-999, ])          # These are the question type Z, i think.

# There is a third variant - Type Z. take these out. 
# dim(GSS[GSS$natracey!=-999 | GSS$natfare!=-999, ])
# dim(GSS) - dim(GSS[GSS$natracey==-999 & GSS$natfare==-999, ])
# GSS <- GSS[GSS$natracey!=-999 | GSS$natfare!=-999, ]
# dim(GSS[GSS$natracey!=-999 & GSS$natfare!=-999, ])[1] == 0 # should be zero

# Assign treatment to 
GSS$W = ifelse(GSS$natracey==-999, 0, 1)
table(GSS$W)

# Responses: 
GSS$y_welfare = ifelse(GSS$natfare==-999,0,1) + ifelse(GSS$natfarey==-999,0,1)
GSS$y_welfare = GSS$natfare*(1-GSS$W) + GSS$natfarey*GSS$W
GSS$y_welfare[GSS$y_welfare==-999] = 0
table(GSS$y_welfare)
table(df$Y)

GSS$y_race = ifelse(GSS$natrace==-999,0,1) + ifelse(GSS$natracey==-999,0,1)
GSS$y_race = GSS$natrace*(1-GSS$W) + GSS$natracey*GSS$W
GSS$y_race[GSS$y_race==-999] = 0
table(GSS$y_race)

# merge on year, id and check if the Y's match up

temp = 
  df %>%
  left_join(GSS[, c("year","id","y_welfare","y_race")], 
            by=c('year','id'))

temp %>% 
  select(c(year, id, y_race)) %>%
  readr::write_csv(here::here("data","black_assistance.csv"))

readr::read_csv(here::here("data","black_assistance.csv"))

# temp
# table(temp$y_race)
# table(temp$y_welfare)
# table(temp$W, temp$y_race)
# table(temp$W, temp$y_welfare)
# 
# GSS %>%
#   janitor::tabyl(year, W)

# partyid:  7=other party, 8=don't know, 9=no answer (Scale is 0-6)
# polviews: 0=not applicable, 8=don't know, 9=no answer (Scale is 1-7)
# age: 98=don't know, 99=no answer
# educ: 98=don't know, 99=no answer
# hrs1: -1=not applicable, 98=don't know, 9=no answer
# wrkslf: 1=self-employed, 2=work for someone else, 8=don't know, 9=no answer, 0=not applicable
# occ80: codes, not sure what each represents
# ballot: 0=not applicable, (scale is 1-3)
# income: (Scale is 1-12), 13=refused, 98,99,0=other
# racdif1: 1=Yes, 2=No, 8=Don't know, 9=No answer, 0=Not applicable
# racdif2: 1=Yes, 2=No, 8=Don't know, 9=No answer, 0=Not applicable
# racdif3: 1=Yes, 2=No, 8=Don't know, 9=No answer, 0=Not applicable
# racdif4: 1=Yes, 2=No, 8=Don't know, 9=No answer, 0=Not applicable

# filter out RACDIF not applicables:
GSS = GSS[GSS$RACDIF1!=0 & GSS$YEAR<=2010,]
GSS = GSS[!GSS$RACDIF1 %in% c(8,9),]
dim(GSS)
# filter out partyid=other, don't know, no answer
GSS = GSS[!GSS$PARTYID %in% c(7,8,9),]
dim(GSS)
# filter out polviews=other, don't know, no answer
GSS = GSS[!GSS$PARTYID %in% c(0,8,9),]
dim(GSS)
# filter out age=other, don't know, no answer
GSS = GSS[!GSS$PARTYID %in% c(98,99),]
dim(GSS)
# filter out age=other, don't know, no answer
GSS = GSS[!GSS$BALLOT %in% c(0),]
dim(GSS)
# filter out educ=don't know, no answer
GSS = GSS[!GSS$EDUC %in% c(98,99),]
dim(GSS)

colSums(df==-999)


# This isn't coming out to what the paper had......
