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

GSS <- GSS[GSS$YEAR>=1986,]
# GSS <- GSS[GSS$YEAR!=1987,]
GSS = GSS[GSS$YEAR <= 2010,]

table(GSS$YEAR)
table(df$year)

sum(table(GSS$ID_) != table(df$id))
sum(table(GSS$YEAR) != table(df$year))

# natfare: 
# 0 - not applicable
# 3 - Too much

GSS$NATFARE[GSS$NATFARE %in% c(1,2,8,9)] <- 9
GSS$NATFARE[GSS$NATFARE == 3] <- 1
GSS$NATFARE[GSS$NATFARE == 0] <- -999
GSS$NATFARE[GSS$NATFARE == 9] <- 0
table(GSS$NATFARE)


# natfarey: 
# 0 - not applicable
# 3 - Too much
table(GSS$NATFAREY)
GSS$NATFAREY[GSS$NATFAREY %in% c(1,2,8,9)] <- 9
GSS$NATFAREY[GSS$NATFAREY == 3] <- 1
GSS$NATFAREY[GSS$NATFAREY == 0] <- -999
GSS$NATFAREY[GSS$NATFAREY == 9] <- 0
table(GSS$NATFAREY)

# natrace: 
# 0 - not applicable
# 3 - too much

table(GSS$NATRACE)
GSS$NATRACE[GSS$NATRACE %in% c(1,2,8,9)] <- 9
GSS$NATRACE[GSS$NATRACE == 3] <- 1
GSS$NATRACE[GSS$NATRACE == 0] <- -999
GSS$NATRACE[GSS$NATRACE == 9] <- 0
table(GSS$NATRACE)

# natracey: 
# 0 - not applicable
# 3 - too much

table(GSS$NATRACEY)
GSS$NATRACEY[GSS$NATRACEY %in% c(1,2,8,9)] <- 9
GSS$NATRACEY[GSS$NATRACEY == 3] <- 1
GSS$NATRACEY[GSS$NATRACEY == 0] <- -999
GSS$NATRACEY[GSS$NATRACEY == 9] <- 0
table(GSS$NATRACEY)

head(GSS[, c("NATRACE", "NATRACEY", "NATFARE", "NATFAREY")],30)

# sanity check: number of -999 in NATRACEY and NATFAREY should be equal, and should be the same rows
sum(GSS$NATRACEY==-999) == sum(GSS$NATFAREY==-999)
dim(GSS[GSS$NATRACEY==-999 & GSS$NATFAREY!=-999, ])[1] == 0 # should be zero
dim(GSS[GSS$NATRACEY==-999 & GSS$NATFARE==-999, ])

# There is a third variant - Type Z. take these out. 
# dim(GSS[GSS$NATRACEY!=-999 | GSS$NATFARE!=-999, ])
# dim(GSS) - dim(GSS[GSS$NATRACEY==-999 & GSS$NATFARE==-999, ])
# GSS <- GSS[GSS$NATRACEY!=-999 | GSS$NATFARE!=-999, ]
# dim(GSS[GSS$NATRACEY!=-999 & GSS$NATFARE!=-999, ])[1] == 0 # should be zero

# Assign treatment to 
GSS$W = ifelse(GSS$NATRACEY==-999, 0, 1)
table(GSS$W)

# Responses: 
GSS$Y_WELFARE = ifelse(GSS$NATFARE==-999,0,1) + ifelse(GSS$NATFAREY==-999,0,1)
GSS$Y_WELFARE = GSS$NATFARE*(1-GSS$W) + GSS$NATFAREY*GSS$W
GSS$Y_WELFARE[GSS$Y_WELFARE==-999] = 0
table(GSS$Y_WELFARE)
table(df$y)

GSS$Y_RACE = ifelse(GSS$NATRACE==-999,0,1) + ifelse(GSS$NATRACEY==-999,0,1)
GSS$Y_RACE = GSS$NATRACE*(1-GSS$W) + GSS$NATRACEY*GSS$W
GSS$Y_RACE[GSS$Y_RACE==-999] = 0
table(GSS$Y_RACE)
table(df$y)

# merge on year, id and check if the Y's match up

temp = 
  df %>%
  left_join(GSS[, c("YEAR","ID_","Y_WELFARE","Y_RACE")] %>% 
              rename(year = YEAR,id=ID_), 
            by=c('year','id'))

temp[ , c('w',"y",'Y_WELFARE',"Y_RACE")]
temp[temp$y != temp$Y_WELFARE,c("year","id",'w',"y","Y_WELFARE","Y_RACE")]

temp
table(temp$Y_RACE)
table(temp$Y_WELFARE)
table(temp$w, temp$Y_RACE)
table(temp$w, temp$Y_WELFARE)

GSS %>%
  janitor::tabyl(YEAR, W)

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



dim(GSS)[1] - (7836+7681)
names(GSS)
head(GSS)
table(GSS$PARTYID)
table(GSS$POLVIEWS)
table(GSS$AGE)
table(GSS$BALLOT)
table(GSS$RACDIF1)
table(GSS$RACDIF2)
table(GSS$RACDIF3)
table(GSS$RACDIF4)
dim(GSS[GSS$RACDIF1!=0 & GSS$YEAR<=2010,])
GSS %>%
  janitor::tabyl(YEAR, W)

