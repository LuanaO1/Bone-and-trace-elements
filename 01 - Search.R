# Install required packages
if(!require(bib2df)) install.packages("bib2df")
if(!require(dplyr)) install.packages("dplyr")
if(!require(PRISMA2020)) install.packages("PRISMA2020")
if(!require(revtools)) install.packages("revtools")

library(bib2df); library(dplyr); library(PRISMA2020)
library(revtools)

# Creating centralizing table ####
cen.tbl <- data.frame(ID = NA, Type = NA, Authors = NA, Year = NA, Title = NA, 
                      Journal = NA, Abstract = NA, Keywords = NA, DB = NA, DOI = NA)
# Importing bibliography
sou <- bib2df("Elsevier Science Direct.bib"); rec <- 1; dbase <- "Elsevier Science Direct"
sou <- bib2df("Elsevier Scopus.bib"); rec <- 1; dbase <- "Elsevier Scopus"
sou <- bib2df("PubMed.bib"); rec <- 1; dbase <- "PubMed"
sou <- bib2df("PubMed Central.bib"); rec <- 1; dbase <- "PubMed Central"; sou$KEYWORDS <- NA
sou <- bib2df("SpringerLink.bib"); rec <- 1; dbase <- "SpringerLink"; sou$KEYWORDS <- NA

while (rec <= nrow(sou)) {
  rw <- sou %>% filter(BIBTEXKEY == sou$BIBTEXKEY[rec])
  id <- rw$BIBTEXKEY
  type <- rw$CATEGORY
  authors <- paste(unlist(rw$AUTHOR), collapse = "; ")
  year <- rw$YEAR
  title <- gsub("[{|}]", "", rw$TITLE)
  journal <- rw$JOURNAL
  abstract <- rw$ABSTRACT
  keywords <- rw$KEYWORDS
  db <- dbase
  doi <- rw$DOI
  # Updating centralizing table
  cen.tbl <- rbind(cen.tbl,
                   c(id, type, authors, year, title, 
                     journal, abstract, keywords, db, doi))
  # Jumping on the next record
  rec <- rec + 1
}

# Deleting first empty record and cleaning environment
if(is.na(cen.tbl[1,])) cen.tbl <- cen.tbl[-1,]
rm(rw, sou, abstract, authors, db, dbase, doi, id, journal, keywords, rec, title, type, year)
# Saving centralizer table
save(cen.tbl, file = "01-Centralizing table.RData"); total.db <- nrow(cen.tbl)

# Creating PRISMA structure ####
PRISMA.tpl <- read.csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020"))
PRISMA.tpl <- PRISMA.tpl %>% 
  mutate(boxtext = case_when(data == "identification" ~ "Identification", T~boxtext))
PRISMA.tpl$n[which(PRISMA.tpl$data == "database_results")] <- total.db

# Drawing PRISMA flow diagram
PRISMA <- PRISMA_flowdiagram(PRISMA_data(PRISMA.tpl),
                             interactive = T, previous = F, other = F)
PRISMA

# Searching for duplicates entries ####


