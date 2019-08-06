library(rorcid)
library(bib2df)
library(scholar)
library(cranlogs)
library(packagefinder)

RenderCV <- function(id="0000-0002-0337-5997", scholar_id = "vpjEkQwAAAAJ") {
  
  publications <- rorcid::orcid_works(id, format="application/json")[[1]][[1]]
  publications <- publications[order(publications$`publication-date.month.value`),]
  publications <- publications[!duplicated(tolower(publications$title.title.value)),]

  employments <- rorcid::orcid_employments(id, format="application/json")[[1]]$`affiliation-group`

  educations <- rorcid::orcid_educations(id, format="application/json")[[1]]$`affiliation-group`

  citations <- rorcid::orcid_citations(id)$citation

  cat(citations, file=file.path(tempdir(), "me.bib"), sep="\n\n")

  bibs <- as.data.frame(bib2df::bib2df(file.path(tempdir(), "me.bib")), stringsAsFactors=FALSE)
  bibs <- bibs[!duplicated(bibs$DOI),]

  scholar <- scholar::get_publications(scholar_id)

  packages <- as.data.frame(packagefinder::exploreFields("Meara", c("Maintainer", "Authors@R", "Author"), "or", "like"))

  total_dl <- function(x) {
    return(sum(cranlogs::cran_downloads(packages=x,from="2005-01-01")$count))
  }
  packages$downloads <- sapply(packages$NAME, total_dl)

  publications$doi <- NA
  for (i in sequence(nrow(publications))) {
   # print(i)
   # print(publications$title.title.value[i])
    best.match <- agrep(publications$title.title.value[i],bibs$TITLE)
    if(length(best.match)>1) {
      best.match <- which.min(adist(publications$title.title.value[i],bibs$TITLE))
    }
    if(length(best.match)==1) {
      publications$doi[i] <- bibs$DOI[best.match]
    }
  }

  publications$scholar_citations <- NA
  for (i in sequence(nrow(publications))) {
   # print(i)
   # print(publications$title.title.value[i])
    best.match <- agrep(tolower(publications$title.title.value[i]),tolower(scholar$title))
    if(length(best.match)>1) {
      best.match <- which.min(adist(tolower(publications$title.title.value[i]),tolower(scholar$title)))
    }
    #print(scholar$title[best.match])
    #print(paste0("Cites ", scholar$cites[best.match]))

    if(length(best.match)==1) {
      publications$scholar_citations[i] <- scholar$cites[best.match]
    }
  }

  current.year <- as.numeric(format(Sys.Date(), "%Y"))
  citation.year <- current.year - 1
  paper.years <- c(citation.year-1, citation.year-2)
  scholar_previous2 <- scholar[which(scholar$year %in% paper.years),]
  scholar_previous2$number <- as.character(scholar_previous2$number)
  scholar_previous2$journal <- as.character(scholar_previous2$journal)
  scholar_previous2 <- scholar_previous2[which(nchar(scholar_previous2$journal)>0),]
  scholar_previous2 <- scholar_previous2[which(nchar(scholar_previous2$number)>0),]
  impact.citations <- 0
  for (i in sequence(nrow(scholar_previous2))) {
    ach <- get_article_cite_history(scholar_id, scholar_previous2$pubid[i])
    impact.citations <- sum(impact.citations, ach$cites[which(ach$year==citation.year)], na.rm=TRUE)
  }
  impact.factor <- impact.citations / nrow(scholar_previous2)

  funding.intro <- "This is all in addition to other **funding my students have gotten** (NSF EAPSI grant, fellowships from NIMBioS and PEER (an NIH-funded program at UTK), Google Summer of Code funding), **funding for workshops or working groups** (from NIMBioS and the Society for Systematic Biologists), and **funding I got before my faculty position** (NESCent postdoctoral fellowship, NSF DDIG, NSF GRF, and various internal grants at UC Davis)."

  activities <- rorcid::orcid_activities(id)[[1]]
  funding <- plyr::rbind.fill(activities$funding$group$`funding-summary`)
  funding <- funding[order(funding$`start-date.year.value`, decreasing = TRUE),]

}
