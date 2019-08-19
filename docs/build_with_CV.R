library(cv)
cv::CreateMarkdown(outdir=".") # If this stalls, it could be because you're not authenticated to orcid
#cv::FinalCompileCV(outdir=".", open.files=FALSE)
md.files <- list.files(pattern="\\.md")
file.rename(md.files, gsub(".md", ".Rmd",md.files))

rmarkdown::render_site()
source("functions.R")
sitemap <- GenerateSitemap()
cat(sitemap, file="sitemap.xml")
system("git add .; git commit -m'updating site'; git push")
