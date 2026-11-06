# Build the self-contained page ----------------------------------------------------
#
# Embeds web/data.json into web/template.html and writes web/index.html (and the
# root index.html that GitHub Pages serves), so the
# published page needs no network request for its data.
#
# Run from the project root after R/03_export_web.R:  Rscript R/04_build_page.R

library(here)

template <- paste(readLines(here("web", "template.html"), warn = FALSE), collapse = "\n")
data     <- paste(readLines(here("web", "data.json"), warn = FALSE), collapse = "")
data     <- gsub("</", "<\\/", data, fixed = TRUE)   # can't close the <script> tag early

marker <- "/*BEY_DATA*/"
stopifnot("Template is missing the data marker" = grepl(marker, template, fixed = TRUE))
page <- sub(marker, data, template, fixed = TRUE)

# web/index.html is the working copy; the root index.html is what GitHub Pages serves.
writeLines(page, here("web", "index.html"))
writeLines(page, here("index.html"))
message(sprintf("Wrote web/index.html and index.html (%.0f KB)", file.size(here("index.html")) / 1024))
