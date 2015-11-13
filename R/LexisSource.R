LexisSource<-function(x, encoding = "UTF-8") {
  # Use filename 'x' to read the file and split it
  # into a character vector called 'content' with one item
  # per document
  content<-readLines(x, warn=FALSE)
  ###The following are lines of usually superfluous information that are placed between the what is used to demarcate the beginning of the full text (^LENGTH) and the end of the full text (^LOAD-DATE:)
  content<-   content[grep('^DATELINE:', content, invert=TRUE)]
  #strip out empty white lines
  content<-content[grep('.', content)]
  num <- cumsum(grepl("\f", content))
  content<-split(content, num)
  
  #New code with SimpleSource
  s<-tm::SimpleSource(encoding, length = length(content), content = content,
                      uri = x, reader = readLexis, class = "LexisSource")
  
  s
}