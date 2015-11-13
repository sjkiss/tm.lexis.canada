readLexis<-tm::FunctionGenerator(
  function(elem, language, id) {
    function(elem, language, id) {
      
      # Use elem$content, which contains one item of the 'contents'
      # vector you passed above, to create a document
      
      # In particular, you can provide the following information.
      # This code assumes that 'content' contains the plain text of
      # the article body, and that you have filled the other variables
      # with information you extracted from elem$conten
      author<-gsub('BYLINE: ', '', grep('^BYLINE:', elem$content, value=TRUE))
      section<-gsub('SECTION: ', '', grep('^SECTION: ', elem$content, value=TRUE))
      datetimestamp<-as.Date(#open as.Date
        gsub(" Monday| Tuesday| Wednesday| Thursday| Friday| Saturday| Sunday", "", #open gsub
             elem$content[grep("DOCUMENTS$", elem$content)+2 ]),#close gsub 
        format='%B %d, %Y') #close as.Date
      origin<-elem$content[grep('DOCUMENTS?', elem$content)+1]
      ####Note: Just because ProQuest decides to be a little tricky with their encoding of "Full Text", you have to account for both capital T and small t. Thanks ProQuest! 
      content<- elem$content[(grep("^LENGTH:", elem$content)+1):(grep("^LOAD-DATE:", elem$content)-2)]
      #out2[grep("^LENGTH:", out2)+2:grep("^LOAD-DATE:", out2)-1]
      
doc <- PlainTextDocument(x = content
                         ,
                               author = author,                            
                               datetimestamp =datetimestamp,
                               origin = origin
      )
      
      # To set additional information about the article, use
      meta(doc, "Length") <- sum(sapply(gregexpr("\\W+", doc), length) + 1)
      meta(doc, "Section")<-section
      doc
    }
  }
)