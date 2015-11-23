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
      #This method of grepping dates appears to be more versatile for Lexis-nexis output.
      datetimestamp<-as.Date(
        elem$content[grep(', [0-9]{4} Monday$|, [0-9]{4} Tuesday$|, [0-9]{4} Wednesday$|, [0-9]{4} Thursday$|, [0-9]{4} Friday$|, [0-9]{4} Saturday$|, [0-9]{4} Sunday$', elem$content)]
      , format='%B %d, %Y %A')
      
      origin<-elem$content[grep('DOCUMENTS$', elem$content)+1]
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