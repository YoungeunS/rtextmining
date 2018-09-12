#'Rbarima
#' @export

rtextmining <- function(wc.input.text = .Last.value, 
                    launch.browser = False) {
  
  ################################################################################################################
  #ui.R
  ################################################################################################################
  
  ui <- dashboardPage(
    dashboardHeader(title = "TextMining"),
    
    dashboardSidebar(
      useShinyjs(),
      div(
        style = "padding: 15px",
        helpText("1. Upload a text file saved as UTF-8. Max upload size is 700KB on shinyapp.io. It goes up to 15MB when running locally. The data only updates after clicking Upload button.")
      ),
      fileInput("wc", "Upload your text file", multiple = F, accept = "text/plain"), 
      actionButton("wc_upload", "Upload"),
      br(),
      div(
        style = "padding: 15px",
        helpText("2. Adjust up or down to set your min/max word.")
      ),
      sliderInput("wordfreq", "MIN frequency of word", 1, 100, 10),
      sliderInput("maxword", "MAX frequency of word", 1, 10000, 1000),
      br(), 
      div(
        style = "padding: 15px",
        helpText("3. Select and remove stopwords")
      ),
      uiOutput("choose_stopwords"),
      actionButton("wc_stopwords_update", "Upload")
    ), 
    dashboardBody(
      tabsetPanel(
        tabPanel("Word Cloud",
                 icon = shiny::icon("cloud"),
                 br(),
                 h4("What is Wordcloud?"),
                 br(),
                 div(
                   style ="padding: 20px",
                   p("Word clouds or tag clouds are graphical representations of word frequency that give greater prominence to words that appear more frequently in a source text. 
                     The larger the word in the visual the more common the word was in the document(s).
                     This type of visualization can assist evaluators with exploratory textual analysis by identifying words that frequently appear in a set of interviews, documents, or other text. It can also be used for communicating the most salient points or themes in the reporting stage."), 
                   a(href ="http://www.betterevaluation.org/en/evaluation-options/wordcloud", "<BetterEvaluation>")
                   ),
                 hr(),
                 sidebarLayout(
                   sidebarPanel(
                     div(
                       helpText("4. Design and create your own wordcloud."),
                       br(),
                       style = "padding: 15px",
                       h4("Style 1.")
                     ),
                     selectInput("colorbrewer", "Rcolorbrewer", c("Set3", "Set2", "Set1", "Pastel2", "Pastel1", "Paired", "Dark2", "Accent", 
                                                                  "YIOrRd", "YIOrBr", "YIGnBu", "YIGn", "Reds", "RdPu"), selected = "Set2", selectize = TRUE), 
                     selectInput("vfont", "Font", c("serif", "sans serif", " script", "gothic english", "serif symbol"), selected = "serif", selectize = TRUE),
                     br(), br(), br(), br(), br(), br(), br(), br(),
                     hr(),
                     div(
                       style = "padding: 15px",
                       h4("Style 2.")
                     ),
                     selectInput("color", "Colortheme", c("random-light", "random-dark", "white", "aliceblue", "antiquewhite" , "aquamarine", "azure",   
                                                          "beige",  "bisque",   "black", "blanchedalmond",  "blue", "blueviolet", "brown",  "burlywood",  
                                                          "cadetblue", "chartreuse", "chocolate",  "coral", "cornflowerblue",  "cornsilk", "cyan", "darkblue", "darkcyan", 
                                                          "darkgoldenrod", "darkgreen", "darkgrey", "darkkhaki","darkmagenta","darkolivegreen", "darkorange","darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue" ,"darkslategray", "darkslategrey", "darkturquoise", "darkviolet","deeppink", "deepskyblue",  "dimgray", "dimgrey", "dodgerblue",
                                                          "firebrick" ,"floralwhite", "forestgreen",
                                                          "gainsboro", "ghostwhite", "gold", "goldenrod","gray", "green", "greenyellow", "grey","honeydew","hotpink", "indianred", "ivory", 
                                                          "khaki", "lavender", "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral", "lightcyan", "lightgoldenrod", "lightgoldenrodyellow",  "lightgray", "lightgreen" , "lightgrey",  "lightpink" ,  "lightsalmon" ,  "lightseagreen" ,"lightskyblue",  "lightslategray", "lightslategrey", "lightsteelblue",  "lightyellow","limegreen" ,  "linen", 
                                                          "magenta",  "maroon", "mediumaquamarine" , "mediumblue" , "mediumorchid", "mediumseagreen", "mediumslateblue" , "mediumspringgreen",  "mediumturquoise",  "mediumvioletred" ,  "midnightblue" , "mintcream" , "mistyrose" ,  "moccasin",
                                                          "navajowhite" , "navy",  "navyblue" , "oldlace" , "olivedrab", "orchid" , 
                                                          "palegoldenrod" , "palegreen" , "paleturquoise" ,  "palevioletred", "papayawhip", "peachpuff", "peru",  "pink", "plum", "powderblue",  "purple", 
                                                          "red",  "rosybrown",  "royalblue",  "saddlebrown" ,  "salmon",  "sandybrown" , "seagreen",  "seashell" ,  "sienna", "skyblue",  "slateblue", "slategray", "slategrey",  "snow",  "springgreen" , "steelblue" ,
                                                          "tan", "thistle" , "tomato","turquoise" , "violet" , "violetred",  "wheat" , "whitesmoke" , "yellow", "yellowgreen"), selected = "aliceblue", selectize = TRUE),
                     
                     selectInput("bg_color", "BG Colortheme", c("white", "aliceblue", "antiquewhite" , "aquamarine", "azure",   
                                                                "beige",  "bisque",   "black", "blanchedalmond",  "blue", "blueviolet", "brown",  "burlywood",  
                                                                "cadetblue", "chartreuse", "chocolate",  "coral", "cornflowerblue",  "cornsilk", "cyan", "darkblue", "darkcyan", 
                                                                "darkgoldenrod", "darkgreen", "darkgrey", "darkkhaki","darkmagenta","darkolivegreen", "darkorange","darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue" ,"darkslategray", "darkslategrey", "darkturquoise", "darkviolet","deeppink", "deepskyblue",  "dimgray", "dimgrey", "dodgerblue",
                                                                "firebrick" ,"floralwhite", "forestgreen",
                                                                "gainsboro", "ghostwhite", "gold", "goldenrod","gray", "green", "greenyellow", "grey","honeydew","hotpink", "indianred", "ivory", 
                                                                "khaki", "lavender", "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral", "lightcyan", "lightgoldenrod", "lightgoldenrodyellow",  "lightgray", "lightgreen" , "lightgrey",  "lightpink" ,  "lightsalmon" ,  "lightseagreen" ,"lightskyblue",  "lightslategray", "lightslategrey", "lightsteelblue",  "lightyellow","limegreen" ,  "linen", 
                                                                "magenta",  "maroon", "mediumaquamarine" , "mediumblue" , "mediumorchid", "mediumseagreen", "mediumslateblue" , "mediumspringgreen",  "mediumturquoise",  "mediumvioletred" ,  "midnightblue" , "mintcream" , "mistyrose" ,  "moccasin",
                                                                "navajowhite" , "navy",  "navyblue" , "oldlace" , "olivedrab", "orchid" , 
                                                                "palegoldenrod" , "palegreen" , "paleturquoise" ,  "palevioletred", "papayawhip", "peachpuff", "peru",  "pink", "plum", "powderblue",  "purple", 
                                                                "red",  "rosybrown",  "royalblue",  "saddlebrown" ,  "salmon",  "sandybrown" , "seagreen",  "seashell" ,  "sienna", "skyblue",  "slateblue", "slategray", "slategrey",  "snow",  "springgreen" , "steelblue" ,
                                                                "tan", "thistle" , "tomato","turquoise" , "violet" , "violetred",  "wheat" , "whitesmoke" , "yellow", "yellowgreen"), selected = "lightsteelblue", selectize = TRUE),
                     
                     selectInput("font", "Font", c("Agency FB" , "Aharoni", "Algerian" , "Andalus", 
                                                   "Angsana New", "AngsanaUPC",  "Aparajita", "Arabic Typesetting",
                                                   "Arial Black", "Arial", "Arial Narrow", "Arial Rounded MT Bold",
                                                   "Arial Unicode MS", "Baskerville Old Face", "Bauhaus 93", "Bell MT",    
                                                   "Book Antiqua", "Bookman Old Style", "Bookshelf Symbol 7",  "Bradley Hand ITC",   
                                                   "Britannic Bold", "Broadway", "Browallia New",  "BrowalliaUPC",                  
                                                   "Brush Script MT", "Calibri" , "Calibri Light", "Californian FB",     
                                                   "Calisto MT" , "Cambria" , "Candara", "Castellar",       
                                                   "GB1"), selected = "serif", selectize = TRUE), 
                     
                     selectInput("fig", "Figure", c("Twitter", "Facebook", "RStudio"), selected = "Twitter", selectize = TRUE), 
                     fileInput("wc_fig", "Upload your figure", accept = c('image/png', 'image/jpeg')),
                     actionButton("wc_fig_upload", "Upload"),
                     width = 2
                   ),
                   
                   mainPanel (
                     
                     h4("Wordcloud - Style 1"),
                     
                     plotOutput("wc_plot",  width = "650px", height = "450px"),
                     br(),
                     hr(),
                     h4("Wordcloud -  Style 2"),
                     wordcloud2Output("wc_plot_2",  width = "650px", height = "450px"),
                     helpText("Wordcloud - style 2 is NOT available on Chrome. Please use Internet Explore.")
                     
                     
                   )
                 )
                 ),
        
        
        
        tabPanel("Word Cloud Table",
                 icon = icon("bar-chart-o"),
                 br(),
                 h4("Wordcloud - Barplot"),
                 br(),
                 plotOutput("wc_barplot"),
                 br(),
                 dataTableOutput("wc_table"),
                 br(),
                 downloadButton(outputId = "wc_mydownload", label = "Download Data")),
        
        
        tabPanel("Sentiment Analysis", 
                 icon = icon("meh-o"),
                 br(),
                 h4("What is Sentiment Analysis?"),
                 br(),
                 div(
                   style ="padding: 20px",
                   p("Opinion mining (sometimes known as sentiment analysis or emotion AI) refers to the use of natural language processing, 
                     text analysis, computational linguistics, and biometrics to systematically identify, extract, quantify, and study affective 
                     states and subjective information. Sentiment analysis is widely applied to voice of the customer materials such as reviews 
                     and survey responses, online and social media, and healthcare materials for applications that range from marketing to customer service to clinical medicine."),
                   br(),
                   p("Generally speaking, sentiment analysis aims to determine the attitude of a speaker, writer, or other subject with respect 
                     to some topic or the overall contextual polarity or emotional reaction to a document, interaction, or event. 
                     The attitude may be a judgment or evaluation (see appraisal theory), affective state (that is to say, the emotional state of 
                     the author or speaker), or the intended emotional communication (that is to say, the emotional effect intended by the author or interlocutor)."),
                   
                   a(href ="https://en.wikipedia.org/wiki/Sentiment_analysis", "<Wikipedia>")
                   ),
                 br(),
                 hr(),
                 br(),
                 h4("Sentiment Analysis"),
                 br(),
                 plotOutput("sa_plot"),
                 br(),
                 dataTableOutput("sa_table"),
                 br(),
                 downloadButton(outputId = "sa_mydownload", label = "Download Data")),
        
        tabPanel("Link", 
                 br(),
                 h4("Link"),
                 icon = icon("link"),
                 br(), 
                 a(href ="http://searchbusinessanalytics.techtarget.com/definition/text-mining", "[1] text mining (text analytics)"),
                 br(), 
                 a(href ="https://www.tidytextmining.com/index.html", "[2] Text Mining with R"),
                 br(), 
                 a(href ="http://www.betterevaluation.org/en/evaluation-options/wordcloud", "[3] Word Cloud"),
                 br(), 
                 a(href ="https://moderndata.plot.ly/create-colorful-graphs-in-r-with-rcolorbrewer-and-plotly/", "[4] CREATE COLORFUL GRAPHS IN R WITH RCOLORBREWER AND PLOTLY"), 
                 br(), 
                 a(href ="https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/", "[5] How to expand color palette with ggplot and RColorBrewer"), 
                 br(), 
                 a(href ="http://astrostatistics.psu.edu/su07/R/html/grDevices/html/Hershey.html", "[6] Hershey Vector Fonts in R"),
                 br(), 
                 a(href ="https://www.wordclouds.com/", "[7] WordClouds tool"),
                 br(), 
                 a(href ="http://fontawesome.io/icons/", "[8] The Icons"),
                 br(),
                 a(href ="http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf", "[9] Colors in R")
        ),
        tags$head(tags$script(src = "test.js"))
        
        
                 )
        )
      )
  
  ################################################################################################################
  #server.R
  ################################################################################################################
  
  
  options(shiny.maxRequestSize=30*1024^2)
  server <- function(input, output, session) {
    
    # corpus cleaning
    wc_data_corpus <- reactive({
      
      input$wc_upload
      
      isolate({
        
        withProgress({
          setProgress(message = "Processing corpus...")
          wc_file <- input$wc
          if(!is.null(wc_file)){
            wc_text <- readLines(wc_file$datapath, encoding = "UTF-8")
          }
          else
          {
            wc_text <- readLines(wc.input.text, encoding = "UTF-8")
          }
          
          wc_corpus <- Corpus(VectorSource(wc_text))
          
          
          remove_url <- function(x) gsub("http[^[:space:]]*", "", x)
          remove_NumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
          wc_corpus_clean <- tm_map(wc_corpus, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
          wc_corpus_clean <- tm_map(wc_corpus_clean, content_transformer(tolower)) #convert to lowercase
          wc_corpus_clean <- tm_map(wc_corpus_clean, content_transformer(remove_url))
          wc_corpus_clean <- tm_map(wc_corpus_clean, content_transformer(removeNumbers)) #remove numbers
          wc_corpus_clean <- tm_map(wc_corpus_clean, content_transformer(removeWords), stopwords()) #remove stopwords
          wc_corpus_clean <- tm_map(wc_corpus_clean, content_transformer(remove_NumPunct)) 
          wc_corpus_clean <- tm_map(wc_corpus_clean, content_transformer(stripWhitespace)) #remove whitespace
          wc_corpus_clean <- tm_map(wc_corpus_clean, content_transformer(trimws)) 
          wc_corpus_clean <- tm_map(wc_corpus_clean, content_transformer(replace_abbreviation))
          
          
        })
      })
    })
    
    # creating stemDocument
    wc_data_stem <- reactive({
      
      wc_stem <- tm_map(wc_data_corpus(), stemDocument)
      wc_stem
      
    })
    
    # creating dictionary
    wc_copy <- reactive({
      
      wc_copy <- wc_data_corpus()
      wc_copy <- unlist(strsplit(as.character(wc_copy), " "))
      wc_copy <- unique(wc_copy)
      
    })
    
    
    d.complete <- reactive({
      #TermDocumentMatrix
      
      dtm <- TermDocumentMatrix(wc_data_stem())
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v), freq=v)%>%
        filter(freq>(input$wordfreq-1) & (freq<input$maxword-1))
      
      
      d.list <- as.character(d$word)
      
      #stemCompletion
      
      d.stemCompletion <- function(x, dictionary) {
        stemCompletion(x, dictionary = dictionary)
      }
      
      d.complete<-d.stemCompletion(d.list, wc_copy())
      #data.frame
      d.complete <-as.data.frame(d.complete)
      d.complete <-d.complete %>%
        cbind(d$freq) %>%
        rename(word = d.complete, freq= `d$freq`)
      
    })
    
    
    ########Tab1- wordcloud
    
    
    wc_plot_input <- function(){
      
      wordcloud_rep <- repeatable(wordcloud)
      withProgress({
        setProgress(message = "Creating wordcloud...")
        
        
        d.complete <- d.complete()
        d.complete.list <- as.character(d.complete$word)
        
        #Stopwords Update
        
        output$choose_stopwords <- renderUI({
          
          selectInput('wc_stopwords', label =  h5("Stopwords"), selected = wc_stopwords_reactive(), choices = d.complete.list, multiple = TRUE)
          
        })
        
        wc_stopwords_reactive <- reactive({
          
          input$wc_stopwords_update
          
          isolate({
            
            wc<-input$wc_stopwords
            wc
            
          })
          
        })
        
        
        d<-d.complete[!d.complete$word %in% wc_stopwords_reactive(), ]
        
        
        
        
        #Rcolorbrewer
        
        #Select a Color theme
        wc_colorb = "Set2"
        
        if(input$colorbrewer == "Set3"){
          wc_colorb = "Set3"
        }else if(input$colorbrewer == "Set2"){
          wc_colorb = "Set2"
        }else if(input$colorbrewer == "Set1"){
          wc_colorb = "Set1"
        }else if(input$colorbrewer == "Pastel2"){
          wc_colorb = "Pastel2"
        }else if(input$colorbrewer == "Pastel1"){
          wc_colorb = "Pastel1"
        }else if(input$colorbrewer == "Paired"){
          wc_colorb = "Paired"
        }else if(input$colorbrewer == "Dark2"){
          wc_colorb = "Dark2"
        }else if(input$colorbrewer == "Accent"){
          wc_colorb = "Accent"
        }else if(input$colorbrewer == "YIOrRd"){
          wc_colorb = "YIOrRd"
        }else if(input$colorbrewer == "YIOrBr"){
          wc_colorb = "YIOrBr"
        }else if(input$colorbrewer == "YIGnBu"){
          wc_colorb = "YIGnBu"
        }else if(input$colorbrewer == "YIGn"){
          wc_colorb = "YIGn"
        }else if(input$colorbrewer == "Reds"){
          wc_colorb = "Reds"
        }else{
          wc_colorb = "RdPu"
        }
        
        
        #vfornt
        
        #Select a Color theme
        wc_vfont = "serif"
        
        if(input$vfont == "serif"){
          wc_vfont = "serif"
        }else if(input$vfont == "sans serif"){
          wc_vfont= "sans serif"
        }else if(input$vfont == "script"){
          wc_vfont= "script"
        }else if(input$vfont == "serif symbol"){
          wc_vfont= "serif symbol"
        }else{
          wc_vfont= "gothic english"
        }
        
        
        
        par(bg = "grey30")
        wordcloud(d$word, d$freq, colors = brewer.pal(8, wc_colorb), random.order=FALSE, rot.per=0.3, vfont=c(wc_vfont, "plain")) 
        wordcloud
        
      })
      
    }
    
    
    
    output$wc_plot <- renderPlot({
      wc_plot_input()
    })
    
    
    ########Tab1- wordcloud2
    
    wordcloud2_rep <- repeatable(wordcloud2)
    
    wc_plot_2_input <- reactive({
      withProgress({
        setProgress(message = "Creating wordcloud...")
        
        
        # Select figure
        
        fig.sel.reactive <- reactive({
          
          input$wc_fig_upload
          
          fig.sel <-"Twitter.png"
          
          if(input$fig == "Twitter"){
            fig.sel = "Twitter.png"
          }else if(input$fig == "Facebook"){
            fig.sel = "Facebook.png"
          }else{
            fig.sel = "RStudio.png"
          }
          
          
          isolate({
            
            inFile <- input$wc_fig
            
            #Select a figure
            
            if(!is.null(inFile)){
              
              file.copy(inFile$datapath, file.path("Fig/", inFile$name))
              fig.sel <- inFile$name
              
            }else{
              fig.sel <- fig.sel
            }
          })
        })
        
        
        
        figPath <- paste("Fig/", fig.sel.reactive(), sep="")
        
        
        d.complete <- d.complete()
        d.complete.list <- as.character(d.complete$word)
        
        #Stopwords Update
        
        output$choose_stopwords <- renderUI({
          
          selectInput('wc_stopwords', label =  h5("Stopwords"), selected = wc_stopwords_reactive(), choices = d.complete.list, multiple = TRUE)
          
        })
        
        wc_stopwords_reactive <- reactive({
          
          input$wc_stopwords_update
          
          isolate({
            
            wc<-input$wc_stopwords
            wc
            
          })
          
        })
        
        
        d<-d.complete[!d.complete$word %in% wc_stopwords_reactive(), ]
        
        
        
        
        #R color_wordcloud 2
        
        
        #Select a Color theme
        wc_color = "random-light"
        
        if(input$color == "random-light"){
          wc_color = "random-light"
        }else if(input$color == "random-dark"){
          wc_color = "random-dark"
        }else if(input$color == "white"){
          wc_color = "white"
        }else if(input$color == "aliceblue"){
          wc_color = "aliceblue"
        }else if(input$color == "antiquewhite"){
          wc_color = "antiquewhite"  
        }else if(input$color == "aquamarine"){
          wc_color = "aquamarine"
        }else if(input$color == "azure"){
          wc_color = "azure"
        }else if(input$color == "beige"){
          wc_color = "beige"
        }else if(input$color == "bisque"){
          wc_color = "bisque"
        }else if(input$color == "black"){
          wc_color = "black"
        }else if(input$color == "blanchedalmond"){
          wc_color = "blanchedalmond"
        }else if(input$color == "blue"){
          wc_color = "blue" 
        }else if(input$color == "blueviolet"){
          wc_color = "blueviolet"
        }else if(input$color == "brown" ){
          wc_color = "brown"  
        }else if(input$color == "burlywood"){
          wc_color = "burlywood"
        }else if(input$color == "cadetblue" ){
          wc_color = "cadetblue"  
        }else if(input$color == "chartreuse"){
          wc_color = "chartreuse"  
        }else if(input$color ==  "chocolate"){
          wc_color =  "chocolate" 
        }else if(input$color == "coral"){
          wc_color = "coral" 
        }else if(input$color == "cornflowerblue"){
          wc_color = "cornflowerblue" 
        }else if(input$color == "cornsilk"){
          wc_color = "cornsilk"  
        }else if(input$color == "cyan"){
          wc_color = "cyan" 
        }else if(input$color == "darkblue"){
          wc_color = "darkblue" 
        }else if(input$color ==  "darkcyan"){
          wc_color =  "darkcyan" 
        }else if(input$color == "darkgoldenrod"){
          wc_color = "darkgoldenrod"
        }else if(input$color == "darkgray"){
          wc_color = "darkgray" 
        }else if(input$color == "darkkhaki" ){
          wc_color = "darkkhaki"  
        }else if(input$color == "darkmagenta"){
          wc_color = "darkmagenta"   
        }else if(input$color == "darkolivegreen"){
          wc_color = "darkolivegreen"   
        }else if(input$color == "darkorange"){
          wc_color = "darkorange"
        }else if(input$color == "darkorchid"){
          wc_color = "darkorchid"
        }else if(input$color == "darksalmon"){
          wc_color = "darksalmon"    
        }else if(input$color ==  "darkseagreen"){
          wc_color =  "darkseagreen"  
        }else if(input$color ==  "darkslateblue"){
          wc_color ="darkslateblue"
        }else if(input$color == "darkslategray"){
          wc_color = "darkslategray" 
        }else if(input$color == "darkturquoise"){
          wc_color = "darkturquoise" 
        }else if(input$color == "darkviolet" ){
          wc_color = "darkviolet" 
        }else if(input$color == "deeppink"){
          wc_color = "deeppink"   
        }else if(input$color == "deepskyblue" ){
          wc_color = "deepskyblue" 
        }else if(input$color == "firebrick"){
          wc_color = "firebrick"  
        }else if(input$color ==  "dimgrey"){
          wc_color =  "dimgrey"   
        }else if(input$color ==  "floralwhite"){
          wc_color =  "floralwhite"  
        }else if(input$color == "forestgreen"){
          wc_color = "forestgreen" 
        }else if(input$color == "gainsboro"){
          wc_color = "gainsboro"   
        }else if(input$color == "ghostwhite"){
          wc_color = "ghostwhite"  
        }else if(input$color == "gold"){
          wc_color = "gold"  
        }else if(input$color == "goldenrod"){
          wc_color = "goldenrod" 
        }else if(input$color ==  "gray"){
          wc_color =  "gray" 
        }else if(input$color ==  "green"){
          wc_color =  "green" 
        }else if(input$color == "grey"){
          wc_color = "grey" 
        }else if(input$color ==  "honeydew"){
          wc_color =  "honeydew" 
        }else if(input$color == "hotpink" ){
          wc_color = "hotpink"  
        }else if(input$color == "indianred"){
          wc_color = "indianred"  
        }else if(input$color == "ivory"){
          wc_color = "ivory"  
        }else if(input$color == "khaki" ){
          wc_color = "khaki" 
        }else if(input$color == "lavender"){
          wc_color = "lavender"
        }else if(input$color ==  "lawngreen"){
          wc_color =  "lawngreen"  
        }else if(input$color ==  "lemonchiffon"){
          wc_color =  "lemonchiffon"
        }else if(input$color ==  "lightblue" ){
          wc_color ="lightblue" 
        }else if(input$color == "lightcoral"){
          wc_color = "lightcoral" 
        }else if(input$color == "lightcyan" ){
          wc_color = "lightcyan"  
        }else if(input$color == "lightgoldenrod"){
          wc_color = "lightgoldenrod"  
        }else if(input$color == "lightgoldenrodyellow"){
          wc_color = "lightgoldenrodyellow"   
        }else if(input$color == "lightgray"){
          wc_color = "lightgray" 
        }else if(input$color == "lightgrey"){
          wc_color = "lightgrey"
        }else if(input$color == "lightpink"){
          wc_color =  "lightpink"   
        }else if(input$color ==  "lightsalmon"){
          wc_color = "lightsalmon" 
        }else if(input$color == "lightseagreen" ){
          wc_color = "lightseagreen" 
        }else if(input$color == "lightskyblue"){
          wc_color = "lightskyblue"   
        }else if(input$color == "lightslateblue"){
          wc_color = "lightslateblue"  
        }else if(input$color =="lightsteelblue"){
          wc_color = "lightsteelblue"  
        }else if(input$color == "lightyellow"){
          wc_color = "lightyellow"  
        }else if(input$color ==  "limegreen" ){
          wc_color = "limegreen" 
        }else if(input$color ==  "linen"){
          wc_color =  "linen"
        }else if(input$color == "magenta"){
          wc_color = "magenta"
        }else if(input$color == "maroon"){
          wc_color = "maroon" 
        }else if(input$color ==  "mediumaquamarine"){
          wc_color =  "mediumaquamarine"  
        }else if(input$color ==  "mediumblue"){
          wc_color =  "mediumblue"    
        }else if(input$color == "mediumseagreen"){
          wc_color = "mediumseagreen"
        }else if(input$color ==  "mediumslateblue"){
          wc_color =  "mediumslateblue"
        }else if(input$color == "mediumspringgreen"){
          wc_color = "mediumspringgreen" 
        }else if(input$color == "mediumturquoise"){
          wc_color = "mediumturquoise"   
        }else if(input$color == "mediumvioletred"){
          wc_color = "mediumvioletred" 
        }else if(input$color == "midnightblue"){
          wc_color = "midnightblue" 
        }else if(input$color == "mintcream"){
          wc_color = "mintcream"
        }else if(input$color ==  "mistyrose"){
          wc_color =  "mistyrose"
        }else if(input$color ==  "moccasin"){
          wc_color =  "moccasin"  
        }else if(input$color == "navajowhite"){
          wc_color ="navajowhite"       
        }else if(input$color == "navy"){
          wc_color = "navy"  
        }else if(input$color == "navyblue"){
          wc_color = "navyblue"  
        }else if(input$color == "oldlace"){
          wc_color = "oldlace" 
        }else if(input$color == "olivedrab"){
          wc_color = "olivedrab"  
        }else if(input$color == "orange"){
          wc_color = "orange" 
        }else if(input$color == "orangered"){
          wc_color = "orangered"
        }else if(input$color == "orchid"){
          wc_color =  "orchid"   
        }else if(input$color == "palegoldenrod"){
          wc_color = "palegoldenrod"
        }else if(input$color == "palegreen"){
          wc_color = "palegreen"  
        }else if(input$color == "paleturquoise"){
          wc_color = "paleturquoise"  
        }else if(input$color == "palevioletred"){
          wc_color = "palevioletred"   
        }else if(input$color =="papayawhip"){
          wc_color = "papayawhip"  
        }else if(input$color == "peachpuff"){
          wc_color = "peachpuff" 
        }else if(input$color == "peru"){
          wc_color = "peru" 
        }else if(input$color == "pink"){
          wc_color =  "pink"  
        }else if(input$color == "powderblue"){
          wc_color =  "powderblue"
        }else if(input$color == "purple"){
          wc_color = "purple"  
        }else if(input$color == "red"){
          wc_color = "red"  
        }else if(input$color == "royalblue"){
          wc_color = "royalblue" 
        }else if(input$color == "saddlebrown"){
          wc_color = "saddlebrown"  
        }else if(input$color == "seagreen"){
          wc_color = "seagreen"
        }else if(input$color == "seashell"){
          wc_color = "seashell"
        }else if(input$color == "sienna"){
          wc_color = "sienna"
        }else if(input$color == "skyblue"){
          wc_color = "skyblue"
        }else if(input$color == "slateblue"){
          wc_color = "slateblue"
        }else if(input$color == "slategray"){
          wc_color = "slategray" 
        }else if(input$color == "snow"){
          wc_color = "snow"  
        }else if(input$color == "springgreen"){
          wc_color = "springgreen" 
        }else if(input$color == "steelblue"){
          wc_color = "steelblue"
        }else if(input$color ==  "tan"){
          wc_color =  "tan"  
        }else if(input$color == "thistle"){
          wc_color = "thistle"
        }else if(input$color == "tomato"){
          wc_color ="tomato"
        }else if(input$color == "turquoise"){
          wc_color = "turquoise"  
        }else if(input$color == "violet"){
          wc_color = "violet"
        }else if(input$color == "violetred"){
          wc_color = "violetred"   
        }else if(input$color == "wheat"){
          wc_color = "wheat"
        }else if(input$color == "whitesmoke"){
          wc_color = "whitesmoke"
        }else if(input$color == "yellow" ){
          wc_color = "yellow" 
        }else{
          wc_color = "yellowgreen"    
        }
        
        
        
        
        
        
        
        
        #Select a background color
        #wc_bg_color = "brown"
        
        #if(input$bg_color == "brown"){
        #  wc_bg_color = "brown"
        #}else if(input$bg_color == "white"){
        #  wc_bg_color = "white"
        #}else if(input$bg_color == "black"){
        #  wc_bg_color = "black"
        #}else{
        #  wc_bg_color = "grey"
        #}
        
        
        
        #Select a bg_color theme
        wc_bg_color = "random-light"
        
        if(input$bg_color == "random-light"){
          wc_bg_color = "random-light"
        }else if(input$bg_color == "random-dark"){
          wc_bg_color = "random-dark"
        }else if(input$bg_color == "white"){
          wc_bg_color = "white"
        }else if(input$bg_color == "aliceblue"){
          wc_bg_color = "aliceblue"
        }else if(input$bg_color == "antiquewhite"){
          wc_bg_color = "antiquewhite"  
        }else if(input$bg_color == "aquamarine"){
          wc_bg_color = "aquamarine"
        }else if(input$bg_color == "azure"){
          wc_bg_color = "azure"
        }else if(input$bg_color == "beige"){
          wc_bg_color = "beige"
        }else if(input$bg_color == "bisque"){
          wc_bg_color = "bisque"
        }else if(input$bg_color == "black"){
          wc_bg_color = "black"
        }else if(input$bg_color == "blanchedalmond"){
          wc_bg_color = "blanchedalmond"
        }else if(input$bg_color == "blue"){
          wc_bg_color = "blue" 
        }else if(input$bg_color == "blueviolet"){
          wc_bg_color = "blueviolet"
        }else if(input$bg_color == "brown" ){
          wc_bg_color = "brown"  
        }else if(input$bg_color == "burlywood"){
          wc_bg_color = "burlywood"
        }else if(input$bg_color == "cadetblue" ){
          wc_bg_color = "cadetblue"  
        }else if(input$bg_color == "chartreuse"){
          wc_bg_color = "chartreuse"  
        }else if(input$bg_color ==  "chocolate"){
          wc_bg_color =  "chocolate" 
        }else if(input$bg_color == "coral"){
          wc_bg_color = "coral" 
        }else if(input$bg_color == "cornflowerblue"){
          wc_bg_color = "cornflowerblue" 
        }else if(input$bg_color == "cornsilk"){
          wc_bg_color = "cornsilk"  
        }else if(input$bg_color == "cyan"){
          wc_bg_color = "cyan" 
        }else if(input$bg_color == "darkblue"){
          wc_bg_color = "darkblue" 
        }else if(input$bg_color ==  "darkcyan"){
          wc_bg_color =  "darkcyan" 
        }else if(input$bg_color == "darkgoldenrod"){
          wc_bg_color = "darkgoldenrod"
        }else if(input$bg_color == "darkgray"){
          wc_bg_color = "darkgray" 
        }else if(input$bg_color == "darkkhaki" ){
          wc_bg_color = "darkkhaki"  
        }else if(input$bg_color == "darkmagenta"){
          wc_bg_color = "darkmagenta"   
        }else if(input$bg_color == "darkolivegreen"){
          wc_bg_color = "darkolivegreen"   
        }else if(input$bg_color == "darkorange"){
          wc_bg_color = "darkorange"
        }else if(input$bg_color == "darkorchid"){
          wc_bg_color = "darkorchid"
        }else if(input$bg_color == "darksalmon"){
          wc_bg_color = "darksalmon"    
        }else if(input$bg_color ==  "darkseagreen"){
          wc_bg_color =  "darkseagreen"  
        }else if(input$bg_color ==  "darkslateblue"){
          wc_bg_color ="darkslateblue"
        }else if(input$bg_color == "darkslategray"){
          wc_bg_color = "darkslategray" 
        }else if(input$bg_color == "darkturquoise"){
          wc_bg_color = "darkturquoise" 
        }else if(input$bg_color == "darkviolet" ){
          wc_bg_color = "darkviolet" 
        }else if(input$bg_color == "deeppink"){
          wc_bg_color = "deeppink"   
        }else if(input$bg_color == "deepskyblue" ){
          wc_bg_color = "deepskyblue" 
        }else if(input$bg_color == "firebrick"){
          wc_bg_color = "firebrick"  
        }else if(input$bg_color ==  "dimgrey"){
          wc_bg_color =  "dimgrey"   
        }else if(input$bg_color ==  "floralwhite"){
          wc_bg_color =  "floralwhite"  
        }else if(input$bg_color == "forestgreen"){
          wc_bg_color = "forestgreen" 
        }else if(input$bg_color == "gainsboro"){
          wc_bg_color = "gainsboro"   
        }else if(input$bg_color == "ghostwhite"){
          wc_bg_color = "ghostwhite"  
        }else if(input$bg_color == "gold"){
          wc_bg_color = "gold"  
        }else if(input$bg_color == "goldenrod"){
          wc_bg_color = "goldenrod" 
        }else if(input$bg_color ==  "gray"){
          wc_bg_color =  "gray" 
        }else if(input$bg_color ==  "green"){
          wc_bg_color =  "green" 
        }else if(input$bg_color == "grey"){
          wc_bg_color = "grey" 
        }else if(input$bg_color ==  "honeydew"){
          wc_bg_color =  "honeydew" 
        }else if(input$bg_color == "hotpink" ){
          wc_bg_color = "hotpink"  
        }else if(input$bg_color == "indianred"){
          wc_bg_color = "indianred"  
        }else if(input$bg_color == "ivory"){
          wc_bg_color = "ivory"  
        }else if(input$bg_color == "khaki" ){
          wc_bg_color = "khaki" 
        }else if(input$bg_color == "lavender"){
          wc_bg_color = "lavender"
        }else if(input$bg_color ==  "lawngreen"){
          wc_bg_color =  "lawngreen"  
        }else if(input$bg_color ==  "lemonchiffon"){
          wc_bg_color =  "lemonchiffon"
        }else if(input$bg_color ==  "lightblue" ){
          wc_bg_color ="lightblue" 
        }else if(input$bg_color == "lightcoral"){
          wc_bg_color = "lightcoral" 
        }else if(input$bg_color == "lightcyan" ){
          wc_bg_color = "lightcyan"  
        }else if(input$bg_color == "lightgoldenrod"){
          wc_bg_color = "lightgoldenrod"  
        }else if(input$bg_color == "lightgoldenrodyellow"){
          wc_bg_color = "lightgoldenrodyellow"   
        }else if(input$bg_color == "lightgray"){
          wc_bg_color = "lightgray" 
        }else if(input$bg_color == "lightgrey"){
          wc_bg_color = "lightgrey"
        }else if(input$bg_color == "lightpink"){
          wc_bg_color =  "lightpink"   
        }else if(input$bg_color ==  "lightsalmon"){
          wc_bg_color = "lightsalmon" 
        }else if(input$bg_color == "lightseagreen" ){
          wc_bg_color = "lightseagreen" 
        }else if(input$bg_color == "lightskyblue"){
          wc_bg_color = "lightskyblue"   
        }else if(input$bg_color == "lightslateblue"){
          wc_bg_color = "lightslateblue"  
        }else if(input$bg_color =="lightsteelblue"){
          wc_bg_color = "lightsteelblue"  
        }else if(input$bg_color == "lightyellow"){
          wc_bg_color = "lightyellow"  
        }else if(input$bg_color ==  "limegreen" ){
          wc_bg_color = "limegreen" 
        }else if(input$bg_color ==  "linen"){
          wc_bg_color =  "linen"
        }else if(input$bg_color == "magenta"){
          wc_bg_color = "magenta"
        }else if(input$bg_color == "maroon"){
          wc_bg_color = "maroon" 
        }else if(input$bg_color ==  "mediumaquamarine"){
          wc_bg_color =  "mediumaquamarine"  
        }else if(input$bg_color ==  "mediumblue"){
          wc_bg_color =  "mediumblue"    
        }else if(input$bg_color == "mediumseagreen"){
          wc_bg_color = "mediumseagreen"
        }else if(input$bg_color ==  "mediumslateblue"){
          wc_bg_color =  "mediumslateblue"
        }else if(input$bg_color == "mediumspringgreen"){
          wc_bg_color = "mediumspringgreen" 
        }else if(input$bg_color == "mediumturquoise"){
          wc_bg_color = "mediumturquoise"   
        }else if(input$bg_color == "mediumvioletred"){
          wc_bg_color = "mediumvioletred" 
        }else if(input$bg_color == "midnightblue"){
          wc_bg_color = "midnightblue" 
        }else if(input$bg_color == "mintcream"){
          wc_bg_color = "mintcream"
        }else if(input$bg_color ==  "mistyrose"){
          wc_bg_color =  "mistyrose"
        }else if(input$bg_color ==  "moccasin"){
          wc_bg_color =  "moccasin"  
        }else if(input$bg_color == "navajowhite"){
          wc_bg_color ="navajowhite"       
        }else if(input$bg_color == "navy"){
          wc_bg_color = "navy"  
        }else if(input$bg_color == "navyblue"){
          wc_bg_color = "navyblue"  
        }else if(input$bg_color == "oldlace"){
          wc_bg_color = "oldlace" 
        }else if(input$bg_color == "olivedrab"){
          wc_bg_color = "olivedrab"  
        }else if(input$bg_color == "orange"){
          wc_bg_color = "orange" 
        }else if(input$bg_color == "orangered"){
          wc_bg_color = "orangered"
        }else if(input$bg_color == "orchid"){
          wc_bg_color =  "orchid"   
        }else if(input$bg_color == "palegoldenrod"){
          wc_bg_color = "palegoldenrod"
        }else if(input$bg_color == "palegreen"){
          wc_bg_color = "palegreen"  
        }else if(input$bg_color == "paleturquoise"){
          wc_bg_color = "paleturquoise"  
        }else if(input$bg_color == "palevioletred"){
          wc_bg_color = "palevioletred"   
        }else if(input$bg_color =="papayawhip"){
          wc_bg_color = "papayawhip"  
        }else if(input$bg_color == "peachpuff"){
          wc_bg_color = "peachpuff" 
        }else if(input$bg_color == "peru"){
          wc_bg_color = "peru" 
        }else if(input$bg_color == "pink"){
          wc_bg_color =  "pink"  
        }else if(input$bg_color == "powderblue"){
          wc_bg_color =  "powderblue"
        }else if(input$bg_color == "purple"){
          wc_bg_color = "purple"  
        }else if(input$bg_color == "red"){
          wc_bg_color = "red"  
        }else if(input$bg_color == "royalblue"){
          wc_bg_color = "royalblue" 
        }else if(input$bg_color == "saddlebrown"){
          wc_bg_color = "saddlebrown"  
        }else if(input$bg_color == "seagreen"){
          wc_bg_color = "seagreen"
        }else if(input$bg_color == "seashell"){
          wc_bg_color = "seashell"
        }else if(input$bg_color == "sienna"){
          wc_bg_color = "sienna"
        }else if(input$bg_color == "skyblue"){
          wc_bg_color = "skyblue"
        }else if(input$bg_color == "slateblue"){
          wc_bg_color = "slateblue"
        }else if(input$bg_color == "slategray"){
          wc_bg_color = "slategray" 
        }else if(input$bg_color == "snow"){
          wc_bg_color = "snow"  
        }else if(input$bg_color == "springgreen"){
          wc_bg_color = "springgreen" 
        }else if(input$bg_color == "steelblue"){
          wc_bg_color = "steelblue"
        }else if(input$bg_color ==  "tan"){
          wc_bg_color =  "tan"  
        }else if(input$bg_color == "thistle"){
          wc_bg_color = "thistle"
        }else if(input$bg_color == "tomato"){
          wc_bg_color ="tomato"
        }else if(input$bg_color == "turquoise"){
          wc_bg_color = "turquoise"  
        }else if(input$bg_color == "violet"){
          wc_bg_color = "violet"
        }else if(input$bg_color == "violetred"){
          wc_bg_color = "violetred"   
        }else if(input$bg_color == "wheat"){
          wc_bg_color = "wheat"
        }else if(input$bg_color == "whitesmoke"){
          wc_bg_color = "whitesmoke"
        }else if(input$bg_color == "yellow" ){
          wc_bg_color = "yellow" 
        }else{
          wc_bg_color = "yellowgreen"    
        }
        
        
        
        
        
        #fornt
        #selectInput("font", "Wordcloud 1: Font", c("Agency FB" , "Aharoni", "Algerian" , "Andalus", 
        #                                            "Angsana New", "AngsanaUPC",  "Aparajita", "Arabic Typesetting",
        #                                            "Arial Black", "Arial", "Arial Narrow", "Arial Rounded MT Bold",
        #                                            "Arial Unicode MS", "Baskerville Old Face", "Bauhaus 93", "Bell MT",    
        #                                            "Book Antiqua", "Bookman Old Style", "Bookshelf Symbol 7",  "Bradley Hand ITC",   
        #                                            "Britannic Bold", "Broadway", "Browallia New",  "BrowalliaUPC",                  
        #                                            "Brush Script MT", "Calibri" , "Calibri Light", "Californian FB",     
        #                                            "Calisto MT" , "Cambria" , "Candara", "Castellar",       
        #                                            "GB1"), selected = "serif", selectize = TRUE)
        
        #Select a Color theme
        wc_font = "Agency FB"
        
        if(input$font == "Agency FB" ){
          wc_font = "Agency FB" 
        }else if(input$font == "Aharoni"){
          wc_font= "Aharoni"
        }else if(input$font == "Algerian" ){
          wc_font= "Algerian" 
        }else if(input$font == "Andalus"){
          wc_font= "Andalus"
        }else if(input$font == "Angsana New"){
          wc_font= "Angsana New"   
        }else if(input$font == "AngsanaUPC"){
          wc_font= "AngsanaUPC"
        }else if(input$font == "Aharoni"){
          wc_font= "Aharoni"
        }else if(input$font == "Algerian" ){
          wc_font= "Algerian" 
        }else if(input$font ==  "Aparajita" ){
          wc_font= "Aparajita"    
        }else if(input$font == "Arabic Typesetting"){
          wc_font= "Arabic Typesetting"   
        }else if(input$font == "Arial Black"){
          wc_font= "Arial Black"
        }else if(input$font == "Arial"){
          wc_font= "Arial"
        }else if(input$font == "Arial Narrow"){
          wc_font= "Arial Narrow"
        }else if(input$font == "Arial Rounded MT Bold" ){
          wc_font= "Arial Rounded MT Bold"
        }else if(input$font == "Arial Unicode MS"){
          wc_font= "Arial Unicode MS"
        }else if(input$font == "Baskerville Old Face" ){
          wc_font= "Baskerville Old Face"
        }else if(input$font == "Bauhaus 93"){
          wc_font= "Bauhaus 93"
        }else if(input$font == "Bell MT"){
          wc_font= "Bell MT"
        }else if(input$font == "Book Antiqua"){
          wc_font= "Book Antiqua"
        }else if(input$font == "Bookman Old Style" ){
          wc_font= "Bookman Old Style"
        }else if(input$font == "Bookshelf Symbol 7"){
          wc_font= "Bookshelf Symbol 7"
        }else if(input$font == "Bradley Hand ITC"){
          wc_font= "Bradley Hand ITC"
        }else if(input$font == "Britannic Bold"){
          wc_font= "Britannic Bold"
        }else if(input$font == "Broadway" ){
          wc_font= "Broadway"
        }else if(input$font == "Browallia New"){
          wc_font= "Browallia New"
        }else if(input$font == "BrowalliaUPC" ){
          wc_font= "BrowalliaUPC"
        }else if(input$font == "Brush Script MT"){
          wc_font= "Brush Script MT"
        }else if(input$font == "Calibri" ){
          wc_font= "Calibri"
        }else if(input$font == "Calibri Light"){
          wc_font= "Calibri Light"
        }else if(input$font == "Californian FB" ){
          wc_font= "Californian FB"
        }else if(input$font == "Calisto MT"){
          wc_font= "Calisto MT"
        }else if(input$font == "Cambria" ){
          wc_font= "Cambria" 
        }else if(input$font == "Candara"){
          wc_font= "Candara"
        }else if(input$font == "Castellar"){
          wc_font= "Castellar"
        }else{
          wc_font= "GB1"
        }
        
        wc2 <- wordcloud2(d, figPath = figPath, color = wc_color, backgroundColor = wc_bg_color, fontFamily = wc_font)
        wc2
        
        
      })
    })
    
    
    output$wc_plot_2 <- renderWordcloud2({
      wc_plot_2_input()
    })
    
    
    
    
    
    #Tab 2:Wordcloud table
    
    output$wc_barplot <-renderPlot({
      
      withProgress({
        setProgress(message = "Creating word frequency table...")
        
        
        d.complete <- d.complete()
        d.complete.list <- as.character(d.complete$word)
        
        #Stopwords Update
        
        output$choose_stopwords <- renderUI({
          
          selectInput('wc_stopwords', label =  h5("Stopwords"), selected = wc_stopwords_reactive(), choices = d.complete.list, multiple = TRUE)
          
        })
        
        wc_stopwords_reactive <- reactive({
          
          input$wc_stopwords_update
          
          isolate({
            
            wc<-input$wc_stopwords
            wc
            
          })
          
        })
        
        
        d<-d.complete[!d.complete$word %in% wc_stopwords_reactive(), ]
        
        
      
        
        d %>%
          top_n(10) %>%
          ggplot(aes(reorder(word, freq), as.factor(freq), fill=as.factor(freq))) +
          scale_fill_brewer() +
          geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
          labs(y = "Top 10 wordcloud words", x = NULL) +
          coord_flip()
        
      })
      
    })
    
    
    #Table download
    wc_table_data <- reactive({
      
      d.complete <- d.complete()
      d.complete.list <- as.character(d.complete$word)
      
      #Stopwords Update
      
      output$choose_stopwords <- renderUI({
        
        selectInput('wc_stopwords', label =  h5("Stopwords"), selected = wc_stopwords_reactive(), choices = d.complete.list, multiple = TRUE)
        
      })
      
      wc_stopwords_reactive <- reactive({
        
        input$wc_stopwords_update
        
        isolate({
          
          wc<-input$wc_stopwords
          wc
          
        })
        
      })
      
      
      d<-d.complete[!d.complete$word %in% wc_stopwords_reactive(), ]
      
      
      
      
      d
    })
    
    output$wc_table <- renderDataTable({
      wc_table_data()
    }, 
    filter = 'top',
    rownames = FALSE)
    
    output$wc_mydownload <-downloadHandler(
      filename = "data.csv", 
      content = function(file){
        write.csv(wc_table_data(), file)
      })
    
    
    #Tab3: Sentiment Analysis
    
    
    bing_word_counts_rep <- repeatable(bing_word_counts)
    
    output$sa_plot <-renderPlot({
      
      withProgress({
        setProgress(message = "Creating sentiment analysis...")
        
        
        d.complete <- d.complete()
        d.complete.list <- as.character(d.complete$word)
        
        #Stopwords Update
        
        output$choose_stopwords <- renderUI({
          
          selectInput('wc_stopwords', label =  h5("Stopwords"), selected = wc_stopwords_reactive(), choices = d.complete.list, multiple = TRUE)
          
        })
        
        wc_stopwords_reactive <- reactive({
          
          input$wc_stopwords_update
          
          isolate({
            
            wc<-input$wc_stopwords
            wc
            
          })
          
        })
        
        
        d<-d.complete[!d.complete$word %in% wc_stopwords_reactive(), ]
        
        
        
        
        #sentiment Analysis_Bing Words Count
        
        bing_word_counts <- d %>%
          right_join(get_sentiments("bing")) %>% #"afinn" is for numeric scores
          filter(!is.na(freq))
        
        bing_word_counts <- bing_word_counts[which(bing_word_counts$freq > input$wordfreq & bing_word_counts$freq < input$maxword),]
        
        #Rcolorbrewer
        
        #Select a Color theme
        wc_colorb = "Set2"
        
        if(input$colorbrewer == "Set3"){
          wc_colorb = "Set3"
        }else if(input$colorbrewer == "Set2"){
          wc_colorb = "Set2"
        }else if(input$colorbrewer == "Set1"){
          wc_colorb = "Set1"
        }else if(input$colorbrewer == "Pastel2"){
          wc_colorb = "Pastel2"
        }else if(input$colorbrewer == "Pastel1"){
          wc_colorb = "Pastel1"
        }else if(input$colorbrewer == "Paired"){
          wc_colorb = "Paired"
        }else if(input$colorbrewer == "Dark2"){
          wc_colorb = "Dark2"
        }else if(input$colorbrewer == "Accent"){
          wc_colorb = "Accent"
        }else if(input$colorbrewer == "YIOrRd"){
          wc_colorb = "YIOrRd"
        }else if(input$colorbrewer == "YIOrBr"){
          wc_colorb = "YIOrBr"
        }else if(input$colorbrewer == "YIGnBu"){
          wc_colorb = "YIGnBu"
        }else if(input$colorbrewer == "YIGn"){
          wc_colorb = "YIGn"
        }else if(input$colorbrewer == "Reds"){
          wc_colorb = "Reds"
        }else{
          wc_colorb = "RdPu"
        }
        
        
        
        
        bing_word_counts %>%
          group_by(sentiment) %>%
          top_n(10) %>%
          ggplot(aes(reorder(word, freq), freq, fill = sentiment)) +
          scale_fill_brewer(palette = wc_colorb) +
          geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = "Top 10 Nagative & Positive", x = NULL) +
          coord_flip()
        
      })
    })
    
    
    sa_table_data <- reactive({
      
      
      d.complete <- d.complete()
      d.complete.list <- as.character(d.complete$word)
      
      #Stopwords Update
      
      output$choose_stopwords <- renderUI({
        
        selectInput('wc_stopwords', label =  h5("Stopwords"), selected = wc_stopwords_reactive(), choices = d.complete.list, multiple = TRUE)
        
      })
      
      wc_stopwords_reactive <- reactive({
        
        input$wc_stopwords_update
        
        isolate({
          
          wc<-input$wc_stopwords
          wc
          
        })
        
      })
      
      
      d<-d.complete[!d.complete$word %in% wc_stopwords_reactive(), ]
      
      
      #sentiment Analysis_Bing Words Count
      
      bing_word_counts <- d %>%
        right_join(get_sentiments("bing")) %>% #"afinn" is for numeric scores
        filter(!is.na(freq))
      
      bing_word_counts <- bing_word_counts[which(bing_word_counts$freq > input$wordfreq & bing_word_counts$freq < input$maxword),]
      
      bing_word_counts
      
    })
    
    
    output$sa_table <- renderDataTable({
      sa_table_data()
    }, 
    filter = 'top',
    rownames = FALSE)
    
    # Download
    output$sa_mydownload <-downloadHandler(
      filename = "data.csv", 
      content = function(file){
        write.csv(sa_table_data(), file)
      })
    
  } 
  
  runGadget(ui, server, viewer = browserViewer())
  
}
  