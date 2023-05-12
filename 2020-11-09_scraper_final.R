# +++ SCRIPT for SCRAPING URLs of Spiegel-Online +++
# Version: v20201109
# Author: Gunnar Miklis


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ==== 0.0 DOCUMENTATION ====
# EXPLANATION OF CODING

# 1.1   # Input Data
        # Import, use Function: readFile()
        # Read File 'year_edition.csv'
        # Content: Years from 1980 to 2018
        # Content: Each year has about 52-53 editions
        # Variable: years


# 2.1   # Create Pattern
        # Create URL for each edition, with a Pattern
        # (This will only be an assumption)
        # The created List will be validated on errors later in the Parser
        # Step 1 - Build URL: pattern + year + edition + .html
        # Step 2 - Make List of all build URLs
        # Variable: editions

# 2.2   # Make Editions List
        # Build a list structure for each edition

# 2.3   # Create a directory for each year

# 2.5   Parse Editions for Articles, use Function: parseWebpage()
        # Read Webpage Content with the Package rvest: read_html()


# 3.1   # Get URL for each Article
        # Check if Website is available, use Function: tryCatch()
        # Get Errors if webpage is unavailable, 'ERRROR' = TRUE
        # Get Content, if webpage is available, webpage_content
        # Get all <a> attributes with rvest: html_nodes()
        # Get all 'attributes' from <a> with rvest: html_attrs()
        # Get attributes: 'href' and 'title'
        # Check for relevant Webpages using RegExp and stringr: str_match()
        # Clear NAs
        # Variable: articles

# 3.2   Parse Artiles for Text, use Function: parseWebpage()
        # Read Webpage Content with the Package rvest: read_html()

# 3.3   # Get Text of each Article
        # Check if Website is available, use Function: tryCatch()
        # Get Errors if webpage is unavailable, 'ERRROR' = TRUE
        # Get Content, if webpage is available, article_content
        # Get all <p> attributes with rvest: html_nodes()
        # Get text from <p> with rvest: html_text()

# 3.4   Clean Articles
        # Check for relevant Webpages using RegExp and stringr: str_match()
        # Clean Text, Remove unwanted characters with: str_replace_all()
        # Clean Text, Remove unwanted spaces with:  str_squish()

# 3.5   Write Articles to File
        # Create Filename and Directories: dir.create()
        # Export each article-text to .txt File
        # Variable: article

# 3.6   Text to Article
        # Nest each text into article



# 4.1   Make a Article List
        # Put each article in articles_list

# 4.2   Nest Articles
        # Put each articles_list into edition

# 4.3   Make a Edition List
        # Put each edition into editions_list

# 4.4   Nest Editions into Year List aka Main List
        # Put editions_list into main_list



# 5.0   Write Lists to File
        # Write 'error_list' to file
        # Write 'main_list' to file

# 5.1   Save main_list.rdata


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LIST STRUCTURE

# Structure of Website
  # Spiegel Online . de
  # Year      (1980 - 2018) x39   =   39 years
  # Edition   (1 - 52/53)   x52   =   2.036 editions
  # Articles  (~100)        x100  =   203.600 articles

# Anticipated Structure for the Main List
  # Type: List
  # YEAR ->   EDITION ->  ARTICLE   ->  TEXT
  # 1980 ->   [1-53]  ->  [[1-100]] ->  [[['text']]]
  # 1981 ->   [1-52]  ->  [[1-100]] ->  [[['text']]]
  # ...


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# OVERVIEW

# Variable:   years           (List of Years, each with latest Edition, read from File)
# Variable:   editions        (URL List for each Edition, assumed/built from pattern)
# Variable:   webpage_content (Editions Webpage Content)
# Variable:   article_content (Articles Webpage Content)
# Variable:   articles        (URL List for each Article, scraped)
# Variable:   p_text          (Text for each Article, scraped)
# Variable:   articles_list   (List of all articles for each edition)
# Variable:   editions_list   (List of all editions for each year)
# Variable:   main_list       (List of all years, including editions and articles)
# Variable:   error_list      (List with all errors occurred during process)




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ==== 0.1 Directories ====
setwd( 'C:/Users/Gunnar/Desktop/R' )  # Workdirectory
dir.create( 'Articles', showWarnings = FALSE ) # Create Article Folder


# ==== 0.1 Libraries ====
library( rvest )            # Scrape Webpage Content html_nodes(), html_attrs()
library( stringr )          # Use RegExpr, str_match()
library( rlist )            # Use list.append() and list.save command
library( lubridate )        # timer, convert seconds to h:m:s, seconds_to_period()


# ==== 0.2 Variables ====
# Global Variables
main_list <- list()         # Main Array
error_list <- data.frame()  # Table of Errors 

# For Progress Tracking
counter <- 0                # Just to follow the Scraping Progress, via Console Output
tic <- proc.time()[3]       # TicToc Stopwatch, Progress-Timer


# ==== 0.3 Functions ====
# FUNCTION: Parser
parseWebpage <- function( url ) {
  
  out <- tryCatch(
    
    expr = {
      
      # Read Webpage
      webpage <- read_html( url )
    },
    
    error = function( e ){
      
      # Print Error Message in Console
      message( 'Something went wrong here: ', url )
      message( 'Error Type: ', e )
      
      return( 'ERROR' )
    }
  )
  
  suppressWarnings(
    if ( out == 'ERROR' ) {
      return( 'ERROR' )
      
    })
  
  suppressWarnings(  
    if ( out != 'ERROR' ) {
      return ( out )
      
    })
  
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ==== ________________________ ====
# ==== ___ YEARS ___ ====
# ==== 1.1 Input Data ====
years <- read.csv2( 'import_year_edition.csv', sep = '-' )
years_length <- nrow(years)

# ==== ' ' ' TESTING ====
#years_length <- 3
#for ( i in years$Year[years_length] ) {
# ++++++++++++++++++++++


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ==== ___ EDITIONS ___ ====
# ==== 2.1 Create Pattern ====
for ( i in 1:years_length ) {
  
  editions_list <- list()
  
  # Build URL - STEP 1: use pattern
  pattern <- 'https://www.spiegel.de/spiegel/print/index-'
  
  # Build URL - STEP 2: add year to pattern
  pattern_year <- paste( pattern, years$Year[i], sep='' )
  
  # Build URL - STEP 3: add edition and add '.html'
  # Get total number of edition (52 or 53)
  latest_edition <- years$Editions[i]
  
  # ==== ' ' ' TESTING ====  
  #latest_edition <- 3
  #for ( j in 23 ) {
  # ++++++++++++++++++++++
  
  for ( j in 1:latest_edition ) {
    
    # Build URL
    pattern_year_edition <- paste( pattern_year, j, sep='-' )
    pattern_year_edition_html <- paste( pattern_year_edition, '.html', sep='' )

  # ==== 2.2 Make Editions List ====
    # Put everything in a list
    edition <- list()
    edition[1] <- years[[1]][i]; names( edition )[[1]] <- 'Year'
    edition[2] <- j; names( edition )[[2]] <- 'Editions in total'
    edition[3] <- pattern_year_edition_html; names( edition )[[3]] <- 'URL'
    edition[4] <- ''; names( edition )[[4]] <- 'ERROR' 
  
  # ==== 2.3 Create Year Directory ====  
    dir.create( paste( 'Articles/', edition[[1]], sep='' ), showWarnings = FALSE )  

  # ==== 2.4 Progress Tracking ====
    # Just to see the Scraping Progress in Console
    toc <- proc.time()[3] - tic
    toc <- round( toc, digits = 0 )
    toc <- seconds_to_period( toc )
    
    counter <- counter+1; message( paste( '#', counter , ', ', toc, sep=''  ) )
    
  # ==== 2.5 Parse Editions for Article ====
    #Parse
    webpage_content <- parseWebpage( edition[[3]] )
    
    if ( webpage_content == 'ERROR' ) {
      # Write Error
      edition[[4]] <- TRUE
      error_list <- rbind( error_list, c( edition[[1]], edition[[2]], edition[[3]], TRUE ) )
      names( error_list ) <- c( 'Year', 'Edition', 'URL', 'ERROR' )
    }
    if ( webpage_content != 'ERROR' ) {
      # Write Error
      edition[[4]] <- FALSE
      
      # Print Progress in Console
      print( paste( edition[[1]], edition[[2]] ) )
      
      # ==== 2.6 Create Edition Directory ====
      path <- paste( 'Articles/', edition[[1]], '/', edition[[2]], sep='' )
      dir.create( path, showWarnings = FALSE )
      
      
      
      # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      # ==== ___ ARTICLES ___ ====
      # ==== 3.1 Get URL for each Article ====
        articles <- data.frame('','','')
        
        # Get all <a> attributes
        a_data <- html_nodes( webpage_content, 'a' )
        
        # Get all 'attributs' from <a>
        attrs_data <- html_attrs( a_data )
        attrs_data_length <- length( attrs_data ) # Get Length of data-frame
        
        # Define Pattern / Regular Expression
        validate_pattern <- '(https://www.spiegel.de/spiegel/print/d-[0-9]*.html)'
        
        # Validate html attributs in 'a'. Looking for 'href' and 'title'
        for ( k in 1:attrs_data_length ) {
          
          # Get href (URLs) and validate href with validate_pattern
          # Only relevant Webpages will be used
          found <- str_match( attrs_data[[k]]['href'], validate_pattern ); 
          found <- found[,1]
          articles[k,2] <- found
          
          # Get Article Title
          title_data <- attrs_data[[k]]['title']
          articles[k,1] <- title_data
          
          # Leave Column 'Text' empty for now. Will be added later with getText()
          articles[k,3] <- 'NA'
          
        }
        
        # Delete NA
        articles <- na.omit( articles )
        
        # Name columns
        names( articles )[[1]] <- 'Title'
        names( articles )[[2]] <- 'URL'
        names( articles )[[3]] <- 'Text'
          
      # ==== 3.2 Parse Articles for Text ====
        # Create temporary list
        articles_list <- list()
        articles_length <- nrow( articles )
        
        # ==== ' ' ' TESTING ====        
        #articles_length <- 3
        #for ( l in 59:articles_length ) {
        # ++++++++++++++++++++++
        
        for ( l in 1:articles_length ) {
          article_content <- parseWebpage( articles[[2]][[l]] )
          article <- list()
          
          if ( article_content == 'ERROR' ) {
            # Write Error
            article[1] <- articles[[1]][l]; names( article )[[1]] <- 'Title'
            article[2] <- articles[[2]][l]; names( article )[[2]] <- 'URL'
            article[3] <- 'ERROR'; names( article )[[3]] <- 'Text'
            articles_list[[l]] <- article; names( articles_list )[[l]] <- 'ERROR'
            
            error_list <- rbind( error_list, c( edition[[1]], edition[[2]], edition[[3]], TRUE ) )
            names( error_list ) <- c( 'Year', 'Edition', 'URL', 'ERROR' )
          }
          if ( article_content != 'ERROR' ) {
            # ==== 3.3 Get Text of each Article ====
              # Get all <p> attributes
              p_data <- html_nodes( article_content, 'p' )
              
              # Get 'text' from <p>
              p_text <- html_text( p_data )
              
            # ==== 3.4 Clean Articles ====
              # Text 'Cleaning'
              p_text <- paste (p_text, collapse = ' ') # Merge multiple lines to one single line
              p_text <- str_replace_all( p_text, '[^[:alnum:]]', ' ') # Identify the unwanted characters / swap out all non-alphanumeric characters
              p_text <- str_squish( p_text ) # Merge multiple spaces into single space
              
            # ==== 3.5 Write Articles to File ====
              # Build Filename
              article_name <- str_replace_all( articles[l,1], '[^[:alnum:]]', '_') # Cleaning Characters
              article_name <- str_squish( article_name ) # Cleaning Spaces
              article_name <- strtrim(article_name, 30) # Limit String length to 30 Characters
              filename <- paste( edition[[1]], edition[[2]], l, article_name, '.txt', sep='-' ) #Year_Edition_Article-Title
              
              # Export Article to File
              write.table( p_text, paste( path, '/', filename, sep='' ), row.names = F, col.names=F, quote = F )
              
              # ==== 3.6 Text to Article ====
                # Format into correct list format
                article[1] <- articles[[1]][l]; names( article )[[1]] <- 'Title'
                article[2] <- articles[[2]][l]; names( article )[[2]] <- 'URL'
                article[3] <- p_text; names( article )[[3]] <- 'Text'
                
              # ==== 3.7 Progress Tracking ====
                # Print Progress in console
                print( paste( l, '/', articles_length ) )
                
                
                
                
              # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              # ==== ___ MERGE DATA ___ ====
              # ==== 4.1 Text List ====  
                # Put each Article in 'Article List'
                articles_list[[l]] <- article; names( articles_list )[[l]] <- paste( 'Article', l )
            }
          }
          
          # ==== 4.2 Article List ====  
            # Nest 'Article List' to current Edition
            edition <- list.append( edition, articles_list )
            names( edition )[5] <- 'Articles'
      
    }
    
    # ==== 4.3 Edition List ====
      # Put each Edition in 'Edition List'
      # Put each entry into corresponding edition
      editions_list[[j]] <- edition; names( editions_list )[[j]] <- paste( 'Edition', j )
    
  }
  
  # ==== 4.4 Year List ====
    #Put 'Edition List' to current Year
    # Assign each Edition to the corresponding year
    main_list[[i]] <- editions_list; names( main_list )[[i]] <- years$Year[i]
  
}



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ==== ___ EXPORT FILE ___ ====

# write Lists to File
# Write Error List to File: error_list.csv
write.table( error_list, 'export_error_list.csv', sep=';', row.names = F, quote = F )

# Write Main List to File: final_list.csv 
final_list <- unlist( main_list ) # Convert List into characters
final_list <- as.data.frame( final_list ) # Convert List to table
write.table( final_list, 'export_final_list.csv', sep=';', col.names = F, quote = F )


# ==== 5.1 Save Main List ====
list.save(main_list, 'main_list.rdata')

# ==== 5.2 Overall Progress ====
# Just to see the Scraping Progress in Console
toc <- proc.time()[3] - tic
toc <- round( toc, digits = 0 )
toc <- seconds_to_period( toc )
message( paste( 'Overall Progress time:', toc ) )