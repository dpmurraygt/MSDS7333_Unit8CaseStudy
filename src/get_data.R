
# create raw data for runners

library(XML)

extractResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL)
  {
    doc = htmlParse(url)
    
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }
    
    else if (year == 2000 & sex == "female"){
      pres = getNodeSet(doc, "//p") # for bad HTML
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]
      
    }
    
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]
      if(length(els) == 1){
        els = strsplit(txt, "\n")[[1]]
      }
    } 
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }


ubase <- paste("http://www.cherryblossom.org/results/", 1999:2012,"/", sep="")

menURLs = 
  c("cb99m.html", "cb003m.htm", "oof_m.html", "oofm.htm", 
    "CB03-M.HTM", "men.htm", "CB05-M.htm", "men.htm", "men.htm", 
    "men.htm", "09cucb-M.htm", "2010cucb10m-m.htm", 
    "2011cucb10m-m.htm",
    "2012cucb10m-m.htm")

# paste together base url and mens urls
urls = paste(ubase, menURLs, sep = "")

years = 1999:2012
files = paste("Data/mens", years, '.txt', sep="")

# export tables
menTables = mapply(extractResTable, 
                   url = urls, 
                   year = years, 
                   file = files)

# EXPORT WOMENS' DATA
womenURLs = 
  c("cb99f.html", "cb003f.htm", "oof_f.html", "ooff.htm", 
    "CB03-F.HTM", "women.htm", "CB05-F.htm", "women.htm", "women.htm", 
    "women.htm", "09cucb-F.htm", "2010cucb10m-f.htm", 
    "2011cucb10m-f.htm",
    "2012cucb10m-f.htm")

urls = paste(ubase, womenURLs, sep = "")
files = paste("Data/womens", years, '.txt', sep="")

# push women results files
womenTables = mapply(extractResTable, 
                     url = urls, 
                     year = years, 
                     sex = "female", # need this or vector will be null when considering conditional
                     file=files)

