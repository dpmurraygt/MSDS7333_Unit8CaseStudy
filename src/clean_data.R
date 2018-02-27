# Nolan and Temple Lang Ch. 2

# find column locations based on ==== ==== spacer row
findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]] # find a group of column locs, this gets space BETWEEN cols
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ") # if last position is not " "
    return( c(0, spaceLocs, rowLength + 1)) # return an array of 0, spacer locations and max position + 1
  else return(c(0, spaceLocs)) # else return array with 0 and the spacer locations
}

# create FUN to extract certain columns only, all of this is reliant on the spacer row (yikes)
# takes header row, column names, locations of blanks in separator row

selectCols = function(shortColNames, headerRow, searchLocs) {
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    
    startPos = regexpr(shortName, headerRow)[[1]]
    
    if (startPos == -1) return( c(NA, NA) )
    
    index = sum(startPos >= searchLocs) # this is dependent upon ORDER of cols
    c(searchLocs[index] + 1, searchLocs[index + 1]) # get rid of the -1 in the second term
  }, 
  
  headerRow = headerRow, searchLocs = searchLocs )
}

# roll all this mess up into a function for modularity
extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data 
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    
    # Obtain the starting and ending positions of variables   
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    return(Values)
  }


# Convert Time
convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}

# adjust for footnotes in run time and blanks
createDF = function(Res, year, sex) 
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  age = gsub("X{2}\\s{1}?|\\s{3}?","0  ", Res[,'ag']) # authors' methods did not catch all blanks "   " for men 
                                                      # handle women's XX's and blanks
  Res[, 'ag'] = age
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}


## READ IN AND TRANSFORM TO MATRIX
mfilenames = paste("Data/mens", 1999:2012, ".txt", sep = "")
menFiles = lapply(mfilenames, readLines)
names(menFiles) = 1999:2012

menResMat = lapply(menFiles, extractVariables)

# now for women's files
wfilenames = paste("Data/womens", 1999:2012, ".txt", sep = "")
womenFiles = lapply(wfilenames, readLines)
names(womenFiles) = 1999:2012

# errors out because of missing data in 2001 file
# fix women's issu... nope not even going to type that. 
womenFiles[[3]] = append(womenFiles[[3]], menFiles[[3]][4:5], after=3)
womenResMat = lapply(womenFiles, extractVariables)


# fix 2006 formatting issues
separatorIdx = grep("^===", menFiles[["2006"]])
separatorRow = menFiles[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
menFiles[['2006']][separatorIdx] = separatorRowX

menResMat = sapply(menFiles, extractVariables)
menDF = mapply(createDF, menResMat, year = 1999:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)



# clean 2006 file with bad separator for women as well
separatorIdx = grep("^===", womenFiles[["2006"]])
separatorRow = womenFiles[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
womenFiles[['2006']][separatorIdx] = separatorRowX

womenResMat = sapply(womenFiles, extractVariables)

womenDF = mapply(createDF, womenResMat, year = 1999:2012,
                 sex = rep("W", 14), SIMPLIFY = FALSE)


cbMen = do.call(rbind, menDF)
na.fail(cbMen) # check

cbWomen = do.call(rbind, womenDF)
na.fail(cbWomen) #check

save(cbMen, file = "Data/cbMen.rda")
save(cbWomen, file = "Data/cbWomen.rda")
