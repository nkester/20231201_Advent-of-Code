library(readr)
library(stringr)
library(tibble)

# Part 1 ----
schematic <- readr::read_lines(file = "../adventOfCodeDay3.txt")

# Symbols to find. All need to be escaped 
symbols <- r'{[\,\:\;\?\!\/\*\@\#\-\_\'\"\[\]\{\}\(\)\|\`\=\+\^\~\<\>\$\&\%\\]}'

# Functions
fun_locationMatrix <- function(schematic,findSymbols){
  
  rows <- length(schematic)
  positions <- nchar(schematic[1])
  
  sym_matrix <- matrix(nrow = rows,ncol = positions)
  
  for(rdx in 1:rows){
    
    string <- schematic[rdx]
    
    for(idx in 1:positions){
      
      string_symbols <- stringr::str_locate_all(string = string,
                                                pattern = findSymbols)[[1]][,1]
      
      if(idx %in% string_symbols){
        
        # Approach, make indicators of success in the appropriate matrix cells around the symbol
        
        # Row bounds
        ## Row upper bound
        if(rdx < rows){row_upBound <- rdx + 1}else{row_upBound <- rdx}
        ## Row lower bound
        if(rdx > 1){row_lowBound <- rdx - 1}else{row_lowBound <- rdx}
        ## Col upper bound
        if(idx < positions){col_upBound <- idx + 1}else{col_upBound <- idx}
        ## Col lower bound
        if(idx > 1){col_lowBound <- idx - 1}else{col_lowBound <- idx}
        
        # Expand the positive to each area around the symbol where a number could be.
        sym_matrix[row_lowBound:row_upBound,col_lowBound:col_upBound] <- 1
        
        # 2 means yes a symbol in this position
        sym_matrix[rdx,idx] <- 2
        
      }else{
        
        # If the cell has already been changed from NA it happened above and
        #  we don't want to undo that.
        if(is.na(sym_matrix[rdx,idx])){
          
          # 0 means not a symbol in this position
          sym_matrix[rdx,idx] <- 0
          
        }
        
      }
      
    }
    
  }
  
  return(sym_matrix)
}

fun_getNumbers <- function(schematic, overlap){
  
  rows <- length(schematic)
  positions <- nchar(schematic[1])
  
  for(rdx in 1:rows){
    
    string <- schematic[rdx]
    
    numbers <- tibble::as_tibble(stringr::str_locate_all(string = string,
                                       pattern = '[0-9]+')[[1]])
    
    numbers[,"used"] <- NA
    
    for(idx in 1:positions){
      
      if(overlap[rdx,idx] != 0){
        
        if(nrow(numbers)==0){
          
          warning(sprintf("Found overlap but no number. row: %i, position: %i",rdx,idx))
          
        }else{
          
          for(ndx in 1:nrow(numbers)){
            
            if(between(idx,numbers[ndx,'start'],numbers[ndx,'end'])){
              
              if(!is.na(numbers[ndx,'used'])){
                
                message("Number already counted")
                
              }else if(exists('values')){
                
                values <- c(values,
                            stringr::str_sub(string = string, 
                                             start = numbers[ndx,'start'], 
                                             end = numbers[ndx,'end']))
                
                numbers[ndx,'used'] <- "X"
                
              }else{ 
                
                values <- stringr::str_sub(string = string, 
                                           start = numbers[ndx,'start'], 
                                           end = numbers[ndx,'end'])
                
                numbers[ndx,'used'] <- "X"
                
              }
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  
  return(as.numeric(values))
  
}

# Create a matrix of all symbols and their adjacent cells
symbolLocationMatrix <- fun_locationMatrix(schematic = schematic,
                                           findSymbols = symbols)

# Create a matrix of all numbers
numberLocationMatrix <- fun_locationMatrix(schematic = schematic,
                                           findSymbols = '[:digit:]')

# Using matrix multiplication, find the overlap of the two matrices
overlap <- symbolLocationMatrix * numberLocationMatrix

# Extract the full numbers associated with those overlap locations
nums <- fun_getNumbers(schematic = schematic,
                       overlap = overlap)

# Compute the sum
sum(nums)

# Part 2 ----

# Symbol to find
star_symbol <- r'{[\*]}'

gear_locations <- fun_locationMatrix(schematic = schematic,
                                     findSymbols = star_symbol)

gear_locations
