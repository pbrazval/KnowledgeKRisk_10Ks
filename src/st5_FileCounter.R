filecounter <- function(){
  path <- "/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/output/1A files/"
  
  # Create empty vectors to store the year and file count
  years <- c()
  file_counts <- c()
  
  # Iterate over the years
  for (year in 2006:2022) {
    # Construct the folder path for each year
    folder_path <- paste0(path, year)
    
    # Count the number of files in the folder
    file_count <- length(list.files(folder_path))
    
    # Append the year and file count to the respective vectors
    years <- c(years, year)
    file_counts <- c(file_counts, file_count)
  }
  
  # Create a dataframe from the year and file count vectors
  data <- data.frame(Year = years, File_Count = file_counts)
  
  # Print the dataframe
  print(data)
  
  textfolder = "/Users/pedrovallocci/Documents/PhD (local)/Research/By Topic/Measuring knowledge capital risk/text/"
  
  tex_table <- stargazer(data, title = "File Counts by Year", 
                         align = TRUE, header = FALSE, 
                         summary = FALSE, rownames = FALSE, digit.separator = "", out = paste0(textfolder, "file_counts", ".tex"))

}

