# Title: Build File Data Store
# Author: Sai Karthikeyan, Sura
# Date: 2025-01-20


# Global variables for directories
rootDir <- "docDB"
intakeDir <- "docTemp"


# Function to set up the intake files and document store folders
# Creates the intake and document store directories if they do not exist
setupDB <- function() {
  if (!dir.exists(intakeDir)) {
    dir.create(intakeDir)
    print("Document intake folder 'docTemp' created successfully")
  }
  
  if (!dir.exists(rootDir)) {
    dir.create(rootDir)
    print("Document store folder 'docDB' created successfully")
  }
  
  # Create sample files in the intake directory
  sampleFiles <- c(
    "KlainerIndustries.261124.011224.xml",
    "TechCorp.010125.151225.csv",
    "DataSolutions.151224.171224.json",
    "GlobalTech.230124.250124.xml",
    "KlainerIndustries.LLC.261124.011224.xml",
    "TechCorp.010125.txt",
    "Global.Tech.230124.250124.xml",
    "DataSolutions.151224.171224",
    "ExtraCompany.111222.221122.xml",
    "EmptyFile.010124.020124.csv"
  )
  for (file in sampleFiles) {
    filePath <- file.path(intakeDir, file)
    write("Sura's Assignment02.1", file = filePath)
    print(paste("Created sample file:", filePath))
  }
}


# Function to check if the file is properly named
# Returns TRUE if the file follows the naming convention, otherwise FALSE
checkFile <- function(fileName) {
  # Check file format (ClientName.FirstDay.LastDay.ext)
  pattern <- "^[a-zA-Z]+\\.[0-9]{6}\\.[0-9]{6}\\.(xml|csv|json)$"
  if (!grepl(pattern, fileName)) {
    print(paste("Error: Invalid file name format: ", fileName))
    return(FALSE)
  }
  
  parts <- strsplit(fileName, "\\.")[[1]]
  firstDay <- as.Date(parts[2], format = "%d%m%y")
  lastDay <- as.Date(parts[3], format = "%d%m%y")
  
  if (is.na(firstDay) || is.na(lastDay) || lastDay < firstDay) {
    print(paste("Error: Invalid dates in file name: ", fileName))
    return(FALSE)
  }
  return(TRUE)
}

# Function to extract the customer name from the file name
getCustomerName <- function(fileName) {
  parts <- strsplit(fileName, "\\.")[[1]]
  return(parts[1])
}


# Function to extract the first day from the file name
getFirstDay <- function(fileName) {
  parts <- strsplit(fileName, "\\.")[[1]]
  return(parts[2])
}


# Function to extract the extension from the file name
getExtension <- function(fileName) {
  parts <- strsplit(fileName, "\\.")[[1]]
  return(parts[4])
}


# Function to generate the correct document path based on root, first day, and extension
# Returns the full path for the document folder
genDocPath <- function(root, firstDay, ext) {
  return(file.path(root, firstDay, ext))
}


# Function to copy a file from the intake folder to the document folder
# Returns TRUE if the file was successfully copied, FALSE otherwise
storeDoc <- function(intakeFolder, file, docFolder = rootDir) {
  # Check if the intake and document store directories exist, if not, abort
  if (!dir.exists(intakeFolder) || !dir.exists(docFolder)) {
    print("Error: One or both of the required directories ('docTemp' or 'docDB') do not exist. Aborting execution.")
    stop("Required directories are missing. Program aborted.")
  }
  
  # Validate file name
  if (!checkFile(file)) {
    return(FALSE)
  }
  
  # Get customer name, first day, and extension from the file name
  customerName <- getCustomerName(file)
  firstDay <- getFirstDay(file)
  ext <- getExtension(file)
  
  # Generate the document path
  docPath <- genDocPath(docFolder, firstDay, ext)
  
  # Create the directory if it does not exist
  if (!dir.exists(docPath)) {
    dir.create(docPath, recursive = TRUE)
  }
  
  # Copy the file to the document folder
  sourceFile <- file.path(intakeFolder, file)
  destinationFile <- file.path(docPath, customerName)
  
  # Attempt to copy the file and check if successful
  success <- file.copy(sourceFile, destinationFile)
  
  # Check if the copy was successful by comparing sizes
  if (success && file.info(sourceFile)$size == file.info(destinationFile)$size) {
    # Attempt to delete the original file
    deletionSuccess <- unlink(sourceFile)
    if (deletionSuccess == 0) {
      print(paste("Successfully deleted original file:", file))
    } else {
      print(paste("Error: Failed to delete original file:", file))
      return(FALSE)
    }
    return(TRUE)
  }
  
  # If the copy failed, return FALSE
  return(FALSE)
}


# Function to copy all files from the intake folder to the document store
# Removes the original file after confirming successful copy
storeAllDocs <- function(intakeFolder, rootFolder) {
  # List all files in the intake folder
  files <- list.files(intakeFolder)
  
  # Initialize counters for processed and not processed files
  processedFiles <- 0
  notProcessedFiles <- c()
  
  for (file in files) {
    if (storeDoc(intakeFolder, file, rootFolder)) {
      processedFiles <- processedFiles + 1
    } else {
      notProcessedFiles <- c(notProcessedFiles, file)
    }
  }
  
  # Print the results
  print(paste("Successfully processed", processedFiles, "files"))
  
  # Print the list of files that were not processed
  if (length(notProcessedFiles) > 0) {
    print("These files were not processed:")
    for (file in notProcessedFiles) {
      print(file)
    }
  }
}


# Function to reinitialize the document database to a blank state.
resetDB <- function(root) {
  # Get all files and sub folders under the root folder
  # Remove files first
  files <- list.files(root, full.names = TRUE, recursive = TRUE)
  for (file in files) {
    if (file.info(file)$isdir == FALSE) {
      unlink(file)
    }
  }
  # Remove sub directories (after files are deleted)
  subDirs <- list.dirs(root, full.names = TRUE, recursive = TRUE)
  for (dir in subDirs) {
    if (dir != root) {  # Don't remove the root directory
      unlink(dir, recursive = TRUE)
    }
  }
}


# Main function to run the program
main <- function() {
  # Set up the directories and files that need to be processed
  # Check if directories exist, if not, call setupDB()
  if (!dir.exists(intakeDir) || !dir.exists(rootDir)) {
    print("Folders not found. Setting up...")
    setupDB()
  }
  
  # Process all files in the intake folder
  storeAllDocs(intakeDir, rootDir)
  
  # Removes all folders, sub folders, and files in the folder after processing to reinitialize the document database to a blank state
  # resetDB(rootDir)
}


# Call the main function to run the program
main()