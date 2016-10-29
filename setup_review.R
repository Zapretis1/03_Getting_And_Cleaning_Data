## 2016-10-28 Created by Zapretis1
## 
## This R script called setup_review.R helps fellow markers setting up the
## environment to make peer reviewing easier, faster and straightforwar
## This scripts downloads the raw data in an appropriate directory
## unzipping the files
## The user ends up with a ready-to-use environment for running the script
## created for the assignment, namely run_analysis.R

## Custom parameters
data_dir_name <- "03_week4_peer_graded_assignment_zapretis1" #name of the directory to store the data
data_zip_name <- "UCI HAR Dataset.zip" #name of the zip file
data_unzip_name <- "UCI HAR Dataset" #name of the unzip location
script_name <- "run_anlysis.R"

## Assign name of the URL where data can be retrieved
file_URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Create the destination file relative path name for the zipped and unzipped files
my_destzip <- paste(".", data_dir_name, data_zip_name, sep="/")
my_destunzip <- paste(".", data_dir_name, data_unzip_name, sep="/")
my_destscript <- paste(".", data_dir_name, script_name, sep="/")

## Create directory to store data
if(!file.exists(data_dir_name)){dir.create(data_dir_name)}

## Download file
download.file(file_URL, destfile = my_destzip)

## Store download date
date_downloaded <- date()

## List files to check the downloaded file
print(list.files(data_dir_name))

## Unzip the files in the main directory
unzip(my_destzip, exdir = data_dir_name)

## List files to check the unzipped files
print(list.files(my_destunzip))

print("Review environement setup completed")


# EOF