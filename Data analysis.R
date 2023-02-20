library(renv)
library(readr)
library(tidyverse)
library(lubridate)
library(magrittr)

source("Unify input read in.R")
source("functions.R")

# Read in files
bt <- rbind(suppressMessages(ReadBT("Input/Banktrans_tm_02022020.csv")),
            ReadING("Input/NL39INGB0006347095_01-02-2020_03-04-2022.csv"),
            ReadING("Input/NL22INGB0798255498_23-02-2020_03-04-2022.csv"),
            ReadABNA("Input/220404213522_04112020_04042022.TAB"))

# TODO: categorize data




