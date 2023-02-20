### Read in Banktrans csv ###
ReadBT <- function(filename){
  df <- read_delim(filename, delim = ";", escape_double = FALSE, col_types = cols(Rentedatum = col_date(format = "%Y-%m-%d"), 
                    `Tegenrekening BIC` = col_skip(), Bijlage = col_skip(), ...17 = col_skip()),
                    locale = locale(decimal_mark = ",", grouping_mark = "."), na = "empty", trim_ws = TRUE) %>%
    mutate("Type" = Banktype,
           "Omschrijving" = paste(Omschrijving, Opmerkingen),
           
           # adjust savings rows
           "Tegenrekening" = ifelse(grepl("W59431402", Omschrijving), "W59431402", Tegenrekening),
           "Type" = as.factor(ifelse(Tegenrekening == "W59431402", "Sparen", Type)),
           "Omschrijving" = ifelse(Type == "Sparen", "", Omschrijving),
           
           "Omschrijving" = gsub("Valutadatum:.*", "", Omschrijving),
           .keep = "unused") %>%
    rename("Naam" = `Ten name van`,
           "Datum" = Rentedatum,
           "Tag" = Categorie) %>%
    select(Datum, Naam, Rekening, Tegenrekening, Bedrag, Omschrijving, Type, Tag) %>%
    data.frame()
}

### Read in ING csv###
ReadING <- function(filename){
  df <-read.csv2(filename, check.names = FALSE) %>%
    mutate("Datum" = ymd(Datum),
           "Bedrag" = ifelse(`Af Bij` == "Af", `Bedrag (EUR)` * -1, `Bedrag (EUR)`),
           "Omschrijving" = gsub(".*Omschrijving: (.*) IBAN:.*", "\\1", Mededelingen),
           
           # adjust savings rows
           "Tegenrekening" = ifelse(grepl("W59431402", Omschrijving), "W59431402", Tegenrekening),
           "Type" = as.factor(ifelse(Tegenrekening == "W59431402", "Sparen", Mutatiesoort)),
           "Omschrijving" = ifelse(Type == "Sparen", gsub(".*spaarrekening W[0-9]* (.*)", "\\1", Omschrijving), Omschrijving),
           
           "Omschrijving" = gsub("Valutadatum:.*", "", Omschrijving),
           .keep = "unused", .after = Code) %>%
    rename("Naam" = `Naam / Omschrijving`,
           "Saldo" = `Saldo na mutatie`) %>%
    select(-c(Saldo, Code))
}

### Read in ABNA TAB###
ReadABNA <- function(filename){
  df <- read.delim(filename, header = FALSE, skipNul = TRUE)
  colnames(df) <- c("Rekening", "NA1", "Datum", "NA2", "NA3", "NA4", "Bedrag", "Omschrijving")
  df %<>%
    select(-contains("NA")) %>%
    mutate("Omschrijving" = gsub("  +", "; ", Omschrijving),
           "Tegenrekening" = case_when(grepl("/TRTP/", Omschrijving) ~ gsub(".*/IBAN/(.{18}).*", "\\1", Omschrijving),
                                       grepl("IBAN", Omschrijving) ~ gsub(".*IBAN: (.{18}).*", "\\1", Omschrijving),
                                       .default = ""),
           "Type" = as.factor(case_when(grepl("ABN AMRO Bank", Omschrijving) ~ "Diversen",
                                        grepl("ncasso|aandpremie", Omschrijving) ~ "Incasso",
                                        grepl("iDEAL", Omschrijving) ~ "iDEAL",
                                        grepl("verboeking|OVERBOEKING", Omschrijving) ~ "Overschrijving",
                                        grepl("eriodieke overb", Omschrijving) ~ "Online bankieren",
                                        grepl("BEA", Omschrijving) ~ "Betaalautomaat",
                                        .default = "")),
           "Naam" = case_when(grepl("Naam", Omschrijving) ~ gsub(".*Naam: (.*)(;.*?).*", "\\1", Omschrijving),
                              grepl("BasisPakket", Omschrijving) ~ "Kosten BasisPakket",
                              grepl("ring 625680936", Omschrijving) ~ "Maandpremie verzekering 625680936",
                              grepl("/TRTP/", Omschrijving) ~ gsub(".*/NAME/(.*)(/.*?).*", "\\1", Omschrijving),
                              grepl("BEA", Omschrijving) ~ gsub(".*/.{5} (.*),.*", "\\1", Omschrijving),
                              .default = "" ),
           "Omschrijving" = gsub(";", "", Omschrijving),
           "Bedrag" = as.numeric(sub(",", ".", Bedrag)),
           "Datum" = ymd(Datum),
           "Rekening" = as.character("NL80ABNA0625680936"),
           "Tag" = "") %>%
    select(Datum, Naam, Rekening, Tegenrekening, Bedrag, Omschrijving, Type, Tag)
}

