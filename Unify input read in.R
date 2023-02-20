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
  df <- read.delim("Input/220404213522_04112020_04042022.TAB", header = FALSE, skipNul = TRUE)
  colnames(df) <- c("Rekening", "NA1", "Datum", "NA2", "NA3", "NA4", "Bedrag", "X")
  df %<>%
    select(-contains("NA")) %>%
    mutate("X" = gsub(",*( *)$", "", X),
           "X" = gsub("  +", "; ", X),
           "Tegenrekening" = ifelse(grepl("IBAN", X), gsub(".*IBAN: (.{18}).*", "\\1", X), ""),
           "Tegenrekening" = ifelse(grepl("/TRTP/", X), gsub(".*/IBAN/(.{18}).*", "\\1", X), Tegenrekening),
           "X" = str_remove(X, "IBAN: .*; |IBAN: .*"),
           "X" = str_remove(X, "BIC: .*; "),
           "Naam" = ifelse(grepl("Naam", X), gsub(".*Naam: (.*); .*", "\\1", X), ""),
           "Naam" = ifelse(grepl(";", Naam), sub("; .*", "", Naam), Naam),
           "Naam" = ifelse(grepl("BasisPakket", X), "Kosten BasisPakket", Naam),
           "X" = ifelse(grepl("BasisPakket", X), "ABN AMRO Bank N.V.", X),
           "Naam" = ifelse(grepl("ring 625680936", X), "Maandpremie verzekering 625680936", Naam),
           "Naam" = ifelse(grepl("/TRTP/", X), gsub(".*/NAME/(.*)/.*", "\\1", X), Naam),
           "Naam" = ifelse(grepl("/EREF", Naam), gsub("/EREF.*", "", Naam), Naam),
           "Naam" = ifelse(grepl("/REMI", Naam), gsub("/REMI.*", "", Naam), Naam),
           "Naam" = ifelse(grepl("/MARF", Naam), gsub("/MARF.*", "", Naam), Naam),
           "X" = str_remove(X, paste0("; Naam: ",Naam)),
           "Type" = as.factor(case_when(grepl("ABN AMRO Bank", X) ~ "Diversen",
                                        grepl("ncasso", X) ~ "Incasso",
                                        grepl("aandpremie", X) ~ "Incasso",
                                        grepl("iDEAL", X) ~ "iDEAL",
                                        grepl("verboeking|OVERBOEKING", X) ~ "Overschrijving",
                                        grepl("eriodieke overb", X) ~ "Online bankieren",
                                        grepl("BEA", X) ~ "Betaalautomaat",
                                        .default = "")),
           "X" = gsub("BEA; ", "", X),
           "Naam" = ifelse(Type == "Betaalautomaat", gsub(".*/.{5} (.*),.*", "\\1", X), Naam),
           "Naam" = ifelse(grepl("SEPA", X) & Naam == "", gsub("(SEPA .*);.*", "\\1", X), Naam),
           "Bedrag" = as.numeric(sub(",", ".", Bedrag)),
           "Datum" = ymd(Datum),
           "Rekening" = as.character(Rekening),
           "Tag" = "") %>%
    rename("Omschrijving" = X) %>%
    select(Datum, Naam, Rekening, Tegenrekening, Bedrag, Omschrijving, Type, Tag)
}
