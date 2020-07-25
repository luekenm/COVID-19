fipsCoercer <- function(table) {
  return(mutate(table, subregion = if_else((region == "florida" | region == "mississippi") & (subregion == "desoto"), "de soto",
                if_else((region == "alabama" | region == "georgia" | region == "indiana" | region == "missouri" | region == "tennessee") & (subregion == "dekalb"), "de kalb",
                        if_else(substr(subregion, 1, 4) == "st. ", paste("st ", substring(subregion, 5), sep = ""),
                                if_else(substr(subregion, 1, 5) == "ste. ", paste("ste ", substring(subregion, 6), sep = ""),
                                        if_else((region == "south dakota" & subregion == "oglala lakota"), "shannon",
                                                if_else((region == "north dakota" & subregion == "lamoure"), "la moure",
                                                        if_else((region == "illinois" & subregion == "dupage"), "du page",
                                                                if_else((region == "illinois" & subregion == "lasalle"), "la salle",
                                                                        if_else((region == "texas" & subregion == "dewitt"), "de witt",
                                                                                if_else((region == "iowa" & subregion == "o'brien"), "obrien",
                                                                                        if_else((region == "missouri" & subregion == "city of st. louis"), "st louis city",
                                                                                                subregion)))))))))))))
}

# ? near franklin VA
#Queenannes MD
#Balt MD
#ST Mary's MD
#?Fairfax MD