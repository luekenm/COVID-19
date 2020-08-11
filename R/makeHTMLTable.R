makeHTMLTable <- function(table, tableID) {
  filerow = c("")
  i = 1
  filerow[i] = "<div><table><tr>"
  i = i + 1
  for(column_name in attr(table, "names")) {
    filerow[i] = paste("<th>", column_name, "</th>", sep = "")
    i = i + 1
  }
  filerow[i] = "</tr>"
  num_row = 1
  for(rows in table[[1]]) {
    num_col = 1
    i = i + 1
    filerow[i] = "<tr>"
    for(cols in table[1,]) {
      i = i + 1
      filerow[i] = paste("<td>", if(is.na(table[num_row,num_col])){""}else{table[num_row,num_col]}, "</td>", sep = "")
      num_col = num_col + 1
    }
    i = i + 1
    filerow[i] = "</tr>"
    num_row = num_row + 1
  }
  i = i + 1
  filerow[i] = "</table></div>"
  write.table(filerow, paste("./output/", tableID, ".html", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
}