
make_vocab <- function(X = c("Alice", "Cody", "Cody", "Bob", "Elma", "Bob"), mode="") {
  df <- data.frame(table(as.character(X)));
  df$val <- 1:nrow(df);
  names(df) <- c("cat", "cnt", "num");
  row.names(df) <- df$cat;
  if(mode=="df") return(df);

  print0(df);
}

