
### source(file.path(Sys.getenv('USERPROFILE'), 'Desktop/ML70-bridges.R'));
library(ggplot2); 
library(gridExtra); 


fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }

rename <- function(df, ...) { cols <- unlist(list(...)); names(df) <- cols; return(df); }


make_colors <- function(vals=unique(df$type), main="yellow", voc=c("green", "cyan", "pink", "magenta", "blue")) {
   cols <- c();
   s <- 0; n <- length(voc);
   for(vk in vals) { if(vk == "major") { cols[vk] <- main; } else { cols[vk] <- voc[(s %% n) + 1]; s <- s + 1; } }
   return(cols);
}

draw_bridge <- function(df, major="major", fill_colors=NULL) {

for(k in 1:nrow(df)) {
    tk <- df[k, "type"];
    if(tk=="major") { s <- df[k, "amt"]; }
    else { df[k, "y1"] <- s; s <- s + df[k, "amt"]; df[k, "y2"] <- s; }
}

df$x1 <- 1:nrow(df) - 0.43;
df$x2 <- 1:nrow(df) + 0.43;


#grid.table(head(df, 11))

g <- ggplot(df);
g <- g + geom_text(aes(x=mixed, y=0), label='', show.legend=FALSE);

df1 <- df[df$type == major, ];
g <- g + geom_bar(data=df1, aes(x=mixed, y=amt, fill=type), stat="identity", show.legend=FALSE);
g <- g + geom_text(data=df1, aes(x=mixed, y=amt, fill=type, label=fmt_c1(amt) ), show.legend=FALSE);

df1 <- df[df$type != major, ];
g <- g + geom_rect(data=df1, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=type), show.legend=FALSE);
g <- g + geom_text(data=df1, aes(x=(x1+x2)/2, y=(y1+y2)/2, fill=type, label=fmt_c1(amt) ), show.legend=FALSE);

g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
g <- g + theme(axis.text.x = element_text(angle = 25, size=12))

if( !is.null(fill_colors) ) {
   g <- g + scale_fill_manual(values=fill_colors)
}

print(g);
}
