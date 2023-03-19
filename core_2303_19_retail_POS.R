

#-------------------------------------------
library(ggplot2); library(openssl); library(gridExtra);

#-------------------------------------------
main <- function(name='figure1_line', expr=NULL) { expr(); }

#-------------------------------------------
rename <- function(df, ...) { names(df) <- unlist(list(...)); return(df); }

#-------------------------------------------
make_color <- function(nk, map, pos=2) { paste0('#', substr(md5(nk), pos, pos+5) ); }


#-------------------------------------------
fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_c1_e3 <- function(x, div=1e3) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_c1_e6 <- function(x, div=1e6) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_c1_e9 <- function(x, div=1e9) { format(round(x/div, 1), nsmall=1, big.mark=","); }



#-------------------------------------------
draw_bridge_YTD <- function(df=rename(dataset, "xx", "yy", "fill", "tag"), 
color_mapping=list(major='yellow', hypermarkets='red', supermarkets='green', others='cyan'), 
box_width=0.47, more=NULL, yy_fmt=fmt_c1_e3, fmt=fmt_c1_e3, xx_angle=25, major_mult=0.5, major_angle=0, minor_angle=25, legend=TRUE) {
    g <- ggplot(df);
    g <- g + geom_text(aes(x=xx, y=0, label='', group=fill), stat="identity", show.legend=FALSE);

    df1 <- df[df$tag=="major", ];    
    g <- g + geom_bar(data=df1, aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=legend);
    g <- g + geom_text(data=df1, aes(x=xx, y=yy*major_mult, label=fmt(yy) ), angle=major_angle, show.legend=FALSE);

    s <- 0;
    for(k in 1:nrow(df)) { if(df$tag[k] == "major") { s <- df$yy[k]; } else { df[k, "y1"] <- s; s <- s + df$yy[k]; df[k, "y2"] <- s; } }
    df$x1 <- as.integer(df$xx) - box_width;
    df$x2 <- df$x1 + 2*box_width;

    df1 <- df[df$tag!="major", ];    
    g <- g + geom_rect(data=df1, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), show.legend=legend);
    g <- g + geom_text(data=df1, aes(x=(x1+x2)/2, y=(y1+y2)/2, label=fmt(yy) ), angle=minor_angle, show.legend=FALSE);

    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=xx_angle) );
    g <- g + scale_fill_manual(values=color_mapping);
    g <- g + scale_y_continuous(labels=yy_fmt);


    print(g);
}



#-------------------------------------------
draw_lines_YTD <- function(df=rename(dataset, "xx", "yy", "fill"), more=NULL, legend=TRUE) {
    df$sel <- as.character(df$xx);
    g <- ggplot(df);
    g <- add_lines_YTD(g, df[df$sel<"YTD", ]);
    g <- add_lines_YTD(g, df[df$sel>"YTD", ]);
    print(g);
}

#-------------------------------------------
add_lines_YTD <- function(g, df, map=NULL) {
    ldf <- split(df, df$fill);
    
    for(nk in names(ldf)) {
        ddd <- ldf[[nk]];
        ck <- make_color(nk, map);
        g <- g + geom_line(data=ddd, aes(x=xx, y=yy, fill=fill, group=fill), color=ck, show.legend=FALSE);
        g <- g + geom_point(data=ddd, aes(x=xx, y=yy, fill=fill, group=fill), color=ck, show.legend=FALSE);
    }

    return(g);
}
