

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
draw_bridge_YTD <- function(df=rename(dataset, "xx", "yy", "fill", "tag"), more=NULL, 
color_mapping=list(major='yellow', hypermarkets='pink', supermarkets='magenta', others='cyan'), 
box_width=0.47, yy_fmt=fmt_c1_e3, fmt=fmt_c1_e3, xx_angle=25, major_mult=0.5, major_angle=0, minor_angle=25, legend=TRUE) {
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
draw_line_array_YTD <- function(df=rename(dataset, "xx", "yy", "fill", "panel"), more=NULL) {
    if( is.null(more) ) { 
        more <- list(yy_fmt=fmt_c1_e3, label_fmt=fmt_c1_e3, xx_angle=0, make_col=make_color, legend=TRUE);
        more <- c(more, array_ncol=2);
    }

    ldf <- split(df, df$panel);
    ldf <- lapply(ldf, FUN=function(ddd) { draw_lines_YTD(ddd); });
    grid.arrange(grobs=ldf, ncol=more$array_ncol);    
}

#-------------------------------------------
draw_lines_YTD <- function(df=rename(dataset, "xx", "yy", "fill"), more=NULL) {
    if( is.null(more) ) { 
        more <- list(yy_fmt=fmt_c1_e3, label_fmt=fmt_c1_e3, xx_angle=0, make_col=make_color, legend=TRUE);
    }

    df$sel <- as.character(df$xx);
    g <- ggplot(df);
    g <- add_lines_YTD(g, df[df$sel<"YTD", ], more);
    g <- add_lines_YTD(g, df[df$sel>"YTD", ], more);
    print(g);
}

#-------------------------------------------
add_lines_YTD <- function(g, df, more=NULL) {
    if( is.null(more) ) { 
        more <- list(yy_fmt=fmt_c1_e3, label_fmt=fmt_c1_e3, xx_angle=0, make_col=make_color, legend=TRUE);
    }

    ldf <- split(df, df$fill);

    map <- list();    
    for(nk in names(ldf)) {
        ddd <- ldf[[nk]];
        ck <- more$make_col(nk, more);
        map[[nk]] <- ck;
        g <- g + geom_rect(data=ddd, aes(xmin=xx, xmax=xx, ymin=yy, ymax=yy, fill=fill), show.legend=more$legend);
        g <- g + geom_line(data=ddd, aes(x=xx, y=yy, fill=fill, group=fill), color=ck, show.legend=FALSE);
        g <- g + geom_point(data=ddd, aes(x=xx, y=yy, fill=fill, group=fill), color=ck, show.legend=FALSE);
        g <- g + geom_text(data=ddd, aes(x=xx, y=yy, label=more$label_fmt(yy), group=fill), show.legend=FALSE);
    }

    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) );
    g <- g + scale_y_continuous(labels=more$yy_fmt);
    g <- g + scale_fill_manual(values=map);
    return(g);
}

#-------------------------------------------
draw_carousel_chart1 <- function(ddd, more) { 
    g <- ggplot(ddd) + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", show.legend=more$legend); 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) );
    g <- g + scale_y_continuous(labels=more$yy_fmt);
    if( !is.null(more$color_mapping) ) g <- g + scale_fill_manual(values=more$color_mapping);
    return(g);
}

#-------------------------------------------
draw_carousel_chart2 <- function(ddd, more) {  
    g <- ggplot(ddd) + geom_bar(aes(x=xx, y=yy, fill=fill), stat="identity", position="fill", show.legend=more$legend); 
    g <- g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle=more$xx_angle) );
#    g <- g + scale_y_continuous(labels=more$yy_fmt);
    if( !is.null(more$color_mapping) ) g <- g + scale_fill_manual(values=more$color_mapping);
    return(g);
}

#-------------------------------------------
draw_carousel_SF <- function(df=rename(dataset, "xx", "yy", "fill"), more=NULL) {
    if( is.null(more) ) {
        more <- list(chart1=draw_carousel_chart1, chart2=draw_carousel_chart2, array_ncol=2, xx_angle=15, yy_fmt=fmt_c1_e3, legend=TRUE);
	more$color_mapping <- list('A, top 10 customers'='yellow', 'B, other customers'='grey', 'A, top 10 stores'='yellow', 'B, other stores'='grey')
    }
    
    ldf <- list(stack=more$chart1(df, more), full=more$chart2(df, more));
    grid.arrange(grobs=ldf, ncol=more$array_ncol);
}

