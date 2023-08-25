## please call this within your Rscript visualization of PBI workbook
## please make sure the columns (PVM axis, PVM values, component name, sorting order) in order. Names don't matter. Order does.
## please change the color mapping below to color your bars and boxes
colors <- list('major'='yellow', 'price'='#aa0102', 'vol'='#01bb02', 'mix'='#0102cc');

CRAN_US <- 'http://cran.us.r-project.org'; req <- c("ggplot2", "gridExtra"); ipack <- rownames( installed.packages() ); 
for(pk in req) { if( !(pk %in% ipack) ) install.packages(pk, repos=CRAN_US); }
library(ggplot2); library(gridExtra);


## data formatting
waterfall_data <- function(df, size=0.45, lab="major") {
    df <- df[order(df$sort), ];
    df$xx <- factor(df$xx, levels=df$xx);

    for(k in 1:nrow(df)) {
        nk <- df$fill[k];
        if( nchar(nk)==0 ) { df[k, "y1"] <- 0; s <- df$yy[k]; df[k, "y2"] <- s; }
        else { df[k, "y1"] <- s; s <- s + df$yy[k]; df[k, "y2"] <- s; }
    }

    df$x1 <- df$sort - size;
    df$x2 <- df$x1 + 2 * size;
    df$xx_mixed <- ifelse(nchar(df$fill)==0, as.character(df$xx), df$fill);
    df$fill <- ifelse(nchar(df$fill)==0, lab, df$fill);

    return(df);
}

## bar/box rendering
waterfall_chart <- function(df, pos='bottom', legend=TRUE) {
    g <- ggplot(df) + geom_text(aes(x=xx, y=0, label=''), show.legend=FALSE);
    
    g <- g + geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), show.legend=legend);
    g <- g + geom_text(aes(x=(x1+x2)/2, y=(y1+y2)/2, label=fmt_c1(yy), group=fill), show.legend=FALSE);

    g <- g + scale_fill_manual(values=colors, name='') + no_axis_titles() + scale_cy_c1_e3() + scale_x_discrete(labels=df$xx_mixed);
    g <- g + theme(legend.position=pos, legend.title=element_blank());
    print(g);
}

main <- function() {
  df <- rename(dataset, "xx", "yy", "fill", "sort");
  df <- waterfall_data(df);
  waterfall_chart(df);
}  
