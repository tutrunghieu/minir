library(ggplot2); library(gridExtra);

scale_fill_PVM <- function() { scale_fill_manual(values=list(major='#FFE600', price='#C893C7', vol='#95CB89', mix='#C1C1C1')); }
scale_factor_USUK <- c("US", "UK", "JP", "ML", "SG", "Others");

rename <- function(gdf, ...) { names(gdf) <- unlist(list(...)); return(gdf); }
fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_c1_e3 <- function(x, div=1e3) { format(round(x/div, 1), nsmall=1, big.mark=","); }
scale_cy_c1_e3 <- function() { scale_y_continuous(labels=fmt_c1_e3); }

waterfall_array <- function(gdf, ncol=2, tile_vals=scale_factor_USUK) {
    gdf$FY_long <- paste(gdf$FY, gdf$fill);
    gdf$FY_short <- ifelse(gdf$fill=="major", gdf$FY, gdf$fill);
    gdf$tile <- factor(gdf$tile, levels=tile_vals);

    ldf <- split(gdf, gdf$tile);
    ldf <- lapply(ldf, FUN=waterfall_chart); 
    grid.arrange(grobs=ldf, ncol=ncol);
}

waterfall_chart <- function(gdf, width=0.45, text_fmt=fmt_c1_e3, text_vert=+1, left_fmt=scale_cy_c1_e3) {
    s <- 0;
    for(k in 1:nrow(gdf)) {
        if( gdf$fill[k] == "major") { gdf[k, "y1"] <- 0; gdf[k, "y2"] <- s <- gdf$amt[k]; gdf[k, "ym"] <- gdf[k, "y2"]/2; }
        else { gdf[k, "y1"] <- s; s <- s + gdf$amt[k]; gdf[k, "y2"] <- s; gdf[k, "ym"] <- min(gdf[k, "y1"], gdf[k, "y2"]); }
    }

    gdf$x1 <- as.integer(factor(gdf$FY_long)) - width;
    gdf$x2 <- gdf$x1 + 2*width;

    g <- ggplot(gdf) + facet_wrap(ncol=1, tile ~ .) + geom_text(aes(x=FY_long, y=0, label=''), show.legend=FALSE);

#    g <- g + geom_bar(aes(x=FY_long, y=amt, fill=fill), stat="identity", show.legend=FALSE);
   g <- g + geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), show.legend=FALSE);
   g <- g + geom_text(aes(x=(x1+x2)/2, y=ym, label=text_fmt(amt)), vjust=text_vert, show.legend=FALSE);

    g <- g + theme(axis.text.x = element_text(angle=25), axis.title.x = element_blank(), axis.title.y = element_blank() );
    g <- g + scale_x_discrete(labels=gdf$FY_short) + scale_fill_PVM() + left_fmt();
    return(g);
}

waterfall_array(rename(dataset, "FY", "amt", "fill", "tile"));
