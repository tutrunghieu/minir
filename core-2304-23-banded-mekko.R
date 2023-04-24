library(ggplot2); library(gridExtra);

#-------------------------------------------
fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }

#-------------------------------------------
fmt_100 <- function(x, s=100) { paste0(format(round(x*s, 1), nsmall=1, big.mark=","), '%'); } 


#-------------------------------------------
add_table <- function(g, df) {
    g <- g + annotation_custom(tableGrob(df), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf); 
}

#-------------------------------------------
add_x1_x2_via_yy <- function(df) {
    s <- 0; sum_yy <- sum(df$yy); 
    for(k in 1:nrow(df)) { df[k, "yy100"] <- df[k, "yy"]/sum_yy; df[k, "x1"] <- s; s <- s + df[k, "yy100"]; df[k, "x2"] <- s; }
    return(df);
}


#-------------------------------------------
make_adaptive_bands <- function(more, bands=c(0.4, 0.4, 0.2), 
subsets=c(add_adaptive_band_f1, add_adaptive_band_f2, add_adaptive_band_f3), 
edit_more=NULL) {    
    rdf <- list(); bands <- bands / sum(bands);
    s <- 0;
    for(k in seq_along(bands)) {
        y1 <- s; s <- s + bands[k]; y2 <- s;
        rdf[[k]] <- list(y1=y1, y2=y2, gg=subsets[[k]]);
    }

    return(rdf); 
}

#-------------------------------------------
make_adaptive_columns <- function(df = dataset[dataset$tag=="geo", ]) {
    twd <- sum(df$yy);
    tdf <- lapply(split(df, df$xx), FUN=function(ddd) { sum(ddd$yy)/twd; });

    rdf <- data.frame(); s <- 0;
    for(nk in names(tdf)) {
        rdf[nk, "xx"] <- nk; 
        rdf[nk, "x1"] <- s; s <- s + tdf[[nk]]; rdf[nk, "x2"] <- s; 
    }
    return(rdf);
}


#-------------------------------------------
draw_mosaic_plot <- function(bands=list(Alice=0.4, Bob=0.4, Cody=0.2), hp=make_adaptive_columns(), draw=add_adaptive_band) {
    g <- ggplot() + theme_void();

    more <- list(line_size=1.5, legend=FALSE);
    for(k in 1:nrow(hp)) {
        more$col_name <- as.character(hp$xx[k]); x1 <- hp$x1[k]; x2 <- hp$x2[k];
        s <- 0;
        for(bk in names(bands)) { 
		more$band_name <- bk; y1 <- s; s <- s + bands[[bk]]; y2 <- s;
		g <- g + annotation_custom(ggplotGrob(draw(more)), xmin = x1, xmax = x2, ymin = y1, ymax = y2);  
	}
    }

    g <- g + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0));
    print(g);
}

