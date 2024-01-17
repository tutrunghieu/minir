
library(ggplot2); library(gridExtra); library(digest);

scale_map_USUK <- list("US"="red", "UK"="green", "JP"="blue", "ML"="cyan", "SG"="magenta", "Others"="gray");
scale_map_ELM <- list("Education"="red", "Logistic"="green", "Fine arts"="blue", "Social marketing"="cyan", "Telco"="magenta", "Others"="gray");
scale_map_EP_GRCT <- c("amt", "vol", "asp", "GR", "CT");

factor_paste <- function(x, y, sep=" ", levels=NULL) { 
   if( is.null(levels) ) { factor(paste(x, y, sep=sep)) }
   else { factor(paste(x, y, sep=sep), levels=levels) }
}

page_sel <- function(ldf, num=1, len=20) { sk <- (num-1)*len + 1; ek <- sk + len - 1; ldf[sk:ek]; }


fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_c1_e3 <- function(x, div=1e3) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_100 <- function(x, div=100) { format(round(x*div, 1), nsmall=1, big.mark=","); }

scale_cy_c1_e3 <- function() { scale_y_continuous(labels=fmt_c1_e3); }
scale_cy_c1 <- function() { scale_y_continuous(labels=fmt_c1); }
scale_cy_100 <- function() { scale_y_continuous(labels=fmt_100); }


cart2 <- function(x=names(scale_map_USUK), y=scale_map_EP_GRCT) { tdf <- expand.grid(y=y, x=x); paste(as.character(tdf$x), as.character(tdf$y)) }
cart3 <- function(x, y, z) { tdf <- expand.grid(z=z, y=y, x=x); paste(as.character(tdf$x), as.character(tdf$y), as.character(tdf$z)) }

cart2_USUK <- function(x=names(scale_map_USUK), y=scale_map_EP_GRCT) { tdf <- expand.grid(y=y, x=x); paste(as.character(tdf$x), as.character(tdf$y)) }
cart2_ELM <- function(x=names(scale_map_ELM), y=scale_map_EP_GRCT) { tdf <- expand.grid(y=y, x=x); paste(as.character(tdf$x), as.character(tdf$y)) }

rcolor_USUK <- function(val) { scale_map_USUK[[ val ]] }
rcolor_ELM <- function(val) { scale_map_ELM[[ val ]] }
rcolor_hashed <- function(val) { val <- digest(val, algo="md5"); paste0("#", substr(val, 1, 6)); } 

draw_bars_e3 <- function(gdf, fmt=fmt_c1_e3, rcolor=rcolor_ELM) {  
    col <- rcolor( as.character(gdf$group[1]) );
    ggplot(gdf) + scale_cy_c1_e3() + 
    geom_bar(aes(x=FY, y=amt, group=name), fill=col, stat="identity", show.legend=FALSE) + geom_text(aes(x=FY, y=amt, label=fmt(amt), group=name), show.legend=FALSE) ; }

draw_bars <- function(gdf, fmt=fmt_c1, rcolor=rcolor_ELM) {  
    col <- rcolor( as.character(gdf$group[1]) );
    ggplot(gdf) + scale_cy_c1() + 
    geom_bar(aes(x=FY, y=amt, fill=name), fill=col, stat="identity", show.legend=FALSE) + geom_text(aes(x=FY, y=amt, label=fmt(amt), group=name), show.legend=FALSE) ; }

draw_points <- function(gdf, rcolor=rcolor_ELM) {  
    col <- rcolor( as.character(gdf$group[1]) );
    ggplot(gdf) + scale_cy_100() + 
    geom_line(aes(x=FY, y=amt, group=name), color=col, show.legend=FALSE) + geom_point(aes(x=FY, y=amt), color=col, size=1.5, show.legend=FALSE); }

cyclic_chart_ELM <- function(gdf, rcolor=rcolor_ELM) {
    nk <- gdf$name[1];
    
    if(nk == "GR" | nk == "CT") { g <- draw_points(gdf, rcolor=rcolor); } 
    else if(nk == "amt" | nk == "vol") { g <- draw_bars_e3(gdf, rcolor=rcolor); } 
    else { g <- draw_bars(gdf, rcolor=rcolor); }

    return( g + facet_wrap(ncol=1, tile ~ .) + theme(axis.title.x = element_blank(), axis.title.y = element_blank() ) );
}

cyclic_chart_USUK <- function(gdf, rcolor=rcolor_USUK) { cyclic_chart_ELM(gdf, rcolor); }
cyclic_chart_hashed <- function(gdf, rcolor=rcolor_hashed) { cyclic_chart_ELM(gdf, rcolor); }

cyclic_panel <- function(gdf) {
    gl <- ggplot(gdf) + theme(axis.title.x = element_blank(), axis.title.y = element_blank() ) + facet_wrap(ncol=1, tile ~ .);

    ldf <- lapply(split(gdf, gdf$name), FUN=cyclic_chart_hashed);
    gl <- gl + annotation_custom(arrangeGrob(grobs=ldf, ncol=length(ldf) ) );

    return(gl);
}

cyclic_poster <- function(gdf=dataset, FUN=cyclic_panel, tile_ncol=3) {
    names(gdf) <- c("FY", "amt", "group", "name");
    gdf$tile <- gdf$group;
    ldf <- split(gdf, gdf$tile);
    ldf <- lapply(ldf, FUN=FUN);
    grid.arrange(grobs=ldf, ncol=tile_ncol);
}
cyclic_array <- function(gdf=dataset, FUN=cyclic_chart_ELM, tile_levels=cart2_ELM(), tile_ncol=5) {
    names(gdf) <- c("FY", "amt", "group", "name");
    gdf$tile <- factor_paste(gdf$group, gdf$name, levels=tile_levels);

    ldf <- split(gdf, gdf$tile);
    ldf <- lapply(ldf, FUN=FUN);
    grid.arrange(grobs=ldf, ncol=tile_ncol);
}

