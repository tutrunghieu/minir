library(ggplot2); library(gridExtra);

scale_map_ELM <- list("Education"="red", "Logistic"="green", "Fine arts"="blue", "Social marketing"="cyan", "Telco"="magenta", "Others"="gray");
scale_map_EP_GRCT <- c("amt", "vol", "asp", "GR", "CT");
factor_paste <- function(x, y, sep=" ") { factor(paste(x, y, sep=sep)) }
page_sel <- function(ldf, num=1, len=20) { sk <- (num-1)*len + 1; ek <- sk + len - 1; ldf[sk:ek]; }

fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_c1_e3 <- function(x, div=1e3) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_100 <- function(x, div=100) { format(round(x*div, 1), nsmall=1, big.mark=","); }

scale_cy_c1_e3 <- function() { scale_y_continuous(labels=fmt_c1_e3); }
scale_cy_c1 <- function() { scale_y_continuous(labels=fmt_c1); }
scale_cy_100 <- function() { scale_y_continuous(labels=fmt_100); }


cart2 <- function(x=names(scale_map_USUK), y=scale_map_EP_GRCT) { tdf <- expand.grid(y=y, x=x); paste(as.character(tdf$x), as.character(tdf$y)) }
cart3 <- function(x, y, z) { tdf <- expand.grid(z=z, y=y, x=x); paste(as.character(tdf$x), as.character(tdf$y), as.character(tdf$z)) }

draw_bars_e3_hashed <- function(gdf, fmt=fmt_c1_e3) {  
    col <- 'blue'; # col <- scale_map_USUK[[ as.character(gdf$group[1]) ]];
    ggplot(gdf) + scale_cy_c1_e3() + 
    geom_bar(aes(x=FY, y=amt, group=name), fill=col, stat="identity", show.legend=FALSE) + geom_text(aes(x=FY, y=amt, label=fmt(amt), group=name), show.legend=FALSE) ; }

draw_bars_hashed <- function(gdf, fmt=fmt_c1) {  
    col <- 'blue'; # col <- scale_map_USUK[[ as.character(gdf$group[1]) ]];
    ggplot(gdf) + scale_cy_c1() + 
    geom_bar(aes(x=FY, y=amt, fill=name), fill=col, stat="identity", show.legend=FALSE) + geom_text(aes(x=FY, y=amt, label=fmt(amt), group=name), show.legend=FALSE) ; }

draw_points_hashed <- function(gdf) {  
    col <- 'blue'; # col <- scale_map_USUK[[ as.character(gdf$group[1]) ]];
    ggplot(gdf) + scale_cy_100() + 
    geom_line(aes(x=FY, y=amt, group=name), color=col, show.legend=FALSE) + geom_point(aes(x=FY, y=amt), color=col, size=1.5, show.legend=FALSE); }

cyclic_chart_hashed <- function(gdf) {
    nk <- gdf$name[1];
    
    if(nk == "GR" | nk == "CT") { g <- draw_points_hashed(gdf); } 
    else if(nk == "amt" | nk == "vol") { g <- draw_bars_e3_hashed(gdf); } 
    else { g <- draw_bars_hashed(gdf); }

    return( g + facet_wrap(ncol=1, tile ~ .) + theme(axis.title.x = element_blank(), axis.title.y = element_blank() ) );
}


cyclic_array <- function(gdf=dataset, FUN=cyclic_chart_hashed, tile_fp=factor_paste, tile_levels=cart2(), page_num=1, page_len=20, tile_ncol=5) {
    names(gdf) <- c("FY", "amt", "group", "name");
    gdf$tile <- tile_fp(gdf$group, gdf$name);

    ldf <- split(gdf, gdf$tile);
    ldf <- lapply(page_sel(ldf, num=page_num, len=page_len), FUN=FUN);
    grid.arrange(grobs=ldf, ncol=tile_ncol);
}

cyclic_array(page_num=1, page_len=20, tile_fp=factor_paste); 
