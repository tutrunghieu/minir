
#-------------------------------------
ENUM_PAD <- ggplot;
ENUM_NCOL <- 1;


#-------------------------------------
waterfall_enum <- function(wdf, pos='bottom', lab='major', size=0.45, legend=FALSE, ncol=2) { 
    ldf <- split(wdf, wdf$tile);
    ldx <- list();

    for(nk in names(ldf)) {
        df <- waterfall_data(ldf[[nk]], lab=lab, size=size);
        ldx[[nk]] <- waterfall_chart(df, pos=pos, legend=legend) + facet_wrap(ncol=1, tile ~ .);
    }

    grid.arrange(grobs=ldx, ncol=ncol);
}

#-------------------------------------
pad_grobs <- function(grobs, slots=4, pad=ENUM_PAD) {
	n <- length(grobs); 
	while(n < slots) { grobs[[ paste("pad", n) ]] <- pad(); n <- n + 1;   }
	return(grobs);
}

#-------------------------------------
ggplot_ncol <- function(grobs, ncol=ENUM_NCOL) {
	g <- ggplot() + theme_void() + no_cx_expansion() + no_cy_expansion();
	g <- g + annotation_custom(arrangeGrob(grobs=grobs, ncol=ncol), xmin = 0, xmax = 1, ymin = 0, ymax = 1); 
	return(g);	
}

#-------------------------------------
make_frames <- function(grobs, slots=4, expr=ggplot_ncol, pad=pad_grobs) {
	res <- list(); cur <- list(); n <- length(grobs); 
	
	for(k in seq_along(grobs)) {
		cur[[ paste("page", k) ]] <- grobs[[k]];
		if( length(cur) == slots | k==n) { res[[ paste("frame", k) ]] <- expr(pad(cur, slots=slots)); cur <- list(); }
	} 
		
	return(res);		
}


#-------------------------------------
lapply_tile <- function(df, FUN) { lapply(split(df, df$tile), FUN=FUN); }
facet_tile <- function(ncol=1) { facet_wrap(ncol=1, tile ~ .); }

#-------------------------------------
no_cx_expansion <- function() {  scale_x_continuous(expand=c(0, 0)) }
no_cy_expansion <- function() {  scale_y_continuous(expand=c(0, 0)) }
no_dx_expansion <- function() {  scale_x_discrete(expand=c(0, 0)) }
no_dy_expansion <- function() {  scale_y_discrete(expand=c(0, 0)) }
