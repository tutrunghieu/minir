
#-------------------------------------
CRAN_US <- 'http://cran.us.r-project.org'; req <- c("ggplot2", "gridExtra"); ipack <- rownames( installed.packages() ); 
for(pk in req) { if( !(pk %in% ipack) ) install.packages(pk, repos=CRAN_US); }
library(ggplot2); library(gridExtra);


#-------------------------------------
ENUM_PAD <- ggplot;
ENUM_NCOL <- 1;
TDS_hf <- 582;
TDS_wf <- 998;

#-------------------------------------
no_left_text <- function() { theme( axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()); }

#-------------------------------------
no_base_text <- function() { theme( axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()); }

#-------------------------------------
rename <- function(df, ...) { names(df) <- unlist(list(...)); return(df); }

#-------------------------------------
no_axis_titles <- function() { theme(axis.title.x = element_blank(), axis.title.y = element_blank() ); }

#-------------------------------------
no_left_text <- function() { theme( axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()); }

#-------------------------------------
no_base_text <- function() { theme( axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()); }

#-------------------------------------
fmt_c1 <- function(x, div=1) { format(round(x/div, 1), nsmall=1, big.mark=","); }
fmt_c1_e3 <- function(x, div=1e3) { format(round(x/div, 1), nsmall=1, big.mark=","); }
scale_cy_c1_e3 <- function() { scale_y_continuous(labels=fmt_c1_e3); }


#-------------------------------------
print_frames <- function(grobs, path) {
	if( is.null(names(ldx)) ) names(ldx) <- paste("frame", seq_along(ldx));
    res <- list();
	for(nk in names(ldx)) { res[[nk]] <- sprintf(path, nk); make_PNG_file(tar=res[[nk]], expr=ldx[[nk]]); }
	return(res);
}

#-------------------------------------
make_PNG_file <- function(wd=TDS_wf, hg=TDS_hf, tar=NULL, expr=NULL, expr1=NULL, echo=FALSE) {
	if( is.null(expr) ) return(NULL);
	if( is.ggplot(expr) ) { expr1 <- function() { print(expr); } } else { expr1 <- expr; }
	
	if( is.null(tar) ) tar <- file.path(Sys.getenv("USERPROFILE"), '.out/figure1.png');

	dir.create(dirname(tar), recursive=TRUE, showWarnings = FALSE); 
	png(file=tar, width=wd, height=hg); expr1(); muted <- dev.off();
	LAST_PNG_FILE <<- tar;
	
	if(echo) cat("See", tar, "\n");
	
	return(tar);	
}

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
ggplot_void <- function() { ggplot() + theme_void(); }

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
lapply_tile_ncol <- function(df, ncol=2, show=TRUE, ret="", FUN) { 
	ldx <- lapply(split(df, df$tile), FUN=FUN); 
	if(show) grid.arrange(grobs=ldx, ncol=ncol); 
	if(ret=="list") return(ldx);
	if(ret=="grob") return( arrangeGrob(grobs=ldx, ncol=ncol) );
}

#-------------------------------------
no_cx_expansion <- function() {  scale_x_continuous(expand=c(0, 0)) }
no_cy_expansion <- function() {  scale_y_continuous(expand=c(0, 0)) }
no_dx_expansion <- function() {  scale_x_discrete(expand=c(0, 0)) }
no_dy_expansion <- function() {  scale_y_discrete(expand=c(0, 0)) }
