
df1 <- read.Alteryx("#1", mode="data.frame");
df2 <- read.Alteryx("#2", mode="data.frame");
CRAN_MASTER <- as.character(df2$cran_master[1]);
CRAN_ROOT <- dirname(CRAN_MASTER);


ACTIVE_PATH <- file.path(Sys.getenv('USERPROFILE'), "R401"); 
dir.create(ACTIVE_PATH, recursive = TRUE, showWarnings = FALSE);
.libPaths( ACTIVE_PATH );

run_alteryx_page <- function(df1, df2, page1, vars=ls()) {
	alteryx_params <- list(tabular=df1, textual=df2); 

	Sys.setenv(CRAN_MASTER = CRAN_MASTER);
	Sys.setenv(CRAN_CORE = file.path(CRAN_ROOT, "cran/core.R") );
	Sys.setenv(CRAN_ACTIVE = page1);

	Sys.setenv(dput_dget = file.path(CRAN_ROOT, "temp") );
	Sys.setenv(IMAGE_FOLDER = file.path(CRAN_ROOT, "images") );

	if( "AlteryxCodePage" %in% vars ) {
		p <- file.path(Sys.getenv("dput_dget"), paste0(page1, "-cached-for-java.R") );
	   
		if( !file.exists(p) ) {
			alteryx_params$ls <- vars; 
			dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE);
			dput(alteryx_params, p);
		} 
	}
	
	source( Sys.getenv("CRAN_MASTER") );
}


tdf <- data.frame(name="master", val=Sys.getenv("CRAN_MASTER") );
write.Alteryx(tdf, 1);

