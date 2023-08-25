

#-------------------------------------
dput0 <- function(name="project-settings.R") {

    vars <- list();
    
    vars$include <- function() {
        library(openssl);
        library(ggplot2);
        library(openssl);

        source_md5 <- function(uk, tar=NULL) {
            if( is.null(tar) ) tar <- file.path(Sys.getenv("USERPROFILE"), '.out', paste0(md5(uk), ".R"));
            dir.create(dirname(tar), recursive=TRUE, showWarnings = FALSE); 
            if( !file.exists(tar) ) download.file(uk, tar);
    		source(tar);
        }

        source_md5('https://raw.githubusercontent.com/tutrunghieu/minir/main/2308/sorted-waterfall.R');
        source_md5('https://raw.githubusercontent.com/tutrunghieu/minir/main/2308/split-gantt.R');
        source_md5('https://raw.githubusercontent.com/tutrunghieu/minir/main/2308/waterfall-enum.R');
    }

    dput(vars, file=file.path(Sys.getenv("USERPROFILE"), '.out', name));
}

#-------------------------------------
done <- function() {
	library(ggplot2); print(ggplot() + ggtitle("Saved!!!") );
}
