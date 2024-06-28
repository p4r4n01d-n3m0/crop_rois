crop_rois <- function(path_to_data,
                      path_to_samples,
                      output_path,
                      classes,
                      roi_size){
        ### CHECK, INSTALL AND LOAD NECESSARY PACKAGES
        package_list <- c("data.table","dplyr","imagefx","magick","fs")
        for (pkg in package_list) {
                test <- require(pkg, character.only = TRUE)
                if(!test){
                        install.packages(pkg, dependencies = TRUE)
                        library(pkg, character.only = TRUE)
                }
        }
        cat("All packages loaded\n")
        
        ### CHECK THE INPUTS
        if(!dir.exists(path_to_data)){stop(cat("data folder not found!"))}
        if(!dir.exists(path_to_samples)){stop(cat("sample folder not found!"))}
        if(!dir.exists(output_path)){stop(cat("output folder not found!"))}
        if(length(classes) != length(classes[classes >= 0 & classes <= 8])){
                cat("clases should be a vector between 0 and 8, where:\n")
                cat("DTM = 0, CST = 1, RDL = 2, PLF = 3, SLC = 4, TNT = 5, FRM = 6, NID = 7, BIG STUFF = 8")
                stop(cat("try again!"))
        }
        cat("Inputs checked!\n\n")
        
        ### SELECT THE DATA 
        samples <- list.files(path_to_data)
        classes_name <- c("dtm","cst","rdl","plf","slc","tnt","frm","nid","bgs")
        classes_select <- classes_name[classes+1]
        cat("Selected classes:\n",classes_select,"\n\n")
        cat("Samples selected:\n")
        cat(paste(samples, collapse = "\n"),"\n\n")
        cat("Number of samples:\n",length(samples),"\n\n")
        cat("#########################")
        cat("\n")
        
        ### FIRST LOOP: SELECT THE SAMPLE
        rois_total <- 0
        smpls_togo <- length(samples)
        for (smpl in samples) {
                df <- read.csv(paste0(path_to_data,"/",smpl))[,c("X","Y","Slice","Counter")] %>%
                        filter(Counter %in% classes)
                images <- list.files(paste0(path_to_samples,"/",path_ext_remove(smpl)),full.names = T)[unique(df$Slice)]
                cat("Selected sample:\n",path_ext_remove(smpl),"\n")
                cat("Number of images:",length(images),"\n")
                cat("Number of ROIs:",nrow(df),"\n","\n")
                
                ### SECOND LOOP: SELECT THE IMAGE, CORRECT THE POSTION AND CREATES THE "geom" ARGUMENT
                n <- 1
                for (img in images) {
                        image <- image_read(img)
                        half_roi <- roi_size*3.38
                        X_lim <- half_roi - 4928
                        y_lim <- half_roi - 3264
                        ## change the X,Y position from the center to the upper left conner of the ROIs
                        df2 <- df[df$Slice == unique(df$Slice)[n],c("X","Y","Counter","Slice")] %>%
                                mutate("X" = .$X - half_roi,
                                       "Y" = .$Y - half_roi)
                        ## change the X,Y position to ROIs next to the image margin,
                        ## ensuring the same dimensions for every ROI
                        for (w in seq_along(1:nrow(df2))) {
                                if(df2$X[w] < 0){df2$X[w] <- 0}else{df2$X[w] <- df2$X[w]}
                                if(df2$X[w]+2*half_roi > 4928){df2$X[w] <- df2$X[w] -((df2$X[w]+2*half_roi) - 4928)}else{df2$X[w] <- df2$X[w]}
                                if(df2$Y[w] < 0){df2$Y[w] <- 0}else{df2$Y[w] <- df2$Y[w]}
                                if(df2$Y[w]+2*half_roi > 3264){df2$Y[w] <- df2$Y[w] -((df2$Y[w]+2*half_roi) - 3264)}else{df2$Y[w] <- df2$Y[w]}
                        }
                        ## create the Geom data for each ROI
                        df2 <- df2 %>% 
                                mutate("Geom" = paste0(roi_size*6.76,"x",
                                                       roi_size*6.76,"+",
                                                       .$X,"+",
                                                       .$Y))
                        
                        ### THIRD LOOP: CROP THE ROI AND SAVE IN THE RIGHT FOLDER
                        for (c in seq_along(df2$Geom)) {
                                ROI <- image_crop(image,df2[c,"Geom"])
                                class_dir <- paste0(output_path,"/",classes_name[df2[c,3]+1])
                                if(!dir.exists(class_dir)){dir.create(class_dir)}
                                roi_path <- paste0(class_dir,"/",
                                                   path_ext_remove(smpl),"_"
                                                   ,df2$Slice[c],"_",
                                                   round(df2$X[c],0),"_",
                                                   round(df2$Y[c],0),".jpg")
                                image_write(ROI,path = roi_path)
                        }
                        n <- n+1
                }
                smpls_togo <- smpls_togo-1
                rois_total <- rois_total + nrow(df)
                cat("Sample finished\n")
                cat("Samples to go: ", smpls_togo,"\n")
                cat("ROIs so far:",rois_total,"\n\n")
                cat("#########################")
                cat("\n")
        }
        cat("\nPlin!!\n")
        cat("Your ROIs are ready")
}