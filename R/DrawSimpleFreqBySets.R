require(TBE)
require(ruisu)
require(rgdal)
# Datos -------------------------------------------------------------------
DrawSimpleFreqBySets <- function(
Name = "Pr2004",
dirInput = "J:/_SurveyAnalysisAnchoveta/_ncPr2004/Data",
dirOut = "D:/_CursoGitGitHub",
InputFile = "Biometric_ncProspeccion2004v1.csv",
SpTarget = "anchoveta",
format = "pdf" , #/"png"
show.warnings = F,
stock = "nc" , # "nc" / "sur"
ncol = 5,
nrow = 8,
Nlim = 20,
ylim = c(0,0.50)){
# -------------------------------------------------------------------------
fileName <- file.path(dirInput, InputFile)

Base = read.table(file = fileName, sep = ",", header = T)
if(TBE:::isImarsis2(file = fileName, sep = ",")){
  Base <- TBE:::convertImarsisV2toV1(Base)
  }

Base[, "NOMBRE_COMERCIAL"] <- gsub(",.*$", "", Base[, "NOMBRE_COMERCIAL"])
Base[, "NOMBRE_COMERCIAL"] <- tolower(Base[, "NOMBRE_COMERCIAL"])
Base = subset(x = Base, subset = Base$NOMBRE_COMERCIAL %in% SpTarget)

# Format data
ListIMARPES = c("IMARPE IV", "IMARPE V")
Vessel = trimws(Base$EMBARCACION)
Vessel[grep("OLAYA", Vessel)] <- "OLAYA"
Vessel[grep("FLORES", Vessel)] <- "FLORES"
Vessel = sub("\\,.*", "", Vessel)
Vessel = ifelse(Vessel %in% ListIMARPES, 
                paste(substring(Vessel, 1, 2), sub(".*? ", "", Vessel)),
                substring(Vessel, 1, 3))
Set = trimws(Base$REO_NNUMLAN)
VesselSet = paste(Vessel, Set, sep = "-")

Lon = -abs(Base$LONGITUD_INICIAL)
Lat = -abs(Base$LATITUD_INICIAL)
Area = isopArea.assigner(dataPoints = data.frame(Lon, Lat), colLon = "Lon", colLat = "Lat")
CapTot = Base$CAPTURA_ACTIVIDAD
CapEsp = Base$REE_NPESESP
Marks = Base$LONGITUD_ESPECIE
SFreq = Base$FREC_SIMPLE

# Integrate data
BaseSets = data.frame(VesselSet, Area, Lat, Lon, CapTot, CapEsp, Marks, SFreq)

# Filtered hauls
fileFilterHaul = paste0("filteredHauls_", Name, ".txt")
fileFilterHaul = file.path(dirOut, fileFilterHaul)

if(file.exists(fileFilterHaul)){
  warning("Archivo ", fileFilterHaul, " existente.", call. = FALSE, immediate. = TRUE)
  fHauls <- scan(fileFilterHaul, what = character(), skip = 1)
  if(length(fHauls) > 0){
    cat("# Lances filtrados:", fill = TRUE)
    cat(fHauls, "", sep = "\t", fill = TRUE)
    BaseSets <- BaseSets[!(BaseSets$VesselSet %in% fHauls), ]
  }
}

# Arrange sets by latitude (decreasing order)
ListSets = unique(BaseSets$VesselSet)
NumSets = length(ListSets)

ListSetsOrder = NULL
for(i in 1:NumSets){
  BSet <- BaseSets[which(BaseSets$VesselSet == ListSets[i]), ]
  tmp <- BSet[1,]
  tmp[6] <- round(sum(BSet$Marks*BSet$SFreq, na.rm = T)/sum(BSet$SFreq, na.rm = T), digits = 2)
  tmp[7] <- sum(BSet$SFreq, na.rm = T)
  ListSetsOrder = rbind(ListSetsOrder, tmp)
}
ListSetsOrder = ListSetsOrder[order(ListSetsOrder$Lat, decreasing = T), ]

latCategories = list(nc = c(-16, -2), sur = c(-20, -16), all = c(-20, -2))
latCategories = latCategories[[stock]]
ListSetsOrder = ListSetsOrder[latCategories[1] < ListSetsOrder$Lat &
                              ListSetsOrder$Lat <= latCategories[2] ,] 

if(format == "png"){
  
  init = seq(1,nrow(ListSetsOrder), (ncol*nrow)) 
  pnglist = list()
  for(x in 1:length(init)){
    if(x == length(init)){
      pnglist[[x]] = seq(init[x], nrow(ListSetsOrder))
      }else{pnglist[[x]] = seq(init[x], (init[x]+ (ncol*nrow-1)))}
  }
  
  for (p in 1:length(pnglist)){
    listseq <- pnglist[[p]]
    # Draw and save in pdf (simple) length frequencies by set
    nameplot = paste0("FreqBySet_", Name, "_", p,".png")
    nameplot = file.path(dirOut, nameplot)
    png(nameplot, width = 1750, height = 2100, res = 250)
    nc <- ncol
    nr <- nrow
    nwarnings <- 0
    par(mfrow = c(nr,nc), mar = c(0,0,0,0), oma = c(4.5,4,1,3))
    for(i in listseq){
      BaseBySet = BaseSets[which(BaseSets$VesselSet == ListSetsOrder$VesselSet[i]), ]
      x = BaseBySet$Marks
      y = BaseBySet$SFreq/sum(BaseBySet$SFreq, na.rm = T)
      y = y[order(x)]
      x = x[order(x)]
      x0 = seq(1.0, 20, by=0.5)
      y0 = rep(NA, length(x0))
      y0[x0%in%x] = y
      #tryCatch({y0[x0%in%x] = y}, warning=function(w) print(i)) 
      y0 = AddZeros(y0,1)
      juv <- round(100*sum(y[x<12], na.rm = T), digits = 1)
      plot(x0, y0, type = "l", axes = FALSE, xlab = "", ylab = "", xlim = c(1.0,20), ylim = ylim,
         xaxs = "i", yaxs = "i", las = 2, col = 2, lwd = 2)
      
      if(i%%(nr*nc)>((nr-1)*nc) | i%%(nr*nc)==0 | i > (NumSets-nc)){
      axis(1, at = seq(1,20,0.5), labels = NA)
      axis(1, at = seq(2,20,2), labels = seq(2,20,2),  padj = -0.5, cex.axis = 0.60)
      }
      
      if(i%%nc == 1 & i%%2 == 1){
        axis(2, las = 2, cex.axis = 0.70)
      }
      if(i%%nc == 0 & i %%2 == 0)
      {axis(4, las = 2, cex.axis = 0.70)}
    
      abline(v = 12, lty = 2, col = 4)
      mtext(ListSetsOrder$VesselSet[i], side = 3, adj = 0.95, line = -1.5, font = 2, cex = 0.70)
      mtext(paste0(round(juv), "%"," juv"), side = 3, adj = 0.05, line = -1.5, cex = 0.60)
      mtext(paste0("n=", sum(BaseBySet$SFreq, na.rm = TRUE)), side = 3, adj = 0.05, line = -3.0, cex = 0.60)
      
      grad <- floor(-1*BaseBySet$Lat[1])
      minu <- round((-1*BaseBySet$Lat[1])%%1*60)
      if(grad<10){grad = paste0(0, grad)}else{grad = as.character(grad)}
      if(minu<10){minu = paste0(0, minu)}else{minu = as.character(minu)}
      if(minu == "60"){minu = "59"}
      mtext(paste0(grad,"°", minu, "\' S"), side = 3, adj = 0.95, line = -3.0, cex = 0.60)
      dc <- distCoast(lon = BaseBySet$Lon[1], lat = BaseBySet$Lat[1] )
      mtext(paste0(round(dc,1), " mn"), side = 3, adj = 0.95, line = -4.5, cex = 0.60)
      box()
      
      if(i%%(nr*nc) == 0){
      mtext(text = "Frecuencia (%)", side = 2, outer = TRUE, line = 2.75)
      mtext(text = "Longitud total (cm)", side = 1, outer = TRUE, line = 3)
      }else{
        n = length(ListSetsOrder$VesselSet)
        if(i==n){
        # ix = n %% (n %/% nc)
        # iy = ((n %% (nr*nc)) %/% nc) + as.numeric(ix > 0)
        # par("mgp")
        # at = (1-iy/(2*nr))
        fy = par("fig")[3]+1
        fx = grconvertY(0, from = "npc", to = "lines") - par("oma")[1]
        mtext(text = "Frecuencia (%)", side = 2, outer = TRUE, line = 2.75, at = fy/2)
        mtext(text = "Longitud total (cm)", side = 1, outer = TRUE, line = 3-fx)
        }
        }
      
      tryError <- try(get("show.warnings", envir = .GlobalEnv), TRUE)
      if(class(tryError)=="try-error"){
        show.warnings <- FALSE
      }
      
      if(isTRUE(show.warnings) & sum(BaseBySet$SFreq, na.rm = TRUE) <= Nlim){
      nwarnings <- nwarnings + 1
      usr <- par("usr")
      rect(usr[1], usr[3], usr[2], usr[4], col = rgb(1,0,0,0.2), border = "red")
      if(nwarnings == 1){
        warning("Archivo ", fileFilterHaul, " creado.", call. = FALSE, immediate. = TRUE)
        cat("# Posibles lances a filtar:", fill = TRUE)
        cat("# Lances a filtar:", file = fileFilterHaul, fill = TRUE)
        }
      cat(ac(ListSetsOrder$VesselSet)[i], file = fileFilterHaul,
          fill = TRUE, append = TRUE)
      cat(ac(ListSetsOrder$VesselSet)[i], "\t", sep = "")
      }
    }
    dev.off()
  }
}

if(format == "pdf"){
  nameplot = paste0("FreqBySet_", Name,".pdf")
  nameplot = file.path(dirOut, nameplot)
  pdf(nameplot, width = 8.27, height = 11.69)
  nc <- ncol
  nr <- nrow
  nwarnings <- 0
  par(mfrow = c(nr,nc), mar = c(0,0,0,0), oma = c(4.5,4,1,3))
  for(i in 1:nrow(ListSetsOrder)){
    BaseBySet = BaseSets[which(BaseSets$VesselSet == ListSetsOrder$VesselSet[i]), ]
    x = BaseBySet$Marks
    y = BaseBySet$SFreq/sum(BaseBySet$SFreq, na.rm = T)
    y = y[order(x)]
    x = x[order(x)]
    x0 = seq(1.0, 20, by=0.5)
    y0 = rep(NA, length(x0))
    y0[x0%in%x] = y
    #tryCatch({y0[x0%in%x] = y}, warning=function(w) print(i)) 
    y0 = AddZeros(y0,1)
    juv <- round(100*sum(y[x<12], na.rm = T), digits = 1)
    plot(x0, y0, type = "l", axes = FALSE, xlab = "", ylab = "", xlim = c(1.0,20), ylim = ylim,
         xaxs = "i", yaxs = "i", las = 2, col = 2, lwd = 2)
    
    if(i%%(nr*nc)>((nr-1)*nc) | i%%(nr*nc)==0 | i > (NumSets-nc)){
      axis(1, at = seq(1,20,0.5), labels = NA)
      axis(1, at = seq(2,20,2), labels = seq(2,20,2),  padj = -0.5, cex.axis = 0.60)
    }
    
    if(i%%nc == 1 & i%%2 == 1){
      axis(2, las = 2, cex.axis = 0.70)
    }
    if(i%%nc == 0 & i %%2 == 0)
    {axis(4, las = 2, cex.axis = 0.70)}
      
    abline(v = 12, lty = 2, col = 4)
    mtext(ListSetsOrder$VesselSet[i], side = 3, adj = 0.95, line = -1.5, font = 2, cex = 0.70)
    mtext(paste0(round(juv), "%"," juv"), side = 3, adj = 0.05, line = -1.5, cex = 0.60)
    mtext(paste0("n=", sum(BaseBySet$SFreq, na.rm = TRUE)), side = 3, adj = 0.05, line = -3.0, cex = 0.60)
      
    grad <- floor(-1*BaseBySet$Lat[1])
    minu <- round((-1*BaseBySet$Lat[1])%%1*60)
    if(grad<10){grad = paste0(0, grad)}else{grad = as.character(grad)}
    if(minu<10){minu = paste0(0, minu)}else{minu = as.character(minu)}
    if(minu == "60"){minu = "59"}
    mtext(paste0(grad,"°", minu, "\' S"), side = 3, adj = 0.95, line = -3.0, cex = 0.60)
    dc <- distCoast(lon = BaseBySet$Lon[1], lat = BaseBySet$Lat[1] )
    mtext(paste0(round(dc,1), " mn"), side = 3, adj = 0.95, line = -4.5, cex = 0.60)
    box()
      
    if(i%%(nr*nc) == 0){
      mtext(text = "Frecuencia (%)", side = 2, outer = TRUE, line = 2.75)
      mtext(text = "Longitud total (cm)", side = 1, outer = TRUE, line = 3)
      }else{
      n = length(ListSetsOrder$VesselSet)
      if(i==n){
          # ix = n %% (n %/% nc)
          # iy = ((n %% (nr*nc)) %/% nc) + as.numeric(ix > 0)
          # par("mgp")
          # at = (1-iy/(2*nr))
        fy = par("fig")[3]+1
        fx = grconvertY(0, from = "npc", to = "lines") - par("oma")[1]
        mtext(text = "Frecuencia (%)", side = 2, outer = TRUE, line = 2.75, at = fy/2)
        mtext(text = "Longitud total (cm)", side = 1, outer = TRUE, line = 3-fx)
        }
      }
      
      tryError <- try(get("show.warnings", envir = .GlobalEnv), TRUE)
      if(class(tryError)=="try-error"){
        show.warnings <- FALSE
      }
      
      if(isTRUE(show.warnings) & sum(BaseBySet$SFreq, na.rm = TRUE) <= Nlim){
        nwarnings <- nwarnings + 1
        usr <- par("usr")
        rect(usr[1], usr[3], usr[2], usr[4], col = rgb(1,0,0,0.2), border = "red")
        if(nwarnings == 1){
          warning("Archivo ", fileFilterHaul, " creado.", call. = FALSE, immediate. = TRUE)
          cat("# Posibles lances a filtar:", fill = TRUE)
          cat("# Lances a filtar:", file = fileFilterHaul, fill = TRUE)
        }
        cat(ac(ListSetsOrder$VesselSet)[i], file = fileFilterHaul, fill = TRUE, append = TRUE)
        cat(ac(ListSetsOrder$VesselSet)[i], "\t", sep = "")
      }
    }
    dev.off()
  }

remove(show.warnings, envir = .GlobalEnv)
pdf.options(reset = TRUE)
return(invisible())
}

