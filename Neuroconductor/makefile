all: Neuroconductor.pdf Neuroconductor.R
    
Neuroconductor.pdf: Neuroconductor.Rmd 
	Rscript -e "rmarkdown::render('Neuroconductor.Rmd')"
	open Neuroconductor.pdf

Neuroconductor.R: Neuroconductor.Rmd
	Rscript -e "knitr::purl('Neuroconductor.Rmd')"

clean: 
	rm Neuroconductor.pdf Neuroconductor.R

