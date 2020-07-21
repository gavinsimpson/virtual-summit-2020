all: slides purl copy

slides: simpson-aquatic-virtual-summit.Rmd slides.css
	Rscript -e "rmarkdown::render(\"simpson-aquatic-virtual-summit.Rmd\")"

purl: simpson-aquatic-virtual-summit.Rmd
	Rscript -e "knitr::purl(\"simpson-aquatic-virtual-summit.Rmd\")"

copy: simpson-aquatic-virtual-summit.html slides.css macros.js
	cp -R -u simpson-aquatic-virtual-summit_files simpson-aquatic-virtual-summit.html macros.js slides.css libs resources ~/work/web/jekyll/blog/slides/simpson-aquatic-virtual-summit-2020/
