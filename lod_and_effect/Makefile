all: lod_and_effect.js weightlod.json

lod_and_effect.js: lod_and_effect.coffee
	coffee -c lod_and_effect.coffee

weightlod.json: R/grab_data.R
	cd R;R CMD BATCH grab_data.R
