# Explore a model ensemble with GGobi
#
# @keyword dynamic
# @keyword regression
ggobi.ensemble <- function(data, original=NULL, ...) {
	g <- ggobi()
	g["model"] <- summary(data)
	
	c <- coef(data)
	g["model-variable"] <- c
	glyph_type(g["model-variable"]) <- ifelse(c$raw == 0, 1, 6)
	g <- ggobi_longitudinal(c, id = model, g=g)
	g["variable"] <- summary(c)
	
	r <- resid(data)
	g["model-data"] <- r
	g["data"] <- summary(r, original)
	
	invisible(g)
}