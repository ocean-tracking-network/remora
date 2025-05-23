% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_expert_distribution_shp_CAAB_qc.R
\name{get_expert_distribution_shp_CAAB}
\alias{get_expert_distribution_shp_CAAB}
\title{match species CAAB code with CSIRO expert distribution shapefiles}
\usage{
get_expert_distribution_shp_CAAB(CAAB_species_id, spe)
}
\arguments{
\item{CAAB_species_id}{CAAB id of species for which a distribution shapefile
is required}

\item{spe}{species scientific name}
}
\value{
shp is a multipolygon sf data.frame object of the species' distribution
}
\description{
Get expert distribution shapefile, if available, from CSIRO's
Geoserver. Full CAAB code list:
http://www.marine.csiro.au/datacentre/caab/caab_dump_latest.xlsx
}
\details{
For a few species acoustically tagged no shapefile exists.
}
\examples{
# example code
x <- TownsvilleReefQC$QC[[1]]
expert_shp <- get_expert_distribution_shp_CAAB(CAAB_species_id = x$CAAB_species_id[1], 
spe = x$species_scientific_name[1])

}
