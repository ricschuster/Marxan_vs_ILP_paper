prepare_marxan_data <- function(cost, features, target, spf) {
  stopifnot(inherits(cost, "RasterLayer"))
  stopifnot(inherits(features, "Raster"))
  stopifnot(is.numeric(target), 
            length(target) %in% c(1, raster::nlayers(features)))
  stopifnot(is.numeric(spf), length(spf) %in% c(1, raster::nlayers(features)))
  
  # data
  included <- raster::Which(!is.na(cost), cells = TRUE)
  m_pu <- data.frame(id = included,
                     cost = cost[included],
                     status = 0L) %>% 
    filter(!is.na(cost))
  total_rep <- unname(raster::cellStats(features, sum, asSample = FALSE))
  m_species <- data.frame(id = seq_len(raster::nlayers(features)),
                          target = target * total_rep,
                          spf = spf,
                          name = names(features),
                          stringsAsFactors = FALSE)
  m_puvspecies <- features[included] %>% 
    as.data.frame() %>% 
    set_names(seq_len(ncol(.))) %>% 
    mutate(pu = included) %>% 
    gather(species, amount, -pu, convert = TRUE) %>% 
    select(species, pu, amount) %>% 
    arrange(pu, species)
  marxan::MarxanData(pu = m_pu, species = m_species, 
                     puvspecies = m_puvspecies,
                     boundary = NULL)
}