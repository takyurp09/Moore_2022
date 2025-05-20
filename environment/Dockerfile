# hash:sha256:c7e2369fde41d5bbab69cb309c62e66283b3016707e0569890afee30e0f425ff
FROM registry.codeocean.com/codeocean/r-base:3.6.3-ubuntu18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN Rscript -e 'remotes::install_version("EnvStats", "2.4.0")' \
    && Rscript -e 'remotes::install_version("data.table", "1.14.2")' \
    && Rscript -e 'remotes::install_version("forcats", "0.5.1")' \
    && Rscript -e 'remotes::install_version("ggplot2", "3.3.5")' \
    && Rscript -e 'remotes::install_version("metR", "0.10.0")' \
    && Rscript -e 'remotes::install_version("patchwork", "1.1.1")' \
    && Rscript -e 'remotes::install_version("plot.matrix", "1.6")' \
    && Rscript -e 'remotes::install_version("randomForest", "4.6-14")' \
    && Rscript -e 'remotes::install_version("randomForestExplainer", "0.10.1")' \
    && Rscript -e 'remotes::install_version("reshape2", "1.4.4")' \
    && Rscript -e 'remotes::install_version("shiny", "1.6.0")' \
    && Rscript -e 'remotes::install_version("tidyverse", "1.3.1")'
