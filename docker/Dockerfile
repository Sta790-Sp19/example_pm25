FROM rocker/tidyverse:3.5.2

RUN apt-get -y install libtcl8.6 libtk8.6 \
  && install2.r --deps TRUE \
    spBayes \
  && mkdir /work

WORKDIR /work

ENTRYPOINT ["Rscript"]
