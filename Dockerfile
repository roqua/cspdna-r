# Full tag, so we don't accidentally go up an R version.
FROM opencpu/ubuntu-22.04:v2.2.11-2 as builder

WORKDIR /app

RUN apt-get update && apt-get -f install -y \
  fonts-liberation \
  git \
  libcairo-dev \
  libcurl4-openssl-dev \
  libxml2-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libtiff-dev \
  libjpeg-dev

ADD ./inst/bash/install-package-dependencies.sh /app/inst/bash/install-package-dependencies.sh
ADD ./packrat/packrat.lock ./packrat/packrat.opts ./packrat/init.R /app/packrat/
RUN ./inst/bash/install-package-dependencies.sh
# Add packrat libs to opencpu search path
ADD ./inst/opencpu_Rprofile /etc/opencpu/Rprofile
# ADD ./inst/csp_dna.conf /etc/opencpu/server.conf.d/csp_dna.conf
RUN echo "Mutex posixsem" >> /etc/apache2/apache2.conf

RUN mkdir -p /app/cspdna.Rcheck/tests/ && ln -sf /proc/self/fd/1 /app/cspdna.Rcheck/tests/testthat.Rout

ADD ./ /app

RUN R --no-save --quiet -e 'devtools::document()'
RUN R CMD INSTALL --no-multiarch --with-keep.source /app
RUN R CMD build /app
CMD R CMD check --no-clean /app/`ls *.gz | tail -1` --no-manual --no-build-vignettes

FROM builder as debug
FROM builder as release
