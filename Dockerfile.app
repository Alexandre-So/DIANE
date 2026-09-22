# Application image: the base plus DIANE.
#
# Two variants, built from the same shared stage:
#   ./build.sh            server — serves the code mounted on /srv/shiny-server,
#                         the way ShinyProxy runs it here.
#   ./build.sh --public   public — runs on its own, from the package installed
#                         below. The one to hand to someone else.

ARG BASE_TAG=diane-base:latest
FROM ${BASE_TAG} AS common

ARG DIANE_VERSION=unknown
ARG GIT_SHA=unknown
ARG BUILD_DATE=unknown

# Explicit, so that no file from the repository is read when R starts.
WORKDIR /

COPY . /build
RUN R -q -e 'remotes::install_local("/build", upgrade = "never")' \
 && rm -rf /build

RUN printf '%s\n' \
      "DIANE       : ${DIANE_VERSION}" \
      "git         : ${GIT_SHA}" \
      "image built : ${BUILD_DATE}" \
      >> /etc/diane-versions.txt

LABEL org.opencontainers.image.title="DIANE" \
      org.opencontainers.image.version="${DIANE_VERSION}" \
      org.opencontainers.image.revision="${GIT_SHA}" \
      org.opencontainers.image.created="${BUILD_DATE}"

# Self-contained. server_version keeps its default, so the app writes no shared
# session log and needs no logs/ directory.
FROM common AS public
EXPOSE 8086
CMD ["R", "-e", "options(shiny.port = 8086); DIANE::run_app(host = '0.0.0.0')"]

# With no mount, shiny-server shows an empty index: a missing mount stays
# visible. Last on purpose, so a `docker build` with no --target lands here,
# where a mistake shows, rather than on public, where it would be silent.
FROM common AS server
COPY shiny-customized.config /etc/shiny-server/shiny-server.conf
EXPOSE 8086
CMD ["/usr/bin/shiny-server"]
