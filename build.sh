#!/usr/bin/env bash
#
# Checks this machine and the deployed directory, then builds the DIANE images.
#
#   ./build.sh --check    checks only, build nothing
#   ./build.sh            server image on the existing base (~1 min)
#   ./build.sh --public   self-contained image, for people who mount nothing
#   ./build.sh --base     base image first (~1 h)
#
# Options: --no-cache, --dir, --data-dir. See usage() below.
# R version and CRAN snapshot live in the Dockerfiles, not here.

set -euo pipefail
cd "$(dirname "$0")"

BASE=diane-base
APP=diane
MIN_FREE_GB=10

usage() {
  cat <<'END'
Usage: ./build.sh [--check] [--base] [--public] [--no-cache] [--dir <path>]

  --check      run the checks and stop. Builds nothing.
  --base       also build the base image (every R dependency, about an hour).
               Needed the first time, and whenever DESCRIPTION changes.
  --public     build the self-contained image instead of the server one. It
               runs DIANE from the package installed inside it, with nothing
               to mount. Tagged with a -public suffix.
  --no-cache   ignore the docker cache.
  --dir        directory mounted on /srv/shiny-server (default: this script's
               own directory). Holds the code.
  --data-dir   directory holding the dataset, when it is mounted separately
               (default: same as --dir). It mirrors the package layout:
               <data-dir>/data/ and <data-dir>/inst/extdata/organisms/.
END
}

build_base=false
check_only=false
target=server
suffix=""
dir="."
data_dir=""
data_dir_given=false
# Plain string, expanded unquoted: an empty "${arr[@]}" trips `set -u`.
extra=""
while [ $# -gt 0 ]; do
  case "$1" in
    --check)    check_only=true ;;
    --base)     build_base=true ;;
    --public)   target=public; suffix="-public" ;;
    --no-cache) extra="--no-cache" ;;
    --dir)      shift; dir="${1:?--dir expects a path}" ;;
    --data-dir) shift; data_dir="${1:?--data-dir expects a path}"; data_dir_given=true ;;
    -h|--help)  usage; exit 0 ;;
    *) echo "unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

version=$(awk '/^Version:/ {print $2; exit}' DESCRIPTION)
day=$(date +%Y-%m-%d)
stamp=$(date +"%Y-%m-%d %H:%M:%S %z")
# Read from Dockerfile.base, never restated here.
base_from=$(awk -F= '/^ARG R_VERSION=/ {print "rocker/shiny:" $2; exit}' Dockerfile.base)

if git rev-parse --git-dir >/dev/null 2>&1; then
  sha=$(git rev-parse --short HEAD)
  git diff --quiet HEAD -- || sha="$sha+modified"
else
  sha=no-git
fi

# Mounted from elsewhere, or sitting with the code by default.
data_dir="${data_dir:-$dir}"

echo "DIANE $version — git $sha — $stamp"
echo "Code directory: $(cd "$dir" && pwd)"
echo "Dataset       : $(cd "$data_dir" && pwd)"
echo

#   ____________________________________________________________________________
#   Checks                                                                  ####

failed=0
ko() { echo "  FAIL    $*" >&2; failed=1; }
ok() { echo "  ok      $*"; }

echo "== Checks that need no container =="

docker_root=$(docker info --format '{{.DockerRootDir}}')
free_gb=$(df -BG --output=avail "$docker_root" 2>/dev/null | tail -1 | tr -dc '0-9')
if [ "${free_gb:-0}" -lt "$MIN_FREE_GB" ]; then
  ko "$free_gb GB free on $docker_root, at least $MIN_FREE_GB needed"
else
  ok "$free_gb GB free on $docker_root"
fi

# An .Rprofile in the served directory redirects .libPaths(): nothing starts.
if [ -e "$dir/.Rprofile" ]; then
  ko "$dir/.Rprofile found — remove it, or no session will ever start"
else
  ok "no .Rprofile"
fi
if [ -d "$dir/renv" ]; then
  ko "$dir/renv/ found — remove it"
else
  ok "no renv/"
fi

if [ -d "$dir/logs" ]; then
  ok "logs/ present"
else
  ko "$dir/logs/ missing — the app writes its session counter there"
fi

# Delivery check, not a build precondition: an image builds with no data.
RDA="organisms_index abiotic_stresses gene_annotations regulators_per_organism"

if [ -e "$data_dir/data/organisms.rda" ]; then
  ko "data/organisms.rda found — dataset still in the 1.2.1 layout"
else
  ok "no stale data/organisms.rda"
fi

# Plain ifs: under `set -e`, a false `[ ] && cmd` ends the script.
found=0
for f in $RDA; do
  if [ -e "$data_dir/data/$f.rda" ]; then found=$((found + 1)); fi
done
# Hand-counted glob: under `pipefail`, `ls | wc -l` fails on a missing or
# empty directory, and `set -e` then kills the script silently.
n_rds=0
for f in "$data_dir"/inst/extdata/organisms/*.rds; do
  if [ -e "$f" ]; then n_rds=$((n_rds + 1)); fi
done
if [ "$n_rds" -gt 0 ]; then found=$((found + 1)); fi

if ! $data_dir_given && [ "$found" -eq 0 ]; then
  # Mounted at run time. Say it once, do not fail a build over it.
  echo "  --      no dataset in this directory; it is expected to come from a mount."
  echo "          Pass --data-dir <path> to check the dataset that will be mounted."
else
  # Pointed at, or half here: both deserve the full check.
  for f in $RDA; do
    if [ -e "$data_dir/data/$f.rda" ]; then
      ok "data/$f.rda present"
    else
      ko "data/$f.rda missing — the dataset must be a superset of the package data"
    fi
  done
  if [ "$n_rds" -gt 0 ]; then
    ok "$n_rds organism file(s)"
  else
    ko "no .rds under inst/extdata/organisms/"
  fi
fi

echo
echo "== Checks that run inside a container ($base_from) =="
echo "   Docker's network is not the shell's: the probes run in a container."
if docker pull -q "$base_from" >/dev/null; then
  ok "Docker Hub reachable, $base_from available"
else
  ko "Docker Hub unreachable, or tag $base_from missing"
fi

if docker image inspect "$base_from" >/dev/null 2>&1; then
  # Read the account from the image, never assume it: shiny-server does `su shiny`.
  app_ids=$(docker run --rm --entrypoint sh "$base_from" -c 'id -u shiny; id -g shiny' 2>/dev/null || true)
  app_uid=$(echo "$app_ids" | sed -n 1p)
  app_gid=$(echo "$app_ids" | sed -n 2p)
  if [ -z "$app_uid" ] || [ -z "$app_gid" ]; then
    ko "no shiny account in $base_from — cannot check what the app will be allowed to read"
    app_uid=0; app_gid=0
  else
    ok "app runs as shiny, uid $app_uid gid $app_gid"
  fi
  if docker run --rm --entrypoint bash "$base_from" -c 'apt-get update -qq >/dev/null 2>&1'; then
    ok "apt mirrors"
  else
    ko "apt mirrors unreachable"
  fi

  # Probed from R: the image ships no curl, and this is the libcurl stack
  # install.packages() will use, R proxy settings included.
  if ! docker run --rm --entrypoint Rscript "$base_from" -e '
      probe <- function(u) tryCatch({
          con <- url(u, "rb"); on.exit(close(con)); readBin(con, "raw", 1L); TRUE
        }, error = function(e) grepl("HTTP status", conditionMessage(e)))
      hosts <- c("https://packagemanager.posit.co/", "https://bioconductor.org/",
                 "https://api.github.com/", "https://codeload.github.com/")
      down <- FALSE
      for (u in hosts) {
        up <- probe(u); down <- down || !up
        cat(if (up) "  ok      " else "  FAIL    ", u, "\n", sep = "")
      }
      if (down) quit(status = 1)'; then
    ko "at least one host is unreachable from a container"
  fi

  # The app runs as shiny, not as you. Data it cannot read surfaces as
  # "not an exported object", which points nowhere near the cause.
  if [ "$found" -gt 0 ]; then
    if docker run --rm -u "$app_uid:$app_gid" \
         -v "$(cd "$data_dir" && pwd)/data":/d:ro \
         -v "$(cd "$data_dir" && pwd)/inst/extdata/organisms":/o:ro \
         --entrypoint bash "$base_from" \
         -c '[ -n "$(ls -A /d 2>/dev/null)" ] && [ -n "$(ls -A /o 2>/dev/null)" ]' 2>/dev/null; then
      ok "dataset readable by $app_uid:$app_gid"
    else
      ko "dataset unreadable by $app_uid:$app_gid — run: chmod -R a+rX $data_dir"
    fi
  fi

  # shiny-server writes to logs/ as the shiny user.
  if [ -d "$dir/logs" ]; then
    if docker run --rm -u "$app_uid:$app_gid" -v "$(cd "$dir" && pwd)/logs":/logs "$base_from" \
         bash -c 'touch /logs/.diane-probe && rm -f /logs/.diane-probe' 2>/dev/null; then
      ok "logs/ writable by $app_uid:$app_gid"
    else
      ko "logs/ unwritable by $app_uid:$app_gid — run: chown -R $app_uid:$app_gid $dir/logs"
    fi
  fi
fi

echo
if [ "$failed" -ne 0 ]; then
  echo "Checks failed. Nothing was built." >&2
  exit 1
fi
echo "Checks passed."

if $check_only; then
  exit 0
fi

#   ____________________________________________________________________________
#   Build                                                                   ####

echo
if $build_base; then
  echo "== Base $BASE:$day — expect about an hour =="
  docker build $extra -f Dockerfile.base \
    -t "$BASE:$day" -t "$BASE:latest" .
  echo
elif ! docker image inspect "$BASE:latest" >/dev/null 2>&1; then
  echo "Base image $BASE:latest does not exist on this machine." >&2
  echo "Run first:  ./build.sh --base" >&2
  exit 1
fi

img="$APP:$version$suffix"

echo "== Application image $img ($target) =="
docker build $extra -f Dockerfile.app \
  --target "$target" \
  --build-arg BASE_TAG="$BASE:latest" \
  --build-arg DIANE_VERSION="$version" \
  --build-arg GIT_SHA="$sha" \
  --build-arg BUILD_DATE="$stamp" \
  -t "$img" \
  -t "$img-$day" .

# Does every organism in the index have its file? The defect that survives a
# copy between machines.
echo
if [ "$found" -eq 0 ]; then
  echo "== Dataset integrity: skipped, no dataset to check =="
  echo "   Re-run with --data-dir once the dataset directory exists."
else
echo "== Dataset integrity =="
docker run --rm --entrypoint Rscript \
  -v "$(cd "$dir" && pwd)":/srv/shiny-server:ro \
  -v "$(cd "$data_dir" && pwd)/data":/srv/shiny-server/data:ro \
  -v "$(cd "$data_dir" && pwd)/inst/extdata/organisms":/srv/shiny-server/inst/extdata/organisms:ro \
  "$img" -e '
  e <- new.env(); load("/srv/shiny-server/data/organisms_index.rda", envir = e)
  idx <- e$organisms_index
  slugs <- vapply(idx, function(o) as.character(o[["slug"]]), "")
  f <- file.path("/srv/shiny-server/inst/extdata/organisms", paste0(slugs, ".rds"))
  cat("  ", length(slugs), "organisms in the index,", sum(file.exists(f)), "files present\n")
  if (any(!file.exists(f))) {
    cat("   MISSING:", paste(slugs[!file.exists(f)], collapse = ", "), "\n")
    quit(status = 1)
  }' || { echo "Integrity check failed." >&2; exit 1; }
fi

if [ "$found" -gt 0 ]; then
  data_abs="$(cd "$data_dir" && pwd)"
else
  data_abs="<dataset-dir>"   # not created yet: fill it in before running this
fi

echo
echo "== What the image contains =="
docker run --rm --entrypoint cat "$img" /etc/diane-versions.txt

cat <<END

Images built: $img  and  $img-$day
Full package list:
  docker run --rm --entrypoint cat $img /etc/diane-packages.csv
END

if [ "$target" = public ]; then
  cat <<END

STILL TO CHECK BY HAND — nothing above covers any of it:
  1. Start the image, then open http://localhost:8087 :
     docker run --rm -p 8087:8086 $img
  2. Confirm the bundled organisms show up in the import tab.
  3. Run one network inference to completion. That is the only test of
     compatibility with the base image's Bioconductor release, which a
     successful build does not prove.
END
else
  cat <<END

STILL TO CHECK BY HAND — nothing above covers any of it:
  1. Start the image and open the application, mounted as ShinyProxy will:
     docker run --rm -p 8087:8086 \
       -v $(cd "$dir" && pwd):/srv/shiny-server \
       -v $data_abs/data:/srv/shiny-server/data \
       -v $data_abs/inst/extdata/organisms:/srv/shiny-server/inst/extdata/organisms \
       $img
  2. Confirm the production organisms show up in the import tab.
  3. Run one network inference to completion. That is the only test of
     compatibility with the base image's Bioconductor release, which a
     successful build does not prove.

Then: add a new entry to ShinyProxy's application.yml pointing at $img,
next to the existing one. Only remove the old entry once this one works.
END
fi
