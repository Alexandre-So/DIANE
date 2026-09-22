## Use DIANE locally

To use DIANE locally, download and install DIANE in your R console as follows (you need the remotes package installed) :

```r
remotes::install_github("Alexandre-So/DIANE")
```

DIANE is built and tested on R 4.6.1, available for all OS at <https://cloud.r-project.org/>.

You can then launch the application :

```r
library(DIANE)
DIANE::run_app()
```

------------------------------------------------------------------------

## License

Copyright (C) 2020 Oceane Cassan

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.


------------------------------------------------------------------------

## Deploy DIANE on your server

DIANE can be deployed on any linux server with Docker, as it is at <https://diane.ipsim.inrae.fr>. The instructions are kept up to date in the repository : <https://github.com/Alexandre-So/DIANE#deploy-diane-with-docker>

------------------------------------------------------------------------

Authors : Océane Cassan, Antoine Martin, Sophie Lèbre

Dev : Océane Cassan, PhD Student at IPSIM (Institute for Plant Sciences in Montpellier) research unit, SUPAGRO Montpellier, with contributions from Alexandre Soriano.

The application is now maintained by Alexandre Soriano (CIRAD)
