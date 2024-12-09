# OL in One AS400

## Overview
This project demonstrates how to expose IBM i PCML files as APIs using OpenLegacy. It includes a Dockerized solution for quick deployment and testing.


## Initial Example

```
# Running it with the provided example
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v ./sources:/usr/app/sources --name ol-n1-pcml openlegacy/oln1pcml:3.0.2.2
```

Example values in the .env files are:
```
HOST=ibmi75.openlegacy.com
PASSWORD=openlega
USER=openlega1
CODE_PAGE=037
SSL_PROPERTIES=false
EXTERNAL_LIBS=RMR2L1
LOGGING_LEVEL_IO_OPENLEGACY="INFO"
```
Inside the sources directory, we have the IBMi Demo PCML files
 
Once running the docker image:
* The OpenLegacy Hub is exposed under `http://localhost:8080`. More information regarding the OpenLegacy Hub is available here: [![OpenLegacy Docs](https://img.shields.io/badge/OpenLegacy-Docs-orange.svg)](https://docs.ol-hub.com)
* The OpenLegacy No Code API is exposed under `http://localhost:8090`.
* You can access its OpenAPI page under `http://localhost:8080/openapi`

---
Going forward with this example:
    
    1. Set your IBMi Connection details inside the `.env` file
    2. Set the PCML files you would like to expose inside the sources directory 
    3. Run - docker run --env-file .env -p 8080:8080 -p 8090:8090 -v ./sources:/usr/app/sources --name ol-n1-pcml openlegacy/oln1pcml:3.0.2.2

---

## Troubleshooting
Ensure Docker is installed and running.

Verify that the IBM i credentials in .env are correct.

Check the logs if the application fails to start:
`docker logs ol-n1-pcml`
