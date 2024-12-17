# OL in One AS400

## Overview
This project demonstrates how to expose IBM i PCML files as APIs using OpenLegacy. It includes a Dockerized solution for quick deployment and testing.


## Initial Example

For unix
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v ./sources:/usr/app/sources --name ol-n1-pcml openlegacy/as400-api:3.0.2.2
```
For Windows Bash
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v /$(pwd)/sources:/usr/app/sources --name ol-n1-pcml openlegacy/as400-api:3.0.2.2
```
For Windows PowerShell
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v .\sources:/usr/app/sources --name ol-n1-pcml openlegacy/as400-api:3.0.2.2
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
* You can access its OpenAPI page under `http://localhost:8090/openapi`

## Testing the API Endpoints

The API endpoints exposed under `http://localhost:8090` using the provided JSON payloads in the `payloads/` directory. The API has two `POST` endpoints: `gactcs6x` and `oactcs6x`.

### Prerequisites

1. Ensure the API is running and accessible at `http://localhost:8090`.
2. Install `curl` or any API testing tool (e.g., Postman).
3. Clone this repository and navigate to the directory containing the payloads:
   ```bash
   git clone https://github.com/<your-username>/ol-in-one-public.git
   cd ol-in-one-public/payloads
---
### Endpoints Overview

| Endpoint | HTTP Method |	Description |
|----------|-------------|--------------| 
| /gactcs6x| POST | Accepts a JSON payload for gactcs6x |
| /oactcs6x| POST| Accepts a JSON payload for oactcs6x.|

**Expected Response:** An HTTP 200 OK response with a JSON payload containing the result.

#### Testing with curl 
```
# Calling gactcs6x
curl -X POST \
  http://localhost:8090/gactcs6x \
  -H "Content-Type: application/json" \
  -d @gactcs6x.json
# Calling oactcs6x
curl -X POST \
  http://localhost:8090/oactcs6x \
  -H "Content-Type: application/json" \
  -d @oactcs6x.json  
```

### Notes
Ensure the payloads are properly formatted and aligned with the OpenAPI spec.

If you have any issues or questions, please open an issue in the repository.

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
