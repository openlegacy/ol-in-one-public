# OL in One CTG

## Overview
This project demonstrates how to expose Mainframe CTG COBOL files as APIs using OpenLegacy. It includes a Dockerized solution for quick deployment and testing.


## Initial Example

For unix
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v ./sources:/usr/app/sources --name ol-n1-ctg openlegacy/ctg-api:3.0.2.2
```
For Windows Bash
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v /$(pwd)/sources:/usr/app/sources --name ol-n1-ctg openlegacy/ctg-api:3.0.2.2
```
For Windows PowerShell
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v .\sources:/usr/app/sources --name ol-n1-ctg openlegacy/ctg-api:3.0.2.2
```

Example values in the .env files are:
```
HOST_NAME=mainframe.openlegacy.com
PORT=13200
CODE_PAGE=CP037
SSL_PROPERTIES=false
DEFAULT_IPIC_SERVER=IPCSSL
TRANSACTION_ID=OC2D
LOGGING_LEVEL_IO_OPENLEGACY=INFO
```
Inside the sources directory, we have the Mainframe CTG Demo COBOL files
 
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
   git clone https://github.com/openlegacy/ol-in-one-public.git
   cd ol-in-one-public/ol-in-one-ctg/payloads
   ```
---
### Endpoints Overview

| Endpoint | HTTP Method |	Description |
|---------|-------------|--------------| 
| /gactcs9| POST | Accepts a JSON payload for gactcs9 |
| /fininq2| POST| Accepts a JSON payload for fininq2.|
| /lactcs9| POST| Accepts a JSON payload for lactcs9.|

**Expected Response:** An HTTP 200 OK response with a JSON payload containing the result.

#### Testing with curl 
```
# Calling gactcs9
curl -X 'POST' \
  'http://localhost:8090/gactcs9' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d @gactcs9.json
  
# Calling fininq2
curl -X 'POST' \
  'http://localhost:8090/fininq2' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d @fininq2.json
  
# Calling lactcs9
curl -X 'POST' \
  'http://localhost:8090/lactcs9' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d @lactcs9.json
```

### Notes
Ensure the payloads are properly formatted and aligned with the OpenAPI spec.

If you have any issues or questions, please open an issue in the repository.

---
Going forward with this example:
    
    1. Set your Mainframe CTG Connection details inside the `.env` file
    2. Set the COBOL (.cbl, / cpy) files you would like to expose inside the sources directory 
    3. Run - docker run --env-file .env -p 8080:8080 -p 8090:8090 -v ./sources:/usr/app/sources --name ol-n1-ctg openlegacy/oln1ctg:3.0.2.2

---

## Troubleshooting
Ensure Docker is installed and running.

Verify that the Mainframe CTG credentials in .env are correct.

Check the logs if the application fails to start:
`docker logs ol-n1-ctg`
