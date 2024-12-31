# OL in One Reverse API REST

## Overview
This project demonstrates how to REST service from the Legacy(Mainframe Or IBMi) using OpenLegacy Reverse API acting as a Proxy server.

It includes a Dockerized solution for quick deployment and testing.


## Initial Example

For unix
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v ./sources:/home/ubuntu/sources --name ol-n1-reverse-api-rest openlegacy/reverse-api-rest:latest
```
For Windows Bash
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v /$(pwd)/sources:/home/ubuntu/sources --name ol-n1-reverse-api-rest openlegacy/reverse-api-rest:latest
```
For Windows PowerShell
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v .\sources:/home/ubuntu/sources --name ol-n1-reverse-api-rest openlegacy/reverse-api-rest:latest
```

Example values in the .env files are:
```
HOST=https://api.api-ninjas.com/
CLIENT_TYPE=MF_CICS_COBOL
HUB_LOGGING_LEVEL=error
```
Inside the sources directory, we have the OpenAPI Spec file.
The OpenAPI spec we are using is based on the following endpoints:
* https://api-ninjas.com/api/iban
* https://api-ninjas.com/api/inflation
* https://api-ninjas.com/api/salestax
* https://api-ninjas.com/api/swiftcode

To use these endpoints you need to have an API Key
[API Ninja](https://api-ninjas.com/tos)

Once running the docker image:
* The OpenLegacy Hub is exposed under `http://localhost:8080`. More information regarding the OpenLegacy Hub is available here: [![OpenLegacy Docs](https://img.shields.io/badge/OpenLegacy-Docs-orange.svg)](https://docs.ol-hub.com)
* The OpenLegacy No Code API is exposed under `http://localhost:8090`.
* You can access its OpenAPI page under `http://localhost:8090/openapi`

## Testing the API Endpoints

The API endpoints exposed under `http://localhost:8090` using the provided JSON payloads in the `payloads/` directory. The API has three `POST` endpoints: 
* `iban`
* `inflation`
* `salestax`
* `swiftcode`

### Prerequisites

1. Ensure the API is running and accessible at `http://localhost:8090`.
2. Install `curl` or any API testing tool (e.g., Postman).
3. Clone this repository and navigate to the directory containing the payloads:
   ```bash
   git clone https://github.com/openlegacy/ol-in-one-public.git
   cd ol-in-one-public/ol-in-one-reverse-api-rest/payloads
   ```
### Endpoints Overview

| Endpoint | HTTP Method |	Description |
|----------|-------------|--------------| 
| /iban| POST | Accepts a octet-stream payload for iban.bin. |
| /inflation| POST| Accepts a octet-stream payload for inflation.bin. |
| /salestax| POST| Accepts a octet-stream payload for salestax.bin.|
| /swiftcode| POST| Accepts a octet-stream payload for swiftcode.bin.|


**Expected Response:** An HTTP 200 OK response with a octet-stream payload containing the result.

#### Testing with curl 

* Calling iban
```
curl -X POST \
  http://localhost:8090/iban' \
  -H 'accept: application/octet-stream' \
  -d @iban.bin
```
* Calling inflation
```
curl -X POST \
  http://localhost:8090/inflation' \
  -H 'accept: application/octet-stream' \
  -d @inflation.bin
```  
* Calling salestax
```
curl -X POST \
  http://localhost:8090/salestax' \
  -H 'accept: application/octet-stream' \
  -d @salestax.bin 
```
* Calling swiftcode
```
curl -X POST \
  http://localhost:8090/swiftcode' \
  -H 'accept: application/octet-stream' \
  -d @swiftcode.bin 
```
---
### Notes
Ensure the payloads are properly formatted and aligned with the OpenAPI spec.

If you have any issues or questions, please open an issue in the repository.

---
Going forward with this example:
    
- Set your REST Connection details inside the `.env` file
- Set the OpenAPI.json file you would like to expose inside the sources directory 


---
## Troubleshooting
Ensure Docker is installed and running.

Verify that the REST URL in .env is correct.

Check the logs if the application fails to start:
`docker logs ol-n1-reverse-api-rest`


