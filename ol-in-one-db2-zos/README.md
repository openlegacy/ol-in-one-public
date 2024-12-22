# OL in One DB2 z/OS

## Overview
This project demonstrates how to expose IBM DB2 z/OS SQL as APIs using OpenLegacy. It includes a Dockerized solution for quick deployment and testing.


## Initial Example

For unix
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v ./sources:/usr/app/sources v ./db2jars:/home/ubuntu/lib --name ol-n1-db2zos openlegacy/db2-zos-api:3.0.2.2
```
For Windows Bash
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v /$(pwd)/sources:/usr/app/sources -v /$(pwd)/db2jars:/home/ubuntu/lib --name ol-n1-db2zos openlegacy/db2-zos-api:3.0.2.2
```
For Windows PowerShell
```
docker run --env-file .env -p 8080:8080 -p 8090:8090 -v .\sources:/usr/app/sources -v .\db2jars:/home/ubuntu/lib --name ol-n1-db2zos openlegacy/db2-zos-api:3.0.2.2
```

Example values in the .env files are:
```
HOST=jdbc:db2://mainframe.openlegacy.com:5045/DALLASD
USER=DEMO001
PASSWORD=LEGACY
LOGGING_LEVEL_IO_OPENLEGACY="INFO"
```
Inside the sources directory, we have the sql files

Mount the db2jars into the container, there should be two jar files:
* **db2jcc4.jar**
* **db2jcc_license_cisuz.jar**
 
Once running the docker image:
* The OpenLegacy Hub is exposed under `http://localhost:8080`. More information regarding the OpenLegacy Hub is available here: [![OpenLegacy Docs](https://img.shields.io/badge/OpenLegacy-Docs-orange.svg)](https://docs.ol-hub.com)
* The OpenLegacy No Code API is exposed under `http://localhost:8090`.
* You can access its OpenAPI page under `http://localhost:8090/openapi`

## Testing the API Endpoints

The API endpoints exposed under `http://localhost:8090` using the provided JSON payloads in the `payloads/` directory. The API has three `POST` endpoints: 
* `getaccounts`
* `demo001-lactsql`
* `demo001-gactsql`

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
| /getaccounts| POST | Accepts Empty Payload. |
| /demo001-lactsql| POST| Accepts Empty Payload. |
| /demo001-gactsql| POST| Accepts a JSON payload for demo001-gactsql.|

**Expected Response:** An HTTP 200 OK response with a JSON payload containing the result.

#### Testing with curl 

* Calling getaccounts
```
curl -X POST \
  http://localhost:8090/getaccounts' \
  -H 'accept: application/json' \
  -d ''
```
* Calling demo001-lactsql
```
curl -X POST \
  http://localhost:8090/demo001-lactsql \
  -H "Content-Type: application/json" \
  -d ''
```  
* Calling demo001-gactsql
```
curl -X POST \
  http://localhost:8090/demo001-gactsql \
  -H "Content-Type: application/json" \
  -d @demo001-gactsql.json  
```

### Notes
Ensure the payloads are properly formatted and aligned with the OpenAPI spec.

If you have any issues or questions, please open an issue in the repository.

---
Going forward with this example:
    
- Set your DB2 Zos Connection details inside the `.env` file
- Set the SQL files you would like to expose inside the sources directory 
  - Create a directory called `stored-procedures` add to it:
          - Files called the following way `:schemaName:.:packageName:.:SP:.sql`
          - The content will be the SQL statement that invokes the stored procedure
       - Create a directory called `sql` add to it:
          - Files called the following way `:Name:.sql`
          - The content will be the SQL statement you would like to expose
- Run:  
  ```
  docker run --env-file .env -p 8080:8080 -p 8090:8090 -v ./sources:/usr/app/sources v ./db2jars:/home/ubuntu/lib --name ol-n1-db2zos openlegacy/db2-zos-api:3.0.2.2
  ```

---

## Troubleshooting
Ensure Docker is installed and running.

Verify that the DB2 credentials in .env are correct.

Check the logs if the application fails to start:
`docker logs ol-n1-db2zos`

Make sure you have the right DB2 driver and license jars mounted.
