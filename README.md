# Pfennig

## Dependencies

- PostgreSQL server version 10.*

## Database migrations

### Create new migration

- Run `database/create-migration.sh <migration-title>` in your shell

### Run migrations

- Create a database if you didn't already (expected default name: `pfennig`):
  - `create database if not exists pfennig;`
- Run `database/run-migrations.sh` in your shell
  Environment parameters:
  - `PG_USER` (default: `postgres`)
  - `PG_DATABASE` (default: `pfennig`)
