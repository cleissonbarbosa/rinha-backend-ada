Ada implementation (1:1 architecture) of the Rinha de Backend 2025 backend + custom load balancer.

Endpoints:
- POST /payments: forwards via UDP to one backend; backend calls processors (default with retry, then fallback).
- GET /payments-summary: LB asks a backend; backend merges with peer via UDP and responds to LB; LB returns JSON.

Build: uses gprbuild and AWS (Ada Web Server) in Docker.
