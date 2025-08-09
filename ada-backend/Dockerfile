FROM ubuntu:22.04

WORKDIR /app

# Install runtime dependencies only
RUN apt-get update && apt-get install -y \
    postgresql-client \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy pre-compiled binary
COPY bin/rinha ./rinha
RUN chmod +x ./rinha

# Create runtime user
RUN useradd -r -s /bin/false appuser || true
RUN chown -R appuser:appuser /app || true
USER appuser

# Expose port
EXPOSE 8080

# Run the application
CMD ["./rinha"]
