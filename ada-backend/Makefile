# Makefile for Ada Rinha Backend Project
BINARY_NAME := rinha_backend
SOURCE_DIR := src
BUILD_DIR := bin
TEST_DIR := tests

.PHONY: all build clean docker test test-unit test-integration test-load

all: build

build:
	@echo "Building Ada project with Alire..."
	alr build --release
	@echo "✅ Build complete!"

down:
	@echo "Stopping and removing Docker containers..."
	docker-compose down
	@echo "✅ Docker containers stopped and removed!"

up:
	@echo "Starting Docker containers..."
	docker-compose up -d --build --remove-orphans -f docker-compose.mocks.yml
	@echo "✅ Docker containers started!"
clean:
	@echo "Cleaning build artifacts..."
	alr clean
	@echo "✅ Clean complete!"

docker:
	@echo "Building Docker image..."
	docker build -t rinha-ada-backend:latest .
	@echo "✅ Docker image built!"

test: test-all

test-all:
	@echo "Running all tests..."
	cd $(TEST_DIR) && ./run_tests.sh all
	@echo "✅ All tests complete!"

test-unit:
	@echo "Running unit tests..."
	cd $(TEST_DIR) && ./run_tests.sh unit
	@echo "✅ Unit tests complete!"

test-integration:
	@echo "Running integration tests..."
	cd $(TEST_DIR) && ./run_tests.sh integration
	@echo "✅ Integration tests complete!"

test-load:
	@echo "Running load tests..."
	cd $(TEST_DIR) && ./run_tests.sh load
	@echo "✅ Load tests complete!"

run: build
	@echo "Running Ada backend..."
	./bin/$(BINARY_NAME)

install-deps:
	@echo "Installing Ada dependencies..."
	alr get
	@echo "✅ Dependencies installed!"

format:
	@echo "Formatting Ada code..."
	find $(SOURCE_DIR) -name "*.ads" -o -name "*.adb" | xargs -r gnatpp -P rinha_backend.gpr
	@echo "✅ Code formatted!"

help:
	@echo "Available targets:"
	@echo "  build         - Build the Ada project"
	@echo "  clean         - Clean build artifacts"
	@echo "  docker        - Build Docker image"
	@echo "  test          - Run all tests"
	@echo "  test-unit     - Run unit tests only"
	@echo "  test-integration - Run integration tests only"
	@echo "  test-load     - Run load tests only"
	@echo "  run           - Build and run the application"
	@echo "  install-deps  - Install project dependencies"
	@echo "  format        - Format source code"
	@echo "  help          - Show this help message"
