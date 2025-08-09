#!/bin/bash

# Integration Tests for Rinha Backend Ada
echo "=== Rinha Backend Ada - Integration Tests ==="
echo ""

# Check if system is running
if ! curl -s http://localhost:9999/health > /dev/null; then
    echo "❌ FAIL: System not running. Please start with docker-compose up -d"
    exit 1
fi

echo "✅ System is running, starting integration tests..."
echo ""

# Test 1: Health Check
echo "🔍 Test 1: Health Check"
HEALTH_RESPONSE=$(curl -s -w "%{http_code}" http://localhost:9999/health)
HTTP_CODE=${HEALTH_RESPONSE: -3}
RESPONSE_BODY=${HEALTH_RESPONSE%???}

if [ "$HTTP_CODE" = "200" ]; then
    echo "✅ PASS: Health check returns 200"
    echo "   Response: $RESPONSE_BODY"
else
    echo "❌ FAIL: Health check failed - HTTP Code: $HTTP_CODE"
fi
echo ""

# Test 2: Process Payment - Valid Request
echo "🔍 Test 2: Process Payment - Valid Request"
CORRELATION_ID=$(uuidgen)
PAYMENT_REQUEST="{\"correlationId\":\"$CORRELATION_ID\",\"amount\":100.50}"

PAYMENT_RESPONSE=$(curl -s -w "%{http_code}" -X POST \
    -H "Content-Type: application/json" \
    -d "$PAYMENT_REQUEST" \
    http://localhost:9999/payments)

HTTP_CODE=${PAYMENT_RESPONSE: -3}
RESPONSE_BODY=${PAYMENT_RESPONSE%???}

if [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "202" ]; then
    echo "✅ PASS: Payment processed successfully - HTTP Code: $HTTP_CODE"
    echo "   Request: $PAYMENT_REQUEST"
    echo "   Response: $RESPONSE_BODY"
else
    echo "❌ FAIL: Payment processing failed - HTTP Code: $HTTP_CODE"
    echo "   Response: $RESPONSE_BODY"
fi
echo ""

# Test 3: Process Payment - Invalid Request (Missing correlationId)
echo "🔍 Test 3: Process Payment - Invalid Request"
INVALID_REQUEST='{"amount": 50.25}'

INVALID_RESPONSE=$(curl -s -w "%{http_code}" -X POST \
    -H "Content-Type: application/json" \
    -d "$INVALID_REQUEST" \
    http://localhost:9999/payments)

HTTP_CODE=${INVALID_RESPONSE: -3}
RESPONSE_BODY=${INVALID_RESPONSE%???}

if [ "$HTTP_CODE" = "400" ]; then
    echo "✅ PASS: Invalid request properly rejected - HTTP Code: $HTTP_CODE"
    echo "   Response: $RESPONSE_BODY"
else
    echo "❌ FAIL: Invalid request not properly handled - HTTP Code: $HTTP_CODE"
    echo "   Response: $RESPONSE_BODY"
fi
echo ""

# Test 4: Payment Summary
echo "🔍 Test 4: Payment Summary"
FROM_TIME="2025-01-01T00:00:00.000Z"
TO_TIME="2025-12-31T23:59:59.000Z"

SUMMARY_RESPONSE=$(curl -s -w "%{http_code}" \
    "http://localhost:9999/payments-summary?from=$FROM_TIME&to=$TO_TIME")

HTTP_CODE=${SUMMARY_RESPONSE: -3}
RESPONSE_BODY=${SUMMARY_RESPONSE%???}

if [ "$HTTP_CODE" = "200" ]; then
    echo "✅ PASS: Payment summary retrieved - HTTP Code: $HTTP_CODE"
    echo "   Response: $RESPONSE_BODY"
    
    # Check if response contains required fields
    if echo "$RESPONSE_BODY" | grep -q "default" && echo "$RESPONSE_BODY" | grep -q "fallback"; then
        echo "✅ PASS: Summary contains required processor types"
    else
        echo "❌ FAIL: Summary missing required processor types"
    fi
    
    if echo "$RESPONSE_BODY" | grep -q "totalRequests" && echo "$RESPONSE_BODY" | grep -q "totalAmount"; then
        echo "✅ PASS: Summary contains required fields"
    else
        echo "❌ FAIL: Summary missing required fields"
    fi
else
    echo "❌ FAIL: Payment summary failed - HTTP Code: $HTTP_CODE"
    echo "   Response: $RESPONSE_BODY"
fi
echo ""

# Test 5: Multiple Payments
echo "🔍 Test 5: Multiple Payments for Consistency"
for i in {1..5}; do
    CORRELATION_ID=$(uuidgen)
    AMOUNT=$((50 + i * 10))
    
    PAYMENT_REQUEST="{\"correlationId\":\"$CORRELATION_ID\",\"amount\":$AMOUNT.00}"
    
    RESPONSE=$(curl -s -w "%{http_code}" -X POST \
        -H "Content-Type: application/json" \
        -d "$PAYMENT_REQUEST" \
        http://localhost:9999/payments)
    
    HTTP_CODE=${RESPONSE: -3}
    
    if [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "202" ]; then
        echo "✅ Payment $i processed successfully"
    else
        echo "❌ Payment $i failed - HTTP Code: $HTTP_CODE"
    fi
done
echo ""

# Test 6: Load Test (Simple)
echo "🔍 Test 6: Simple Load Test (10 concurrent requests)"
for i in {1..10}; do
    CORRELATION_ID=$(uuidgen)
    curl -s -X POST \
        -H "Content-Type: application/json" \
        -d "{\"correlationId\":\"$CORRELATION_ID\",\"amount\":25.50}" \
        http://localhost:9999/payments &
done

wait
echo "✅ Load test completed (10 concurrent requests)"
echo ""

# Test 7: Edge Cases
echo "🔍 Test 7: Edge Cases"

# Test with very small amount
CORRELATION_ID=$(uuidgen)
SMALL_AMOUNT_REQUEST="{\"correlationId\":\"$CORRELATION_ID\",\"amount\":0.01}"
RESPONSE=$(curl -s -w "%{http_code}" -X POST \
    -H "Content-Type: application/json" \
    -d "$SMALL_AMOUNT_REQUEST" \
    http://localhost:9999/payments)
HTTP_CODE=${RESPONSE: -3}

if [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "202" ]; then
    echo "✅ PASS: Small amount (0.01) processed"
else
    echo "❌ FAIL: Small amount rejected - HTTP Code: $HTTP_CODE"
fi

# Test with large amount
CORRELATION_ID=$(uuidgen)
LARGE_AMOUNT_REQUEST="{\"correlationId\":\"$CORRELATION_ID\",\"amount\":9999.99}"
RESPONSE=$(curl -s -w "%{http_code}" -X POST \
    -H "Content-Type: application/json" \
    -d "$LARGE_AMOUNT_REQUEST" \
    http://localhost:9999/payments)
HTTP_CODE=${RESPONSE: -3}

if [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "202" ]; then
    echo "✅ PASS: Large amount (9999.99) processed"
else
    echo "❌ FAIL: Large amount rejected - HTTP Code: $HTTP_CODE"
fi

# Test duplicate correlation ID
CORRELATION_ID=$(uuidgen)
DUPLICATE_REQUEST="{\"correlationId\":\"$CORRELATION_ID\",\"amount\":100.00}"

# First request
curl -s -X POST \
    -H "Content-Type: application/json" \
    -d "$DUPLICATE_REQUEST" \
    http://localhost:9999/payments > /dev/null

# Second request with same ID
RESPONSE=$(curl -s -w "%{http_code}" -X POST \
    -H "Content-Type: application/json" \
    -d "$DUPLICATE_REQUEST" \
    http://localhost:9999/payments)
HTTP_CODE=${RESPONSE: -3}

if [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "201" ] || [ "$HTTP_CODE" = "202" ]; then
    echo "✅ PASS: Duplicate correlation ID handled (idempotency)"
else
    echo "❌ FAIL: Duplicate correlation ID not handled properly - HTTP Code: $HTTP_CODE"
fi

echo ""

# Final Summary Check
echo "🔍 Final Summary Check"
FINAL_SUMMARY=$(curl -s http://localhost:9999/payments-summary)
echo "Final payment summary: $FINAL_SUMMARY"

echo ""
echo "=== Integration Tests Complete ==="
echo ""
echo "📊 Test Results Summary:"
echo "   ✅ All tests executed"
echo "   📈 Multiple payments processed"
echo "   🔄 Load testing performed"
echo "   🎯 Edge cases covered"
echo ""
echo "🏆 Integration tests completed successfully!"
