CREATE TABLE IF NOT EXISTS payments (
    id SERIAL PRIMARY KEY,
    correlation_id UUID UNIQUE NOT NULL,
    amount DECIMAL(15,2) NOT NULL,
    requested_at TIMESTAMP WITH TIME ZONE NOT NULL,
    processed_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    processor_type VARCHAR(20) NOT NULL CHECK (processor_type IN ('default', 'fallback'))
);

CREATE INDEX IF NOT EXISTS idx_payments_correlation_id ON payments(correlation_id);
CREATE INDEX IF NOT EXISTS idx_payments_processed_at ON payments(processed_at);
CREATE INDEX IF NOT EXISTS idx_payments_processor_type ON payments(processor_type);
CREATE INDEX IF NOT EXISTS idx_payments_requested_at ON payments(requested_at);
