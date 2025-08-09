# Rinha de Backend 2025 - Ada Implementation

Este projeto implementa um backend em Ada para a Rinha de Backend 2025, seguindo todas as especificações do desafio.

## Tecnologias Utilizadas

- **Linguagem**: Ada 2012
- **Servidor Web**: Implementação customizada com load balancing via Nginx
- **Banco de Dados**: PostgreSQL 15
- **Containerização**: Docker Compose
- **Load Balancer**: Nginx

## Arquitetura

O sistema segue a arquitetura especificada:

- **2 instâncias de backend** em Ada
- **Load balancer** Nginx distribuindo requisições
- **Banco PostgreSQL** para persistência
- **Integração** com Payment Processors externos

## Estrutura do Projeto

```
ada-backend/
├── src/
│   ├── rinha_backend.adb          # Programa principal
│   ├── http_server.ads/.adb       # Servidor HTTP
│   ├── payment_processor.ads/.adb # Integração com processadores
│   ├── payment_types.ads          # Tipos de dados
│   └── database.ads/.adb          # Acesso ao banco de dados
├── docker-compose.yml             # Configuração dos contêineres
├── nginx.conf                     # Configuração do load balancer
├── init.sql                       # Script de inicialização do banco
├── Dockerfile                     # Imagem do backend Ada
└── alire.toml                     # Configuração do projeto Ada
```

## Endpoints Implementados

### POST /payments
Processa pagamentos através dos Payment Processors.

**Request:**
```json
{
    "correlationId": "4a7901b8-7d26-4d9d-aa19-4dc1c7cf60b3",
    "amount": 19.90
}
```

**Response:** HTTP 2XX com qualquer corpo

### GET /payments-summary?from=&to=
Retorna resumo dos pagamentos processados.

**Response:**
```json
{
    "default": {
        "totalRequests": 43236,
        "totalAmount": 415542345.98
    },
    "fallback": {
        "totalRequests": 423545,
        "totalAmount": 329347.34
    }
}
```

## Estratégia de Processamento

1. **Prioridade para Default Processor** (menores taxas)
2. **Health checks periódicos** respeitando rate limit (5s)
3. **Fallback automático** em caso de falha
4. **Consistência de dados** para auditoria

## Limites de Recursos

- **Total CPU**: 1.5 cores
- **Total Memória**: 350MB
- **Distribuição (atual)**:
  - Nginx: 0.10 CPU, 40MB
  - Backend 1: 0.50 CPU, 100MB  
  - Backend 2: 0.50 CPU, 100MB
  - PostgreSQL: 0.30 CPU, 90MB

## Execução

1. **Rodar tudo com um único script (recomendado):**
```bash
./run-local.sh         # compila, builda a imagem e sobe tudo
./run-local.sh down    # derruba tudo
./run-local.sh rebuild # rebuilda a imagem e reinicia backend
```

2. **Passos manuais (alternativa):**
```bash
# Subir Payment Processors (oficiais)
docker compose -f ../rinha-de-backend-2025-payment-processor/containerization/docker-compose.yml up -d

# Subir o backend (nesta pasta)
docker compose up -d
```

3. **Testar endpoints:**
```bash
# Processar pagamento
curl -X POST http://localhost:9999/payments \
  -H "Content-Type: application/json" \
  -d '{"correlationId":"123e4567-e89b-12d3-a456-426614174000","amount":100.50}'

# Obter resumo
curl http://localhost:9999/payments-summary
```

## Testes

O projeto inclui uma suíte completa de testes para garantir conformidade com as especificações da Rinha:

### Executar Todos os Testes
```bash
cd tests/
./run_tests.sh
# ou
make test
```

### Tipos de Teste Disponíveis

1. **Testes Unitários** - Testam componentes individuais
```bash
./run_tests.sh unit
# ou
make test-unit
```

2. **Testes de Integração** - Verificam conformidade com especificações
```bash
./run_tests.sh integration  
# ou
make test-integration
```

3. **Testes de Carga** - Simulam condições da competição
```bash
./run_tests.sh load
# ou  
make test-load
```

### Cobertura dos Testes

✅ **Conformidade com Endpoints**
- POST /payments (HTTP 2XX, validação de UUID/decimal)
- GET /payments-summary (formato JSON correto)

✅ **Estratégia de Processamento**
- Priorização do processador default (menor taxa)
- Fallback automático para processador secundário
- Health checks com rate limiting (5s)

✅ **Consistência de Dados**
- Persistência correta no banco PostgreSQL
- Agregação precisa para auditorias do Banco Central
- Prevenção de inconsistências (multa de 35%)

✅ **Performance**
- Otimização para p99 baixo (bônus de performance)
- Testes de carga simulando condições reais
- Medição de throughput e latência

✅ **Recursos e Arquitetura**
- Limites de CPU (1.5) e Memória (350MB)
- Load balancing com 2 instâncias
- Integração com Payment Processors via Docker network

## Compilação Local

Para desenvolvimento local com Alire:

```bash
alr build
./bin/rinha_backend
```

## Características do Ada

- **Type safety**: Sistema de tipos forte previne muitos erros
- **Concorrência nativa**: Tasks Ada para processamento paralelo
- **Performance**: Compilação nativa para máxima eficiência
- **Confiabilidade**: Ideal para sistemas críticos como processamento de pagamentos

## Repositório

Código fonte completo disponível em: [https://github.com/cleissonbarbosa/rinha-backend-ada](https://github.com/cleissonbarbosa/rinha-backend-ada)
