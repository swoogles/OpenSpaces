# E2E Tests (Playwright + Java)

End-to-end tests for the Open Spaces app using Playwright with Java SDK.

## Prerequisites

- Java 17+
- Maven 3.8+
- The server running with `TEST_MODE=true`

## Running Tests

### 1. Start the server in test mode

```bash
TEST_MODE=true sbt server/run
```

### 2. Run the tests

```bash
cd e2e-tests

# Install Playwright browsers (first time only)
mvn exec:java -e -D exec.mainClass=com.microsoft.playwright.CLI -D exec.args="install"

# Run tests
mvn test
```

### Configuration

Environment variables:
- `BASE_URL` - App URL (default: `http://localhost:8080`)
- `HEADLESS` - Run headless (default: `true`, set to `false` for debugging)

### Running with visible browser

```bash
HEADLESS=false mvn test
```

## Test Authentication

Tests use `/api/test/auth?username=<name>` to authenticate. This endpoint:
- Only works when `TEST_MODE=true` is set
- Creates/upserts the test user in the database
- Sets auth cookies for immediate WebSocket access

## Test Structure

- `BaseTest.java` - Common setup, browser lifecycle, auth helpers
- `VotingFlowTest.java` - Voting queue behavior (Topics, Activities)

## Adding New Tests

1. Extend `BaseTest`
2. Call `authenticateAs("unique-username")` to authenticate
3. Call `goToApp()` to navigate to the app
4. Use Playwright's Page API to interact and assert

## CI Integration

```yaml
# Example GitHub Actions step
- name: Run E2E Tests
  env:
    BASE_URL: http://localhost:8080
    TEST_MODE: "true"
  run: |
    cd e2e-tests
    mvn test
```
