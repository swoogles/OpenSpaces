package co.wtf.openspaces;

import com.microsoft.playwright.*;
import org.junit.jupiter.api.*;

/**
 * Base test class providing Playwright browser and page setup.
 * 
 * Configure via environment variables:
 * - BASE_URL: The app URL (default: http://localhost:8080)
 * - HEADLESS: Run headless (default: true)
 */
public abstract class BaseTest {
    protected static Playwright playwright;
    protected static Browser browser;
    protected BrowserContext context;
    protected Page page;

    protected static String baseUrl;

    @BeforeAll
    static void launchBrowser() {
        baseUrl = System.getProperty("baseUrl", System.getenv().getOrDefault("BASE_URL", "http://localhost:8080"));
        boolean headless = Boolean.parseBoolean(System.getenv().getOrDefault("HEADLESS", "true"));
        
        playwright = Playwright.create();
        browser = playwright.chromium().launch(new BrowserType.LaunchOptions()
            .setHeadless(headless)
            .setSlowMo(headless ? 0 : 100)); // Slow down when visible for debugging
    }

    @AfterAll
    static void closeBrowser() {
        browser.close();
        playwright.close();
    }

    @BeforeEach
    void createContextAndPage() {
        context = browser.newContext(new Browser.NewContextOptions()
            .setViewportSize(390, 844)); // iPhone 12 Pro size - mobile-first
        page = context.newPage();
    }

    @AfterEach
    void closeContext() {
        context.close();
    }

    /**
     * Authenticate as a test user.
     * Requires the server to be running with TEST_MODE=true.
     */
    protected void authenticateAs(String username) {
        page.navigate(baseUrl + "/api/test/auth?username=" + username);
        // The endpoint sets cookies and we're now authenticated
    }

    /**
     * Navigate to the main app after authentication.
     */
    protected void goToApp() {
        page.navigate(baseUrl);
        // Wait for the app to load (WebSocket connection, initial state)
        page.waitForSelector(".BottomNav", new Page.WaitForSelectorOptions().setTimeout(10000));
    }

    /**
     * Navigate to Topics view.
     */
    protected void goToTopics() {
        page.click(".BottomNav-tab:has-text('Topics')");
        page.waitForSelector(".TopicSection-title");
    }

    /**
     * Navigate to Activities view.
     */
    protected void goToActivities() {
        page.click(".BottomNav-tab:has-text('Activities')");
        page.waitForSelector(".ActivitiesView");
    }

    /**
     * Navigate to Schedule view.
     */
    protected void goToSchedule() {
        page.click(".BottomNav-tab:has-text('Schedule')");
        page.waitForSelector(".ScheduleView");
    }
}
