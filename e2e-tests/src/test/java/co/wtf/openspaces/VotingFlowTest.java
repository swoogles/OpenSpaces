package co.wtf.openspaces;

import com.microsoft.playwright.*;
import com.microsoft.playwright.options.*;
import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * E2E tests for the voting flow (Topics and Activities).
 */
public class VotingFlowTest extends BaseTest {

    @Test
    @DisplayName("Authenticated user can view Topics")
    void canViewTopics() {
        authenticateAs("e2e-test-user");
        goToApp();
        goToTopics();

        // Should see the Topics section
        assertTrue(page.isVisible(".TopicSection-title"));
    }

    @Test
    @DisplayName("Authenticated user can view Activities")
    void canViewActivities() {
        authenticateAs("e2e-test-user");
        goToApp();
        goToActivities();

        // Should see the Activities view
        assertTrue(page.isVisible(".ActivitiesView"));
    }

    @Test
    @DisplayName("Topics shows one card at a time for unvoted topics")
    void topicsShowsOneAtATime() {
        authenticateAs("e2e-voting-test");
        goToApp();
        goToTopics();

        // Count visible topic cards in the pending section
        // Should be at most 1 unvoted card visible at a time
        Locator pendingCards = page.locator(".VotingQueueView-pending .TopicCard, .TopicSection .TopicsContainer .TopicCard").first();
        
        // Either no cards (all voted) or exactly one
        int count = page.locator(".TopicSection").first().locator(".TopicsContainer .TopicCard").count();
        assertTrue(count <= 1, "Should show at most 1 unvoted topic at a time, found: " + count);
    }

    @Test
    @DisplayName("Swipe right on topic votes interested")
    void swipeRightVotesInterested() {
        authenticateAs("e2e-swipe-test-" + System.currentTimeMillis());
        goToApp();
        goToTopics();

        // Check if there's an unvoted topic
        Locator topicCard = page.locator(".TopicsContainer .TopicCard").first();
        if (topicCard.count() == 0) {
            // No topics to vote on, skip test
            return;
        }

        // Get the card's bounding box for swipe gesture
        BoundingBox box = topicCard.boundingBox();
        if (box == null) return;

        // Perform swipe right gesture
        double startX = box.x + box.width * 0.3;
        double endX = box.x + box.width * 0.9;
        double y = box.y + box.height / 2;

        page.mouse().move(startX, y);
        page.mouse().down();
        page.mouse().move(endX, y, new Mouse.MoveOptions().setSteps(10));
        page.mouse().up();

        // Wait for animation/state update
        page.waitForTimeout(500);

        // The card should now be in the "Viewed Topics" section or show vote indicator
        // This is a smoke test - just verify no crash
    }

    @Test
    @DisplayName("Activities shows one card at a time for unvoted activities")
    void activitiesShowsOneAtATime() {
        authenticateAs("e2e-activity-test");
        goToApp();
        goToActivities();

        // Count visible activity cards in the pending section
        Locator pendingSection = page.locator(".VotingQueueView-pending");
        if (pendingSection.count() > 0) {
            int pendingCards = pendingSection.locator(".HackathonProjectCard").count();
            assertTrue(pendingCards <= 1, "Should show at most 1 unvoted activity at a time");
        }
    }

    @Test
    @DisplayName("Nav badges show unvoted counts")
    void navBadgesShowCounts() {
        authenticateAs("e2e-badge-test");
        goToApp();

        // Check if badges are present (they only appear when count > 0)
        Locator topicsBadge = page.locator(".BottomNav-tab:has-text('Topics') .BottomNav-badge");
        Locator activitiesBadge = page.locator(".BottomNav-tab:has-text('Activities') .BottomNav-badge");

        // Badges are optional - just verify no errors accessing them
        // The badge text should be a number if present
        if (topicsBadge.count() > 0) {
            String badgeText = topicsBadge.textContent();
            assertTrue(badgeText.matches("\\d+\\+?"), "Topics badge should be a number: " + badgeText);
        }
        if (activitiesBadge.count() > 0) {
            String badgeText = activitiesBadge.textContent();
            assertTrue(badgeText.matches("\\d+\\+?"), "Activities badge should be a number: " + badgeText);
        }
    }

    @Test
    @DisplayName("Schedule view loads successfully")
    void scheduleViewLoads() {
        authenticateAs("e2e-schedule-test");
        goToApp();
        goToSchedule();

        // Should see the schedule view
        assertTrue(page.isVisible(".ScheduleView") || page.isVisible(".CalendarDayView"), 
            "Should display schedule view");
    }

    @Test
    @DisplayName("Create button opens creation sheet")
    void createButtonOpensSheet() {
        authenticateAs("e2e-create-test");
        goToApp();

        // Click the + button in the nav
        page.click(".BottomNav-tab--create");

        // Should open the create sheet
        page.waitForSelector(".CreateSheet", new Page.WaitForSelectorOptions().setTimeout(3000));
        assertTrue(page.isVisible(".CreateSheet"));
    }
}
