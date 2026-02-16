-- Legacy vote JSON is now normalized into topic_votes.
ALTER TABLE discussions
    DROP COLUMN interested_parties;
