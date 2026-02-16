-- Track each user's current vote per topic with immutable first-vote timestamp.
CREATE TABLE topic_votes (
    topic_id BIGINT NOT NULL REFERENCES discussions(id),
    github_username TEXT NOT NULL REFERENCES users(github_username),
    position TEXT NOT NULL CHECK (position IN ('Interested', 'NotInterested')),
    first_voted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    PRIMARY KEY (topic_id, github_username)
);

CREATE INDEX idx_topic_votes_topic_id ON topic_votes(topic_id);
CREATE INDEX idx_topic_votes_github_username ON topic_votes(github_username);

-- Ensure every voter username from historical JSON data exists in users.
INSERT INTO users (github_username, display_name, created_at)
SELECT DISTINCT vote.value ->> 'voter', NULL, NOW()
FROM discussions d
CROSS JOIN LATERAL jsonb_array_elements(d.interested_parties) AS vote(value)
WHERE vote.value ? 'voter'
  AND vote.value ->> 'voter' <> ''
ON CONFLICT (github_username) DO NOTHING;

-- Backfill current vote state from existing discussions.interested_parties JSON.
-- We do not know historical vote times, so we seed first_voted_at from discussion.created_at.
INSERT INTO topic_votes (topic_id, github_username, position, first_voted_at)
SELECT d.id,
       vote.value ->> 'voter',
       vote.value ->> 'position',
       d.created_at
FROM discussions d
CROSS JOIN LATERAL jsonb_array_elements(d.interested_parties) AS vote(value)
WHERE vote.value ? 'voter'
  AND vote.value ? 'position'
  AND vote.value ->> 'voter' <> ''
  AND vote.value ->> 'position' IN ('Interested', 'NotInterested')
ON CONFLICT (topic_id, github_username) DO UPDATE
SET position = EXCLUDED.position;
