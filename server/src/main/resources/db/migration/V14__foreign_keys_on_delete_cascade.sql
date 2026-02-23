-- Ensure user/project/discussion deletions do not leave orphaned child rows.
-- This is especially important for admin cleanup flows that delete users directly.

ALTER TABLE discussion_events
    DROP CONSTRAINT IF EXISTS discussion_events_actor_fkey,
    ADD CONSTRAINT discussion_events_actor_fkey
      FOREIGN KEY (actor) REFERENCES users(github_username) ON DELETE CASCADE;

ALTER TABLE discussions
    DROP CONSTRAINT IF EXISTS discussions_facilitator_fkey,
    ADD CONSTRAINT discussions_facilitator_fkey
      FOREIGN KEY (facilitator) REFERENCES users(github_username) ON DELETE CASCADE;

ALTER TABLE topic_votes
    DROP CONSTRAINT IF EXISTS topic_votes_topic_id_fkey,
    ADD CONSTRAINT topic_votes_topic_id_fkey
      FOREIGN KEY (topic_id) REFERENCES discussions(id) ON DELETE CASCADE,
    DROP CONSTRAINT IF EXISTS topic_votes_github_username_fkey,
    ADD CONSTRAINT topic_votes_github_username_fkey
      FOREIGN KEY (github_username) REFERENCES users(github_username) ON DELETE CASCADE;

ALTER TABLE lightning_talks
    DROP CONSTRAINT IF EXISTS lightning_talks_speaker_fkey,
    ADD CONSTRAINT lightning_talks_speaker_fkey
      FOREIGN KEY (speaker) REFERENCES users(github_username) ON DELETE CASCADE;

ALTER TABLE hackathon_projects
    DROP CONSTRAINT IF EXISTS hackathon_projects_owner_fkey,
    ADD CONSTRAINT hackathon_projects_owner_fkey
      FOREIGN KEY (owner) REFERENCES users(github_username) ON DELETE CASCADE;

ALTER TABLE hackathon_project_members
    DROP CONSTRAINT IF EXISTS hackathon_project_members_project_id_fkey,
    ADD CONSTRAINT hackathon_project_members_project_id_fkey
      FOREIGN KEY (project_id) REFERENCES hackathon_projects(id) ON DELETE CASCADE,
    DROP CONSTRAINT IF EXISTS hackathon_project_members_github_username_fkey,
    ADD CONSTRAINT hackathon_project_members_github_username_fkey
      FOREIGN KEY (github_username) REFERENCES users(github_username) ON DELETE CASCADE;
