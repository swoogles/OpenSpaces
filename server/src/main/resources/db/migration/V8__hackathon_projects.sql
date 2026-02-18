-- Hackathon Projects (Wednesday hackday)
-- People can propose projects, join them, and seamlessly switch between them

CREATE TABLE hackathon_projects (
    id BIGINT PRIMARY KEY,
    title VARCHAR(80) NOT NULL,
    owner VARCHAR(255) NOT NULL REFERENCES users(github_username),
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    deleted_at TIMESTAMP WITH TIME ZONE,
    slack_channel_id VARCHAR(50),
    slack_thread_ts VARCHAR(50),
    slack_permalink TEXT
);

CREATE TABLE hackathon_project_members (
    project_id BIGINT NOT NULL REFERENCES hackathon_projects(id),
    github_username VARCHAR(255) NOT NULL REFERENCES users(github_username),
    joined_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    left_at TIMESTAMP WITH TIME ZONE,
    PRIMARY KEY (project_id, github_username)
);

-- Index for finding a user's current project quickly
CREATE INDEX idx_hackathon_members_user_active 
    ON hackathon_project_members(github_username) 
    WHERE left_at IS NULL;

-- Index for finding active projects
CREATE INDEX idx_hackathon_projects_active 
    ON hackathon_projects(id) 
    WHERE deleted_at IS NULL;
