#!/bin/bash

sqlite3 $(dirname "$0")/../database/database.sqlite <<END_SQL

CREATE TABLE IF NOT EXISTS players (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    country TEXT
);

CREATE TABLE IF NOT EXISTS player_stats (
    id INTEGER PRIMARY KEY,
    player_id INTEGER NOT NULL,
    shadow_rating INTEGER NOT NULL,
    free_rating INTEGER NOT NULL,
    games_played INTEGER NOT NULL,
    FOREIGN KEY (player_id) REFERENCES players(id)
);

CREATE TABLE IF NOT EXISTS games (
    id INTEGER PRIMARY KEY,
    timestamp INTEGER NOT NULL,
    turns INTEGER NOT NULL,
    winner_id INTEGER NOT NULL,
    loser_id INTEGER NOT NULL,
    winning_side TEXT NOT NULL,
    victory_type TEXT NOT NULL,
    match_type TEXT NOT NULL,
    competition_types TEXT NOT NULL,
    league TEXT,
    used_expansions INTEGER NOT NULL,
    expansions TEXT NOT NULL,
    was_treebeard_mustered INTEGER,
    used_handicap INTEGER NOT NULL,
    action_tokens INTEGER NOT NULL,
    dwarven_rings INTEGER NOT NULL,
    game_turns INTEGER NOT NULL,
    corruption INTEGER NOT NULL,
    did_fellowship_reach_mordor INTEGER NOT NULL,
    mordor_track INTEGER NOT NULL,
    initial_eyes INTEGER NOT NULL,
    was_aragorn_crowned INTEGER NOT NULL,
    aragorn_crowned_turn INTEGER NOT NULL,
    captured_strongholds TEXT NOT NULL,
    interest_rating INTEGER NOT NULL,
    comment TEXT,
    FOREIGN KEY(winner_id) REFERENCES players(id)
    FOREIGN KEY(loser_id) REFERENCES players(id)
);

END_SQL
