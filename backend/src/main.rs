use crate::types::GameReportForm;
use axum::{extract::Json, http::StatusCode, routing::get, routing::post, serve, Router};
use rusqlite;
use serde_json;
use tokio::net::TcpListener;
use tower_http::cors::{Any, CorsLayer};

mod types;

#[tokio::main]
async fn main() {
    let address = "0.0.0.0:3001";
    let cors = CorsLayer::new().allow_origin(Any);

    let app = Router::new()
        .route("/submit-report", post(submit_report))
        .layer(cors);

    let listener = TcpListener::bind(address).await.unwrap();
    serve(listener, app).await.unwrap();
}

fn insert_rows(report: GameReportForm) -> Result<usize, rusqlite::Error> {
    let conn = rusqlite::Connection::open("database.sqlite")?;
    let a = conn.execute(
        "INSERT INTO players (name, country) VALUES (:name, :country)",
        rusqlite::named_params! {":name": report.winner, ":country": "USA"},
    );
    let b = conn.execute(
        "INSERT INTO players (name, country) VALUES (:name, :country)",
        rusqlite::named_params! {":name": report.loser, ":country": "Russia"},
    );
    conn.close();
    /*
    let insert_result = conn.execute(
        "INSERT INTO games (winner, loser, side, victory_type, match_type, competition_types, league, used_expansions, \
        expansions, was_treebeard_mustered, used_handicap, action_tokens, dwarven_rings, game_turns, corruption, \
        did_fellowship_reach_mordor, mordor_track, initial_eyes, was_aragorn_crowned, aragorn_crowned_turn, \
        captured_strongholds, interest_rating, comment) \
        VALUES (:winner, :loser, :side, :victory_type, :match_type, :competition_types, :league, :used_expansions, \
        :expansions, :was_treebeard_mustered, :used_handicap, :action_tokens, :dwarven_rings, :game_turns, :corruption, \
        :did_fellowship_reach_mordor, :mordor_track, :initial_eyes, :was_aragorn_crowned, :aragorn_crowned_turn, \
        :captured_strongholds, :interest_rating, :comment)",
        rusqlite::named_params! {
            ":winner": report.winner,
            ":loser": report.loser,
            ":side": report.side,
            ":victory_type": report.victory_type,
            ":match_type": report.match_type,
            ":competition_types": [],
            ":league": report.league,
            ":used_expansions": report.used_expansions,
            ":expansions": [],
            ":was_treebeard_mustered": report.was_treebeard_mustered,
            ":used_handicap": report.used_handicap,
            ":action_tokens": report.action_tokens,
            ":dwarven_rings": report.dwarven_rings,
            ":game_turns": report.game_turns,
            ":corruption": report.corruption,
            ":did_fellowship_reach_mordor": report.did_fellowship_reach_mordor,
            ":mordor_track": report.mordor_track,
            ":initial_eyes": report.initial_eyes,
            ":was_aragorn_crowned": report.was_aragorn_crowned,
            ":aragorn_crowned_turn": report.aragorn_crowned_turn,
            ":captured_strongholds": [],
            ":interest_rating": report.interest_rating,
            ":comment": report.comment
        }
    );
    match &insert_result {
        Err(e) => {
            println!("Error: {}", e.to_string());
            insert_result
        }
        _ => insert_result
    }
    */
    match &b {
        Err(e) => {
            println!("Error: {}", e.to_string());
        }
        _ => (),
    }
    match &a {
        Err(e) => {
            println!("Error: {}", e.to_string());
            a
        }
        _ => a,
    }
}

async fn submit_report(Json(payload): Json<GameReportForm>) -> Result<String, StatusCode> {
    match insert_rows(payload) {
        Ok(_) => Ok("Done".to_string()),
        Err(_) => Err(StatusCode::BAD_REQUEST),
    }
}
