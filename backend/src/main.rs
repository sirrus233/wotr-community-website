use crate::types::GameReportForm;
use axum::{extract::Json, http::StatusCode, routing::get, routing::post, serve, Router};
use rusqlite;
use tokio::net::TcpListener;
use tower_http::cors::{Any, CorsLayer};
use types::GameReportFormToSql;

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

fn insert_rows(
    winner: String,
    loser: String,
    named_params: [(&'static str, &dyn rusqlite::ToSql); 23],
) -> Result<(), rusqlite::Error> {
    let mut conn = rusqlite::Connection::open("database/database.sqlite")?;
    let tx = conn.transaction()?;

    tx.execute(
        "INSERT INTO players (name, country) VALUES (:name, :country)",
        rusqlite::named_params! {":name": winner, ":country": "USA"},
    )?;

    tx.execute(
        "INSERT INTO players (name, country) VALUES (:name, :country)",
        rusqlite::named_params! {":name": loser, ":country": "Russia"},
    )?;

    /*
    let sql = r#"
    INSERT INTO games (
        winner, loser, side, victory_type, match_type, competition_types,
        league, used_expansions, expansions, was_treebeard_mustered,
        used_handicap, action_tokens, dwarven_rings, game_turns, corruption,
        did_fellowship_reach_mordor, mordor_track, initial_eyes,
        was_aragorn_crowned, aragorn_crowned_turn, captured_strongholds,
        interest_rating, comment
    ) VALUES (
        :winner, :loser, si:de, :victory_type, :match_type, :competition_types,
        :league, :used_expansions, :expansions, :was_treebeard_mustered,
        :used_handicap, :action_tokens, :dwarven_rings, :game_turns, :corruption,
        :did_fellowship_reach_mordor, :mordor_track, :initial_eyes,
        :was_aragorn_crowned, :aragorn_crowned_turn, :captured_strongholds,
        :interest_rating, :comment
    )
    "#;

    tx.execute(sql, &named_params)?;
    */

    tx.commit()
}

async fn submit_report(Json(report): Json<GameReportForm>) -> Result<String, StatusCode> {
    let params_struct = GameReportFormToSql::from_game_report_form(&report).unwrap();
    let params = params_struct.as_named_params().unwrap();

    match insert_rows(report.winner, report.loser, params) {
        Ok(_) => Ok("Done".to_string()),
        Err(_) => Err(StatusCode::BAD_REQUEST),
    }
}
