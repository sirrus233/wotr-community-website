use crate::types::GameReportForm;
use axum::{extract::Json, http::StatusCode, routing::get, routing::post, serve, Router};
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

async fn submit_report(Json(payload): Json<GameReportForm>) -> Result<String, StatusCode> {
    let s = serde_json::to_string(&payload).expect("Payload should be serializable.");
    println!("{s}");
    Ok("Done".to_string())

    //Err(StatusCode::BAD_REQUEST)
}
