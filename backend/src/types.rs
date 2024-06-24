use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
enum Side {
    Free,
    Shadow,
}

#[derive(Serialize, Deserialize)]
enum Victory {
    Ring,
    Military,
    Concession,
}

#[derive(Serialize, Deserialize)]
enum Expansion {
    LoME,
    WoME,
    KoME,
    Cities,
    #[serde(rename = "Fate of Erebor")]
    FateOfErebor,
    Treebeard,
}

#[derive(Serialize, Deserialize)]
enum Match {
    Ranked,
    Unranked,
}

#[derive(Serialize, Deserialize)]
enum Competition {
    League,
    Tournament,
}

#[derive(Serialize, Deserialize)]
enum League {
    General,
    LoME,
    WoME,
    Super,
    TTS,
}

#[derive(Serialize, Deserialize)]
enum Stronghold {
    Rivendell,
    #[serde(rename = "Grey Havens")]
    GreyHavens,
    #[serde(rename = "Helm's Deep")]
    HelmsDeep,
    Lorien,
    #[serde(rename = "Woodland Realm")]
    WoodlandRealm,
    Erebor,
    #[serde(rename = "Minas Tirith")]
    MinasTirith,
    #[serde(rename = "Dol Amroth")]
    DolAmroth,
    Shire,
    Edoras,
    Dale,
    Pelargir,
    #[serde(rename = "Ered Luin (Cities expansion only)")]
    EredLuin,
    #[serde(rename = "Iron Hills (Fate of Erebor expansion only)")]
    IronHills,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GameReportForm {
    winner: Option<String>,
    loser: Option<String>,
    side: Option<Side>,
    victory_type: Option<Victory>,
    match_type: Option<Match>,
    competition_types: Vec<Competition>,
    league: Option<League>,
    used_expansions: Option<bool>,
    expansions: Vec<Expansion>,
    was_treebeard_mustered: Option<bool>,
    used_handicap: Option<bool>,
    action_tokens: i32,
    dwarven_rings: i32,
    game_turns: i32,
    corruption: i32,
    did_fellowship_reach_mordor: Option<bool>,
    mordor_track: i32,
    initial_eyes: i32,
    was_aragorn_crowned: Option<bool>,
    aragorn_crowned_turn: i32,
    captured_strongholds: Vec<Stronghold>,
    interest_rating: i32,
    comment: String,
}

impl GameReportForm {
    pub fn validate(&self) -> Result<(), String> {
        return Ok(());
    }
}
