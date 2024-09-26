use rusqlite::{types::ToSqlOutput, ToSql};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub enum Side {
    Free,
    Shadow,
}

impl ToSql for Side {
    fn to_sql(&self) -> Result<ToSqlOutput, rusqlite::Error> {
        Ok(ToSqlOutput::from(serde_json::to_string(self).map_err(
            |err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)),
        )?))
    }
}

#[derive(Serialize, Deserialize)]
pub enum Victory {
    Ring,
    Military,
    Concession,
}

impl ToSql for Victory {
    fn to_sql(&self) -> Result<ToSqlOutput, rusqlite::Error> {
        Ok(ToSqlOutput::from(serde_json::to_string(self).map_err(
            |err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)),
        )?))
    }
}

#[derive(Serialize, Deserialize)]
pub enum Expansion {
    LoME,
    WoME,
    KoME,
    Cities,
    #[serde(rename = "Fate of Erebor")]
    FateOfErebor,
    Treebeard,
}

impl ToSql for Expansion {
    fn to_sql(&self) -> Result<ToSqlOutput, rusqlite::Error> {
        Ok(ToSqlOutput::from(serde_json::to_string(self).map_err(
            |err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)),
        )?))
    }
}

#[derive(Serialize, Deserialize, PartialEq)]
pub enum Match {
    Ranked,
    Unranked,
}

impl ToSql for Match {
    fn to_sql(&self) -> Result<ToSqlOutput, rusqlite::Error> {
        Ok(ToSqlOutput::from(serde_json::to_string(self).map_err(
            |err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)),
        )?))
    }
}

#[derive(Serialize, Deserialize)]
pub enum Competition {
    League,
    Tournament,
}

impl ToSql for Competition {
    fn to_sql(&self) -> Result<ToSqlOutput, rusqlite::Error> {
        Ok(ToSqlOutput::from(serde_json::to_string(self).map_err(
            |err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)),
        )?))
    }
}

#[derive(Serialize, Deserialize)]
pub enum League {
    General,
    LoME,
    WoME,
    Super,
    TTS,
}

impl ToSql for League {
    fn to_sql(&self) -> Result<ToSqlOutput, rusqlite::Error> {
        Ok(ToSqlOutput::from(serde_json::to_string(self).map_err(
            |err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)),
        )?))
    }
}

#[derive(Serialize, Deserialize)]
pub enum Stronghold {
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

impl ToSql for Stronghold {
    fn to_sql(&self) -> Result<ToSqlOutput, rusqlite::Error> {
        Ok(ToSqlOutput::from(serde_json::to_string(self).map_err(
            |err| rusqlite::Error::ToSqlConversionFailure(Box::new(err)),
        )?))
    }
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GameReportForm {
    pub winner: String,
    pub loser: String,
    pub side: Side,
    pub victory_type: Victory,
    pub match_type: Match,
    pub competition_types: Vec<Competition>,
    pub league: Option<League>,
    pub used_expansions: bool,
    pub expansions: Vec<Expansion>,
    pub was_treebeard_mustered: Option<bool>,
    pub used_handicap: bool,
    pub action_tokens: i32,
    pub dwarven_rings: i32,
    pub game_turns: i32,
    pub corruption: i32,
    pub did_fellowship_reach_mordor: bool,
    pub mordor_track: i32,
    pub initial_eyes: i32,
    pub was_aragorn_crowned: bool,
    pub aragorn_crowned_turn: i32,
    pub captured_strongholds: Vec<Stronghold>,
    pub interest_rating: i32,
    pub comment: String,
}

impl GameReportForm {
    fn winner_is_nonempty(&self) -> bool {
        self.winner != ""
    }

    fn loser_is_nonempty(&self) -> bool {
        self.loser != ""
    }

    fn competition_is_consistent(&self) -> bool {
        self.match_type == Match::Ranked || self.competition_types.is_empty()
    }

    fn league_is_consistent(&self) -> bool {
        false // TODO
    }

    pub fn validate(&self) -> Result<(), String> {
        return Ok(());
    }
}
