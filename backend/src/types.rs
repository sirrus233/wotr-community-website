use rusqlite::ToSql;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub enum Side {
    Free,
    Shadow,
}

#[derive(Serialize, Deserialize)]
pub enum Victory {
    Ring,
    Military,
    Concession,
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

#[derive(Serialize, Deserialize, PartialEq)]
pub enum Match {
    Ranked,
    Unranked,
}

#[derive(Serialize, Deserialize)]
pub enum Competition {
    League,
    Tournament,
}

#[derive(Clone, Copy, Serialize, Deserialize)]
pub enum League {
    General,
    LoME,
    WoME,
    Super,
    TTS,
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

pub struct GameReportFormToSql {
    pub winner: String,
    pub loser: String,
    side: String,
    victory_type: String,
    match_type: String,
    competition_types: String,
    league: Option<String>,
    used_expansions: bool,
    expansions: String,
    was_treebeard_mustered: Option<bool>,
    used_handicap: bool,
    action_tokens: i32,
    dwarven_rings: i32,
    game_turns: i32,
    corruption: i32,
    did_fellowship_reach_mordor: bool,
    mordor_track: i32,
    initial_eyes: i32,
    was_aragorn_crowned: bool,
    aragorn_crowned_turn: i32,
    captured_strongholds: String,
    interest_rating: i32,
    comment: String,
}

impl GameReportFormToSql {
    pub fn from_game_report_form(report: &GameReportForm) -> Result<Self, serde_json::Error> {
        Ok(GameReportFormToSql {
            winner: report.winner.clone(),
            loser: report.loser.clone(),
            side: serde_json::to_string(&report.side)?,
            victory_type: serde_json::to_string(&report.victory_type)?,
            match_type: serde_json::to_string(&report.match_type)?,
            competition_types: serde_json::to_string(&report.competition_types)?,
            league: match report.league {
                None => None,
                Some(a) => Some(serde_json::to_string(&a)?),
            },
            used_expansions: report.used_expansions,
            expansions: serde_json::to_string(&report.expansions)?,
            was_treebeard_mustered: report.was_treebeard_mustered,
            used_handicap: report.used_handicap,
            action_tokens: report.action_tokens,
            dwarven_rings: report.dwarven_rings,
            game_turns: report.game_turns,
            corruption: report.corruption,
            did_fellowship_reach_mordor: report.did_fellowship_reach_mordor,
            mordor_track: report.mordor_track,
            initial_eyes: report.initial_eyes,
            was_aragorn_crowned: report.was_aragorn_crowned,
            aragorn_crowned_turn: report.aragorn_crowned_turn,
            captured_strongholds: serde_json::to_string(&report.captured_strongholds)?,
            interest_rating: report.interest_rating,
            comment: report.comment.clone(),
        })
    }

    pub fn as_named_params(&self) -> Result<[(&'static str, &dyn ToSql); 23], serde_json::Error> {
        Ok([
            (":winner", &self.winner),
            (":loser", &self.loser),
            (":side", &self.side),
            (":victory_type", &self.victory_type),
            (":match_type", &self.match_type),
            (":competition_types", &self.competition_types),
            (":league", &self.league),
            (":used_expansions", &self.used_expansions),
            (":expansions", &self.expansions),
            (":was_treebeard_mustered", &self.was_treebeard_mustered),
            (":used_handicap", &self.used_handicap),
            (":action_tokens", &self.action_tokens),
            (":dwarven_rings", &self.dwarven_rings),
            (":game_turns", &self.game_turns),
            (":corruption", &self.corruption),
            (
                ":did_fellowship_reach_mordor",
                &self.did_fellowship_reach_mordor,
            ),
            (":mordor_track", &self.mordor_track),
            (":initial_eyes", &self.initial_eyes),
            (":was_aragorn_crowned", &self.was_aragorn_crowned),
            (":aragorn_crowned_turn", &self.aragorn_crowned_turn),
            (":captured_strongholds", &self.captured_strongholds),
            (":interest_rating", &self.interest_rating),
            (":comment", &self.comment),
        ])
    }
}
