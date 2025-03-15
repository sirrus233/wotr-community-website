export const sides = ["Free", "Shadow"] as const;

export const victoryTypes = ["Ring", "Military", "Concession"] as const;

export const matchTypes = ["Rated", "Unrated"] as const;

export const competitionTypes = ["League", "Tournament"] as const;

export const expansionLeagues = [
    "LoMELeague",
    "WoMELeague",
    "SuperLeague",
    "TTSLeague",
] as const;

export const leagues = ["GeneralLeague", ...expansionLeagues] as const;

export const leagueTiers = ["Tier1", "Tier2", "Tier3"] as const;

export const expansions = [
    "LoME",
    "WoME",
    "KoME",
    "Cities",
    "FateOfErebor",
    "Treebeard",
] as const;

export const strongholds = [
    "Rivendell",
    "GreyHavens",
    "HelmsDeep",
    "Lorien",
    "WoodlandRealm",
    "MinasTirith",
    "DolAmroth",
    "Shire",
    "Edoras",
    "Dale",
    "Pelargir",
    "EredLuin",
    "IronHills",
    "MountGundabad",
    "Moria",
    "DolGuldur",
    "Orthanc",
    "Morannon",
    "BaradDur",
    "MinasMorgul",
    "Umbar",
    "Angmar",
    "FarHarad",
    "SouthRhun",
    "Erebor",
] as const;

export const optionalFormFields = ["rid", "timestamp", "logFile"] as const;

export const optionalPayloadFields = [
    "competition",
    "league",
    "expansions",
    "treebeard",
    "actionTokens",
    "dwarvenRings",
    "mordor",
    "aragornTurn",
    "strongholds",
    "comment",
] as const;

export const optionalFields = [
    ...optionalFormFields,
    ...optionalPayloadFields,
] as const;

export const payloadFields = [
    ...optionalPayloadFields,
    "winner",
    "loser",
    "side",
    "victory",
    "match",
    "turns",
    "corruption",
    "initialEyes",
    "interestRating",
] as const;

export const optionalPlayerEditFields = ["country"] as const;

export const serverValidationErrors = [
    "VictoryConditionConflictSPRV",
    "VictoryConditionConflictFPRV",
    "VictoryConditionConflictSPMV",
    "VictoryConditionConflictFPMV",
    "VictoryConditionConflictConcession",
    "NoVictoryConditionMet",
    "InvalidSPMV",
    "InvalidFPMV",
    "InvalidSPRV",
    "InvalidFPRV",
    "CompetitionMismatch",
    "LeagueExpansionMismatch",
    "TreebeardExpansionMismatch",
    "TurnsOutOfRange",
    "CorruptionOutOfRange",
    "MordorOutOfRange",
    "InitialEyesOutOfRange",
    "InterestRatingOutOfRange",
    "InvalidStronghold",
] as const;

export const playerStates = ["Active", "Inactive"] as const;

export enum ErrorMessage {
    Default = "Something went wrong. Please contact an admin for assistance.",
    NotAuthorized = "You cannot pass! If you're an administrator, your session may have expired. Please log in and try again.",
    NotAuthorizedStatus = "You cannot pass!",
    UnknownAuthStatus = "Sign-in unavailable",
    Required = "Required",
    OnSubmit = "Could not submit, please resolve errors",
    MissingPlayerName = "This player does not exist in the database. Unless it's a new player, please check the spelling.",
    ExistingPlayerRequired = "Must choose an existing player",
    PairingFilterInvalid = "Select up to two",
}

export const INFINITE = 100;
export const LEADERBOARD_START_YEAR = 2023;
export const LEAGUE_START_YEAR = 2025;
export const MAX_GAME_LOG_SIZE_MB = 1;
export const MAX_GAME_LOG_SIZE_BYTES = MAX_GAME_LOG_SIZE_MB * 1024 * 1024;

export const COUNTRIES_DATA = {
    "Ascension Island": { code: "AC" },
    Andorra: { code: "AD" },
    "United Arab Emirates": { code: "AE" },
    Afghanistan: { code: "AF" },
    "Antigua and Barbuda": { code: "AG" },
    Anguilla: { code: "AI" },
    Albania: { code: "AL" },
    Armenia: { code: "AM" },
    Angola: { code: "AO" },
    Antarctica: { code: "AQ" },
    Argentina: { code: "AR" },
    "American Samoa": { code: "AS" },
    Austria: { code: "AT" },
    Australia: { code: "AU" },
    Aruba: { code: "AW" },
    "Åland Islands": { code: "AX" },
    Azerbaijan: { code: "AZ" },
    Bosnia: { code: "BA" },
    Barbados: { code: "BB" },
    Bangladesh: { code: "BD" },
    Belgium: { code: "BE" },
    "Burkina Faso": { code: "BF" },
    Bulgaria: { code: "BG" },
    Bahrain: { code: "BH" },
    Burundi: { code: "BI" },
    Benin: { code: "BJ" },
    "Saint Barthélemy": { code: "BL" },
    Bermuda: { code: "BM" },
    Brunei: { code: "BN" },
    Bolivia: { code: "BO" },
    "Bonaire, Sint Eustatius and Saba": { code: "BQ" },
    Brazil: { code: "BR" },
    Bahamas: { code: "BS" },
    Bhutan: { code: "BT" },
    "Bouvet Island": { code: "BV" },
    Botswana: { code: "BW" },
    Belarus: { code: "BY" },
    Belize: { code: "BZ" },
    Canada: { code: "CA" },
    "Cocos (Keeling) Islands": { code: "CC" },
    "Democratic Republic of the Congo": { code: "CD" },
    "Central African Republic": { code: "CF" },
    "Republic of the Congo": { code: "CG" },
    Switzerland: { code: "CH" },
    "Côte d'Ivoire": { code: "CI" },
    "Cook Islands": { code: "CK" },
    Chile: { code: "CL" },
    Cameroon: { code: "CM" },
    China: { code: "CN" },
    Colombia: { code: "CO" },
    "Clipperton Island": { code: "CP" },
    Sark: { code: "CQ" },
    "Costa Rica": { code: "CR" },
    Cuba: { code: "CU" },
    "Cabo Verde": { code: "CV" },
    Curaçao: { code: "CW" },
    "Christmas Island": { code: "CX" },
    Cyprus: { code: "CY" },
    "Czech Republic": { code: "CZ" },
    Germany: { code: "DE" },
    "Diego Garcia": { code: "DG" },
    Djibouti: { code: "DJ" },
    Denmark: { code: "DK" },
    Dominica: { code: "DM" },
    "Dominican Republic": { code: "DO" },
    Algeria: { code: "DZ" },
    "Ceuta and Melilla": { code: "EA" },
    Ecuador: { code: "EC" },
    Estonia: { code: "EE" },
    Egypt: { code: "EG" },
    "Western Sahara": { code: "EH" },
    Eritrea: { code: "ER" },
    Spain: { code: "ES" },
    Ethiopia: { code: "ET" },
    "European Union": { code: "EU" },
    Finland: { code: "FI" },
    Fiji: { code: "FJ" },
    "Falkland Islands": { code: "FK" },
    Micronesia: { code: "FM" },
    "Faroe Islands": { code: "FO" },
    France: { code: "FR" },
    Gabon: { code: "GA" },
    "United Kingdom": { code: "GB" },
    Grenada: { code: "GD" },
    Georgia: { code: "GE" },
    "French Guiana": { code: "GF" },
    Guernsey: { code: "GG" },
    Ghana: { code: "GH" },
    Gibraltar: { code: "GI" },
    Greenland: { code: "GL" },
    Gambia: { code: "GM" },
    Guinea: { code: "GN" },
    Guadeloupe: { code: "GP" },
    "Equatorial Guinea": { code: "GQ" },
    Greece: { code: "GR" },
    "South Georgia and the South Sandwich Islands": { code: "GS" },
    Guatemala: { code: "GT" },
    Guam: { code: "GU" },
    "Guinea-Bissau": { code: "GW" },
    Guyana: { code: "GY" },
    "Hong Kong": { code: "HK" },
    "Heard and McDonald Islands": { code: "HM" },
    Honduras: { code: "HN" },
    Croatia: { code: "HR" },
    Haiti: { code: "HT" },
    Hungary: { code: "HU" },
    "Canary Islands": { code: "IC" },
    Indonesia: { code: "ID" },
    Ireland: { code: "IE" },
    Israel: { code: "IL" },
    "Isle of Man": { code: "IM" },
    India: { code: "IN" },
    "British Indian Ocean Territory": { code: "IO" },
    Iraq: { code: "IQ" },
    Iran: { code: "IR" },
    Iceland: { code: "IS" },
    Italy: { code: "IT" },
    Jersey: { code: "JE" },
    Jamaica: { code: "JM" },
    Jordan: { code: "JO" },
    Japan: { code: "JP" },
    Kenya: { code: "KE" },
    Kyrgyzstan: { code: "KG" },
    Cambodia: { code: "KH" },
    Kiribati: { code: "KI" },
    Comoros: { code: "KM" },
    "Saint Kitts and Nevis": { code: "KN" },
    "North Korea": { code: "KP" },
    "South Korea": { code: "KR" },
    Kuwait: { code: "KW" },
    "Cayman Islands": { code: "KY" },
    Kazakhstan: { code: "KZ" },
    Laos: { code: "LA" },
    Lebanon: { code: "LB" },
    "Saint Lucia": { code: "LC" },
    Liechtenstein: { code: "LI" },
    "Sri Lanka": { code: "LK" },
    Liberia: { code: "LR" },
    Lesotho: { code: "LS" },
    Lithuania: { code: "LT" },
    Luxembourg: { code: "LU" },
    Latvia: { code: "LV" },
    Libya: { code: "LY" },
    Morocco: { code: "MA" },
    Monaco: { code: "MC" },
    Moldova: { code: "MD" },
    Montenegro: { code: "ME" },
    "Saint Martin": { code: "MF" },
    Madagascar: { code: "MG" },
    "Marshall Islands": { code: "MH" },
    "North Macedonia": { code: "MK" },
    Mali: { code: "ML" },
    Myanmar: { code: "MM" },
    Mongolia: { code: "MN" },
    Macau: { code: "MO" },
    "Northern Mariana Islands": { code: "MP" },
    Martinique: { code: "MQ" },
    Mauritania: { code: "MR" },
    Montserrat: { code: "MS" },
    Malta: { code: "MT" },
    Mauritius: { code: "MU" },
    Maldives: { code: "MV" },
    Malawi: { code: "MW" },
    Mexico: { code: "MX" },
    Malaysia: { code: "MY" },
    Mozambique: { code: "MZ" },
    Namibia: { code: "NA" },
    "New Caledonia": { code: "NC" },
    Niger: { code: "NE" },
    "Norfolk Island": { code: "NF" },
    Nigeria: { code: "NG" },
    Nicaragua: { code: "NI" },
    Netherlands: { code: "NL" },
    Norway: { code: "NO" },
    Nepal: { code: "NP" },
    Nauru: { code: "NR" },
    Niue: { code: "NU" },
    "New Zealand": { code: "NZ" },
    Oman: { code: "OM" },
    Panama: { code: "PA" },
    Peru: { code: "PE" },
    "French Polynesia": { code: "PF" },
    "Papua New Guinea": { code: "PG" },
    Philippines: { code: "PH" },
    Pakistan: { code: "PK" },
    Poland: { code: "PL" },
    "Saint Pierre and Miquelon": { code: "PM" },
    "Pitcairn Islands": { code: "PN" },
    "Puerto Rico": { code: "PR" },
    Palestine: { code: "PS" },
    Portugal: { code: "PT" },
    Palau: { code: "PW" },
    Paraguay: { code: "PY" },
    Qatar: { code: "QA" },
    Réunion: { code: "RE" },
    Romania: { code: "RO" },
    Serbia: { code: "RS" },
    Russia: { code: "RU" },
    Rwanda: { code: "RW" },
    "Saudi Arabia": { code: "SA" },
    "Solomon Islands": { code: "SB" },
    Seychelles: { code: "SC" },
    Sudan: { code: "SD" },
    Sweden: { code: "SE" },
    Singapore: { code: "SG" },
    "Saint Helena": { code: "SH" },
    Slovenia: { code: "SI" },
    "Svalbard and Jan Mayen": { code: "SJ" },
    Slovakia: { code: "SK" },
    "Sierra Leone": { code: "SL" },
    "San Marino": { code: "SM" },
    Senegal: { code: "SN" },
    Somalia: { code: "SO" },
    Suriname: { code: "SR" },
    "South Sudan": { code: "SS" },
    "São Tomé and Príncipe": { code: "ST" },
    "El Salvador": { code: "SV" },
    "Sint Maarten": { code: "SX" },
    Syria: { code: "SY" },
    Swaziland: { code: "SZ" },
    "Tristan da Cunha": { code: "TA" },
    "Turks and Caicos Islands": { code: "TC" },
    Chad: { code: "TD" },
    "French Southern Territories": { code: "TF" },
    Togo: { code: "TG" },
    Thailand: { code: "TH" },
    Tajikistan: { code: "TJ" },
    Tokelau: { code: "TK" },
    "Timor-Leste": { code: "TL" },
    Turkmenistan: { code: "TM" },
    Tunisia: { code: "TN" },
    Tonga: { code: "TO" },
    Turkey: { code: "TR" },
    "Trinidad and Tobago": { code: "TT" },
    Tuvalu: { code: "TV" },
    Taiwan: { code: "TW" },
    Tanzania: { code: "TZ" },
    Ukraine: { code: "UA" },
    Uganda: { code: "UG" },
    "U.S. Minor Outlying Islands": { code: "UM" },
    "United Nations": { code: "UN" },
    "United States of America": { code: "US" },
    Uruguay: { code: "UY" },
    Uzbekistan: { code: "UZ" },
    "Vatican City": { code: "VA" },
    "Saint Vincent and the Grenadines": { code: "VC" },
    Venezuela: { code: "VE" },
    "Virgin Islands (British)": { code: "VG" },
    "Virgin Islands (U.S.)": { code: "VI" },
    Vietnam: { code: "VN" },
    Vanuatu: { code: "VU" },
    "Wallis and Futuna": { code: "WF" },
    Samoa: { code: "WS" },
    Kosovo: { code: "XK" },
    Yemen: { code: "YE" },
    Mayotte: { code: "YT" },
    "South Africa": { code: "ZA" },
    Zambia: { code: "ZM" },
    Zimbabwe: { code: "ZW" },
    Scotland: { code: "GB-SCT" },
    Wales: { code: "GB-WLS" },
};
