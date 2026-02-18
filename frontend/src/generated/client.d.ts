interface IAfter {
  tag: "After";
  contents: string;
}

interface IBefore {
  tag: "Before";
  contents: string;
}

interface IBetween {
  tag: "Between";
  contents: [string, string];
}

interface IDeleteReportRequest {
  rid: number;
}

interface IEditPlayerRequest {
  pid: number;
  name: string;
  country: string | null;
}

interface IGameReportFilterSpec {
  players: number[] | null;
  pairing: [number, number] | null;
  timestamp: TimestampFilter | null;
  winners: number[] | null;
  losers: number[] | null;
  turns: InequalityFilter | null;
  victory: VictoryFilter[] | null;
  leagues: League[] | null;
  tokens: InequalityFilter | null;
  dwarvenRings: InequalityFilter | null;
  musterPoints: InequalityFilter | null;
  corruption: InequalityFilter | null;
  mordor: NullableFilter<InequalityFilter> | null;
  aragorn: NullableFilter<InequalityFilter> | null;
  treebeard: boolean | null;
  initialEyes: InequalityFilter | null;
  interestRating: InequalityFilter | null;
  hasLog: boolean | null;
}

interface IGetLeaderboardResponse {
  entries: LeaderboardEntry[];
}

interface IGetReportsResponse {
  reports: ProcessedGameReport[];
  total: number;
}

interface ILeaderboardEntry {
  pid: number;
  name: string;
  country: string | null;
  isActive: boolean;
  currentRatingFree: number;
  currentRatingShadow: number;
  averageRating: number;
  totalGames: number;
  year: number;
  yearlyGames: number;
  yearlyWinsFree: number;
  yearlyWinsShadow: number;
  yearlyLossesFree: number;
  yearlyLossesShadow: number;
  yearlyWinRateFree: number;
  yearlyWinRateShadow: number;
}

interface ILeagueGameStats {
  opponent: string;
  wins: number;
  losses: number;
}

interface ILeaguePlayerStats {
  name: string;
  summary: LeaguePlayerStatsSummary;
  gameStatsByOpponent: {[k in number]?: LeagueGameStats};
}

interface ILeaguePlayerStatsSummary {
  totalWins: number;
  totalGames: number;
  points: number;
}

interface IModifyReportRequest {
  rid: number;
  timestamp: string | null;
  report: RawGameReport;
}

interface INullFilter<T> {
  tag: "NullFilter";
}

interface IProcessedGameReport {
  rid: number;
  timestamp: string;
  winnerId: number;
  winner: string;
  loserId: number;
  loser: string;
  side: Side;
  victory: Victory;
  match: Match;
  competition: Competition[];
  league: League | null;
  expansions: Expansion[];
  treebeard: boolean | null;
  actionTokens: number;
  dwarvenRings: number;
  musterPoints: number;
  turns: number;
  corruption: number;
  mordor: number | null;
  initialEyes: number;
  aragornTurn: number | null;
  strongholds: Stronghold[];
  interestRating: number;
  comment: string | null;
  logFile: string | null;
}

interface IRawGameReport {
  winner: string;
  loser: string;
  side: Side;
  victory: Victory;
  match: Match;
  competition: Competition[];
  league: League | null;
  expansions: Expansion[];
  treebeard: boolean | null;
  actionTokens: number;
  dwarvenRings: number;
  musterPoints: number;
  turns: number;
  corruption: number;
  mordor: number | null;
  initialEyes: number;
  aragornTurn: number | null;
  strongholds: Stronghold[];
  interestRating: number;
  comment: string | null;
}

interface IRemapPlayerRequest {
  fromPid: number;
  toPid: number;
}

interface IRemapPlayerResponse {
  name: string;
}

interface ISubmitGameReportResponse {
  report: ProcessedGameReport;
  winnerRating: number;
  loserRating: number;
}

interface ISubmitReportRequest {
  report: RawGameReport;
  logFile: File | null;
}

interface IUserInfoResponse {
  isAdmin: boolean;
}

interface IValueFilter<T> {
  tag: "ValueFilter";
  contents: T;
}

interface IVictoryComboFilter {
  tag: "VictoryComboFilter";
  contents: [Side, Victory];
}

interface IVictoryKindFilter {
  tag: "VictoryKindFilter";
  contents: Victory;
}

interface IVictorySideFilter {
  tag: "VictorySideFilter";
  contents: Side;
}

type Competition = "League" | "Tournament";

type DeleteReportRequest = IDeleteReportRequest;

type EditPlayerRequest = IEditPlayerRequest;

type Expansion = "LoME" | "WoME" | "KoME" | "Cities" | "FateOfErebor" | "Treebeard";

type GameReportFilterSpec = IGameReportFilterSpec;

type GetLeaderboardResponse = IGetLeaderboardResponse;

type GetReportsResponse = IGetReportsResponse;

type IInequalityFilter = [ApiInequalityOperator, number];

type InequalityFilter = IInequalityFilter;

type LeaderboardEntry = ILeaderboardEntry;

type League = "GeneralLeague" | "LoMELeague" | "WoMELeague" | "SuperLeague" | "TTSLeague";

type LeagueGameStats = ILeagueGameStats;

type LeaguePlayerStats = ILeaguePlayerStats;

type LeaguePlayerStatsSummary = ILeaguePlayerStatsSummary;

type LeagueTier = "Tier1" | "Tier2" | "Tier3";

type Match = "Rated" | "Unrated";

type ModifyReportRequest = IModifyReportRequest;

type NullableFilter<T> = INullFilter<T> | IValueFilter<T>;

type ProcessedGameReport = IProcessedGameReport;

type RawGameReport = IRawGameReport;

type RemapPlayerRequest = IRemapPlayerRequest;

type RemapPlayerResponse = IRemapPlayerResponse;

type Side = "Free" | "Shadow";

type Stronghold = "Rivendell" | "GreyHavens" | "HelmsDeep" | "Lorien" | "WoodlandRealm" | "Erebor" | "MinasTirith" | "DolAmroth" | "Shire" | "Edoras" | "Dale" | "Pelargir" | "EredLuin" | "IronHills" | "MountGundabad" | "Angmar" | "Moria" | "DolGuldur" | "Orthanc" | "Morannon" | "BaradDur" | "MinasMorgul" | "Umbar" | "FarHarad" | "SouthRhun";

type SubmitGameReportResponse = ISubmitGameReportResponse;

type SubmitReportRequest = ISubmitReportRequest;

type TimestampFilter = IBefore | IAfter | IBetween;

type UserInfoResponse = IUserInfoResponse;

type Victory = "Ring" | "Military" | "Concession";

type VictoryFilter = IVictorySideFilter | IVictoryKindFilter | IVictoryComboFilter;

export type ApiInequalityOperator = "GT" | "LT" | "EQ";