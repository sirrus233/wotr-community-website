import queryString from "query-string";


export function postAddLeaguePlayer(league?: League, tier?: LeagueTier, year?: number, playerId?: number, playerName?: string, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<void> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  let params = {league, tier, year, playerId, playerName};
  return (fetchFn || window.fetch)(`/addLeaguePlayer` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        resolve();
      }
    });
  });
}

export function postDeleteReport(body: DeleteReportRequest, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<void> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/deleteReport` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        resolve();
      }
    });
  });
}

export function postModifyReport(body: ModifyReportRequest, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<void> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/modifyReport` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        resolve();
      }
    });
  });
}

export function postRemapPlayer(body: RemapPlayerRequest, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<RemapPlayerResponse> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/remapPlayer` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}

export function postEditPlayer(body: EditPlayerRequest, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<void> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/editPlayer` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        resolve();
      }
    });
  });
}

export function postLogout(fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<void> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  let params = {};
  return (fetchFn || window.fetch)(`/logout` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        resolve();
      }
    });
  });
}

export function postUpdateActiveStatus(fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<void> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  let params = {};
  return (fetchFn || window.fetch)(`/updateActiveStatus` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        resolve();
      }
    });
  });
}

export function getLeagueStats(league?: League, tier?: LeagueTier, year?: number, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<{[k in number]?: LeaguePlayerStats}> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "GET",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  let params = {league, tier, year};
  return (fetchFn || window.fetch)(`/leagueStats` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}

export function getLeaderboard(year?: number, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<GetLeaderboardResponse> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "GET",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  let params = {year};
  return (fetchFn || window.fetch)(`/leaderboard` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}

export function getReports(limit?: number, offset?: number, filter?: GameReportFilterSpec, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<GetReportsResponse> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "GET",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  let params = {limit, offset, filter};
  return (fetchFn || window.fetch)(`/reports` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}

export function postSubmitReport(body: SubmitReportRequest, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<SubmitGameReportResponse> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/submitReport` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}

export function getUserInfo(fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<UserInfoResponse> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "GET",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  let params = {};
  return (fetchFn || window.fetch)(`/userInfo` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}