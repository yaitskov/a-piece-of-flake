/* a piece of flake
   github:yaitskov/add-dependent-file
*/
const xhrError = (xhr) => {
  if (xhr.responseText && xhr.responseText.length > 1) {
    try {
      const respO = JSON.parse(xhr.responseText);
      if (respO.errors instanceof Array) {
        return respO.errors.join("\n");
      }
    } catch (e) {
      console.error(`${xhr.responseText} => ${e}`);
    }
    return xhr.responseText;
  } else {
    if (xhr.statusText && xhr.statusText.length > 1) {
      return xhr.statusText;
    } else {
      return "Server is down or it is offline";
    }
  }
};

const xhrPost = (urlPath, payload, okCb, opsCb) => {
  const xhr = new XMLHttpRequest();
  xhr.open("POST", urlPath, true);
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.setRequestHeader('Accept', 'application/json');
  xhr.send(JSON.stringify(payload));

  xhr.onload = (e) => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        var r = null;
        try {
          r = JSON.parse(xhr.responseText);
        } catch (je) {
          opsCb(`Failed to parse response as JSON: ${je}`);
          return;
        }
        okCb(r);
        } else {
          opsCb(xhrError(xhr));
        }
    } else {
      opsCb(xhrError(xhr));
    }
  };
  xhr.onerror = (e) => {
    opsCb(`${e} => ${xhrError(xhr)}`);
  };
  return xhr;
};

const ready = () => {
  console.log(`Document ready hook is triggered`);
  const navBarBurger = document.querySelector('#navbar-burger');
  const navBarBurgerCss = navBarBurger.className;
  const navBarMenu = document.querySelector('#navbar-menu');
  const navBarMenuCss = navBarMenu.className;

  navBarBurger.addEventListener(
    "click", (e) => {
      if (navBarMenu.className.indexOf("is-active") < 0) {
        navBarMenu.className = `${navBarMenu.className} is-active`;
        navBarBurger.className = `${navBarBurger.className} is-active`;
      } else {
        navBarMenu.className = navBarMenuCss;
        navBarBurger.className = navBarBurgerCss;
      }
    }
  );
};

document.addEventListener("DOMContentLoaded", ready);

const flush = Math.random() >= 0.5 ? new Audio("/snow.mp3")
      : (Math.random() >= 0.5  ?  new Audio("/avalanche.mp3") : new Audio("/flush.mp3"));

const tokenize = s => s.split(/[ \t\n]/).filter(w => w.length > 0);

const flakeViewUrl = (fu) => `/flake/${encodeURIComponent(fu)}`;

const searchFlakesBy = (pattern) => {
  const flakesTbody = document.querySelector('#search-results');
  const foundFlakes = document.querySelector('#found-flakes');
  const errorOutput = document.querySelector('#error-output');
  const noFlakesFound = document.querySelector('#no-flakes-found');
  flakesTbody.innerHTML = "";
  errorOutput.innerHTML = "";
  errorOutput.parentNode.className = "is-hidden";
  foundFlakes.className = "is-hidden";
  noFlakesFound.className = "is-hidden";
  console.log(`Find flakes matching pattern [${pattern}]`);
  xhrPost(
    "find-flakes",
    {skipBroken: false, searchPattern: tokenize(pattern)},
    (foundflakeUrls) => {
      if (foundflakeUrls.length == 0) {
        noFlakesFound.className = "";
      } else {
        foundFlakes.className = "";
      }
      for (const u in foundflakeUrls) {
        const tr = document.createElement("tr");
        const td = document.createElement("td");
        tr.appendChild(td);
        const a = document.createElement("a");
        td.appendChild(a);
        a.setAttribute('href', flakeViewUrl(foundflakeUrls[u]));
        a.innerText = foundflakeUrls[u];
        flakesTbody.appendChild(tr);
      }
    },
    (err) => {
      errorOutput.innerText = err;
      errorOutput.parentNode.className = "";
    });
  return false;
};

const submitFlake = (url) => {
  const badUrl = document.getElementById("bad-url");

  badUrl.className = "is-hidden";
  url = url.trim();
  if (!url.match(/^github:[a-zA-Z0-9._-]+[/][a-zA-Z0-9._-]+$/)) {
    badUrl.className = "";
    return false;
  }

  console.log(`Sumbitting flake ${url}`);

  const sumbittedNotification = document.getElementById("sumbitted-notification");
  const errorOutputHid = document.getElementById("error-output-hid");
  const errorOutput = document.getElementById("error-output");
  const flakeLink = document.getElementById("flake-link");
  flakeLink.setAttribute('href', '#');
  errorOutput.innerText = "";
  errorOutputHid.className = "is-hidden";
  sumbittedNotification.className = "is-hidden";

  xhrPost(
    "submit-flake",
    url,
    (flake) => {
      console.log(`${flake}`);
      switch(flake.tag) {
      case "SubmittedFlake":
      case "FlakeIsBeingFetched":
        /* play sound */
        flush.onended = (e) => {
          window.location = flakeViewUrl(url);
        };
        flush.play();
        flakeLink.setAttribute('href', flakeViewUrl(url));
        sumbittedNotification.className = "";
        break;
      case "FlakeFetched":
      case "FlakeIndexed":
        /* just redirect to flake view page */
        flakeLink.setAttribute('href', flakeViewUrl(url));
        sumbittedNotification.className = "";
        setTimeout(() => window.location = flakeViewUrl(url), 1000);
        break;
      case "BadFlake":
      default:
        errorOutput.innerText = flake.error;
        errorOutputHid.className = '';
      }
    },
    (err) => {
      errorOutput.innerText = `Submition of flake ${url} has failed: ${err}`;
      errorOutputHid.className = '';
    }
  );
  return false;
};
