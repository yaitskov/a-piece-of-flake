/* a piece of flake
   github:yaitskov/add-dependent-file
*/

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
  var xhr = new XMLHttpRequest();
  xhr.open("POST", "find-flakes", true);
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.send(JSON.stringify({skipBroken: false, searchPattern: tokenize(pattern)}));
  xhr.onload = (e) => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        try {
          const foundflakeUrls = JSON.parse(xhr.responseText);
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
        } catch (je) {
          errorOutput.innerText = `Failed to parse response as JSON: ${je}`;
          errorOutput.parentNode.className = "";
        }
      } else {
        errorOutput.innerText = `Status is not 200 but ${xhr.statusText}`;
        errorOutput.parentNode.className = "";
      }
    } else {
      errorOutput.innerText = `Ready statuis is not 4: ${xhr.statusText}`;
      errorOutput.parentNode.className = "";
    }
  };
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
  const xhr = new XMLHttpRequest();
  xhr.open("POST", "submit-flake", true);
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.send(JSON.stringify(url));
  const sumbittedNotification = document.getElementById("sumbitted-notification");
  const errorOutputHid = document.getElementById("error-output-hid");
  const errorOutput = document.getElementById("error-output");
  const flakeLink = document.getElementById("flake-link");
  flakeLink.setAttribute('href', '#');
  errorOutput.innerText = "";
  errorOutputHid.className = "is-hidden";
  sumbittedNotification.className = "is-hidden";
  xhr.onload = (e) => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        console.log(xhr.responseText);
        try {
          const flake = JSON.parse(xhr.responseText);
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
        } catch (je) {
          errorOutput.innerText = `Failed to parse response as JSON ${je}`;
          errorOutputHid.className = '';
        }
      } else {
        errorOutput.innerText = `Submition of flake ${url} has failed: ${xhr.statusText}`;
        errorOutputHid.className = '';
      }
    }
  };
  xhr.onerror = (e) => {
    errorOutput.innerText = `Submition of flake ${url} has failed: ${xhr.statusText}`;
    errorOutputHid.className = '';
  };
  return false;
};
