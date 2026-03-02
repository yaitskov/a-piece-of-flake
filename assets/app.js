/* a piece of flake
   github:yaitskov/add-dependent-file
 */
const flush = Math.random() > 0.36 ? new Audio("/snow.mp3") : new Audio("/flush.mp3");

const submitFlake = (url) => {
  if (url == "" || url.length < 6) {
    console.log(`Flake url [${url}] is too short`);
    return false;
  }

  console.log(`Sumbitting flake ${url}`);
  var xhr = new XMLHttpRequest();
  xhr.open("POST", "submit-flake", true);
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.send(JSON.stringify(url));
  const submitionForm = document.getElementById("submition-form");
  const urlInput = document.getElementById("flake-url");
  xhr.onload = (e) => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        console.log(xhr.responseText);
        try {
          const flake = JSON.parse(xhr.responseText);
          console.log(`${flake}`);
          submitionForm.className = '';
          flush.play();
          switch(flake.tag) {
          case "SubmittedFlake":
            document.querySelector('.SubmittedFlake .date').innerText = flake.submittedAt;
            document.querySelector('.SubmittedFlake a').href = flake.flakeUrl;
            submitionForm.className = 'SubmittedFlake';
            break;
          case "FlakeIsBeingFetched":
            document.querySelector('.FlakeIsBeingFetched .FlakeIsBeingFetched .date').innerText = flake.submitionFetchedAt;
            submitionForm.className = 'FlakeIsBeingFetched';
            break;
          case "BadFlake":
            document.querySelector('.BadFlake .date').innerText = flake.fetcherRespondedAt;
            document.querySelector('.BadFlake .error').innerText = flake.error;
            submitionForm.className = 'BadFlake';
            break;
          case "FlakeFetched":
            document.querySelector('.FlakeFetched .date').innerText = flake.uploadedAt;
            document.querySelector('.FlakeFetched a').href = flake.flakeUrl;
            submitionForm.className = 'FlakeFetched';
            break;
          case "FlakeIndexed":
            document.querySelector('.FlakeIndexed .date').innerText = flake.indexedAt;
            document.querySelector('.FlakeIndexed a').href = flake.flakeUrl;
            submitionForm.className = 'FlakeIndexed';
            break;
          default:
            document.querySelector('.BadFlake .error').innerText = `Unkown flake state ${flake.tag}`;
            submitionForm.className = 'BadFlake';
          }
        } catch (je) {
          console.error(`Failed to parse response as JSON ${je}`);
        }

        // url.value = "";
      } else {
        console.error(`Submition of flake ${url} has failed: ${xhr.statusText}`);
      }
    }
  };
  xhr.onerror = (e) => {
    console.error(`Submition of flake ${url} has failed: ${xhr.statusText}`);
  };
  return false;
};
