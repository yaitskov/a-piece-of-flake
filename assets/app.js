/* a piece of flake */


const submitFlake = (url) => {
  console.log(`Sumbitting flake ${url}`);
  var xhr = new XMLHttpRequest();
  xhr.open("POST", "submit-flake", true);
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.send(JSON.stringify(url));

  return false;
};
