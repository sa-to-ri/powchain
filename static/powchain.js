async function request(method, path, data = null, content_type = null) {
  return await fetch(path, {
    method: method,
    headers: {
      "Content-Type": content_type,
    },
    body: data,
  })
  .catch(e => console.error("Request failed."));
}

async function loadTab(tab_id) {
  const tabs    = ["/chain", "/transactions", "/peers"];
  const div     = document.getElementById("contents");
  const content = await getView(tabs[tab_id]);

  div.innerHTML = content;
}

async function getView(url) {
  return request("GET", url)
    .then(response => response.text());
}

async function startMining() {
  const response = await request("GET", "/mine");
  
  if (!response.ok) {
    console.error(response);
    alert(response.statusText);
  } else {
    loadTab(0);
  }
}

function toggleCollapse(id) {
  toggleElement("l2_" + id);
  toggleElement("l3_" + id);  
}

function toggleElement(name) {
  var elems = document.getElementsByClassName(name);
    
  for (var i = 0; i < elems.length; i++) {
    var elem = elems[i];
    
    if (elem.style.display === "none") {
      console.log("showing");
      elem.style.display = "";
    } else {
      console.log("hiding");
      elem.style.display = "none";
    }
  }
}

async function postTransaction() {
  try {
    const timestamp = Date.now();
    const sender    = document.getElementById("tx_sender").value;
    const recipient = document.getElementById("tx_recipient").value;
    const amount    = Number(document.getElementById("tx_amount").value);
    const data      = JSON.stringify({"timestamp": timestamp, "sender": sender, "recipient": recipient, "amount": amount});
  
    const response  = await request("POST", "/transactions", data, "application/json")
  
    if (!response.ok) {
      console.error(response);
      alert(response.statusText);
    } else {
      loadTab(1);
    }
  } catch (e) {
    console.error("Transaction could not be parsed.");
  }
}

async function postPeer() {
  try {
    const peer     = JSON.stringify(document.getElementById("peer").value);
    const response = await request("POST", "/peers", peer, "application/json");
    
    if (!response.ok) {
      console.error(response);
      alert(response.statusText);
    } else {
      loadTab(2);
    }
  } catch (e) {
     console.error("Peer could not be parsed.");
  }
}

async function deletePeer(peer) {
  try {
    const response = await request("DELETE", "/peers/" + peer);
    
    if (!response.ok) {
      console.error(response);
      alert(response.statusText);
    } else {
      loadTab(2);
    }
  } catch (e) {
     console.error("Peer could not be parsed.");
  }
}
