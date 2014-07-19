/**
 * Network File Writer (Client/Requester)
 *
 * A service for monitoring a specified file. 
 * 
 * 1) Uses a ZMQ 'rep' (TCP) socket based connection.
 * 2) Writes JSON formatted 'strings' to connected clients on file change.
 *
 * NB: Program 'loops' to showing REQ/RES socket syncronous blocking.
 *
 * NB: ZMQ deals with message boundaries, reconnection, etc.
 */
"use strict";
const
  zmq = require('zmq'),
  filename = process.argv[2],
  requester = zmq.socket('req');	  // Create request endpoint

// |*| : Handle replies from responder
//
requester.on("message", function(data) {
  let response = JSON.parse(data);
  console.log("Received response:", response);
});
requester.connect("tcp://localhost:5433");

// |*| : Send request for content
//
for (let i = 0; i <= 3, i++) {
  console.log('Sending request for ' + filename);
  requester.send(JSON.stringify({
  path: filename
  }));
}