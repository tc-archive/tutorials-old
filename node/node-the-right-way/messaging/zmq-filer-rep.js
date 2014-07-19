/**
 * Network File Writer (Server/Requestee)
 *
 * A service for monitoring a specified file. 
 * 
 * 1) Uses a ZMQ 'rep' (TCP) socket based connection.
 * 2) Writes JSON formatted 'strings' to connected clients on file change.
 *
 * NB: ZMQ deals with message boundaries, reconnection, etc.
 */
'use strict';

const
  fs = require('fs'),
  zmq = require('zmq'),
  responder = zmq.socket('rep'); // Socket to reply to client requests

// |*| : Handle incoming requests
//
responder.on('message', function(data) {
  
  // |*| : Parse incoming message
  //
  let request = JSON.parse(data);
  console.log('Received request to get: ' + request.path);
  
  // |*| : Read file and reply with content
  //
  fs.readFile(request.path, function(err, content) {
    console.log('Sending response content');
    responder.send(JSON.stringify({
      content: content.toString(),
      timestamp: Date.now(),
      pid: process.pid
    }));
  });
  
});

// |*| : Listen on TCP port 5433
//
responder.bind('tcp://127.0.0.1:5433', function(err) {
  console.log('Listening for zmq requesters...');
});

// Close the responder when the Node process ends...
process.on('SIGINT', function() {
  console.log('Shutting down...');
  responder.close();
});
