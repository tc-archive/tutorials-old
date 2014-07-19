/**
 * A module to handle 'line delimitted JSON messages'.
 *
 * Uses an imported 'LDJClient' constructor function to create a client that buffers data input 
 * into carriage return delimitted JSON messages. 
 *
 * NB: Does not handle 'message boundaries'.
 *     Does not check 'error' or 'close' events.
 */
"use strict";
const
  net = require('net'),
  ldj = require('./ldj.js'),
  
  netClient = net.connect({ port: 5432 }),
  ldjClient = ldj.connect(netClient);

ldjClient.on('message', function(message) {
  if (message.type === 'watching') {
    console.log("Now watching: " + message.file);
  } 
  else if (message.type === 'changed') {
    console.log(
      "File '" + message.file + "' changed at " + new Date(message.timestamp)
    );
  } 
  else {
    throw Error("Unrecognized message type: " + message.type);
  }
});