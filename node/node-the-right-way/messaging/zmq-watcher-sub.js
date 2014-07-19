/**
 * File Watch Subscriber (client)
 *
 * A client service for monitoring a specified file. 
 * 
 * 1) Uses a ZMQ 'pub' (TCP) socket based connection.
 * 2) Reads JSON formatted 'strings' from connected servers on file change.
 *
 * NB: ZMQ deals with message boundaries, reconnection, etc.
 */
"use strict";

const zmq = require('zmq'),

// create subscriber endpoint
subscriber = zmq.socket('sub'); 
subscriber.subscribe("");			// Subscribe to all messages

// Handle messages from publisher
subscriber.on("message", function(data) {
	let message = JSON.parse(data),
		date = new Date(message.timestamp);

	console.log("File '" + message.file + "' changed at " + date);
});

// Connect to publisher
subscriber.connect("tcp://localhost:5432");