/**
 * A client to connect to the 'net-watcher-json-service'.
 * 
 * 1) Uses a TCP socket based connection.
 * 2) Processes a JSON formatted 'strings' denoting a file change.
 *
 * NB: Does not handle 'message boundaries'.
 *     Does not check 'error' or 'close' events.
 */
"use strict";

const
	net = require('net'),
	client = net.connect({
		port: 5432
	});

// |*| : On 'data' event.
//
client.on('data', function(data) {

	let message = JSON.parse(data);
	
	if (message.type === 'watching') {
		// |*| : Handle 'watching' type messages.
		//
		console.log("Now watching: " + message.file);
	} 
	else if (message.type === 'changed') {
		// |*| : Handles 'chnaged' type messages.
		//
		let date = new Date(message.timestamp);
		console.log("File '" + message.file + "' changed at " + date);
	} 
	else {
		throw Error("Unrecognized message type: " + message.type);
	}
});