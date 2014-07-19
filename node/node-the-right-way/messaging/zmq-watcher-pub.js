/**
 * File Watch Publisher (server)
 *
 * A service for monitoring a specified file. 
 * 
 * 1) Uses a ZMQ 'pub' (TCP) socket based connection.
 * 2) Writes JSON formatted 'strings' to connected clients on file change.
 *
 * NB: ZMQ deals with message boundaries, reconnection, etc.
 */
'use strict';

const
	fs = require('fs'), 
	zmq = require('zmq'),
	publisher = zmq.socket('pub'), 		// Create publisher endpoint.
	filename = process.argv[2];

fs.watch(filename, function() {
	// send message to any subscribers
	publisher.send(JSON.stringify({
		type: 'changed',
		file: filename,
		timestamp: Date.now()
	}));
});


// listen on TCP port 5432
publisher.bind('tcp://*:5432', function(err) {
	console.log('Listening for zmq subscribers...');
});