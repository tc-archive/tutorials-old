/**
 * A service for monitoring a specified file. 
 * 
 * 1) Uses a TCP socket based connection.
 * 2) Writes JSON formatted 'strings' to connected clients on file change.
 *
 * WARNING: Does not buffer inputs! Any data arriving as multiple events will crash the server.
 */
'use strict';

const
	fs = require('fs'),
	net = require('net'),
	filename = process.argv[2], // Command line parameter for target file.
	server = net.createServer(function(connection) {

		// |*| : Reporting
		//
		console.log('Subscriber connected.');
		connection.write(
			JSON.stringify({
				type: 'watching',
				file: filename
			}) + '\n'
		);

		// |*| : Watcher setup
		//
		let watcher = fs.watch(filename, function() {

			connection.write(JSON.stringify({
				type: 'changed',
				file: filename,
				timestamp: Date.now()
			}) + '\n');
		});

		// |*| : Cleanup
		//
		connection.on('close', function() {
			console.log('Subscriber disconnected.');
			watcher.close();
		});

	});


if (!filename) {
	throw Error('No target filename was specified.');
}


// |*| : Create a TCP socket and listen on port 5432.
//
//       To conntect to server: telnet localhost 5432
//
server.listen(5432, function() {
	console.log('Listening for subscribers...');
});