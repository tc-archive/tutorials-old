/**
 * A service for monitoring a specified file. 
 * 
 * 1) Uses a Unix socket based connection.
 * 2) Writes unformated 'strings' to connected clients on file change.
 */
'use strict';

const
	fs = require('fs'), 
	net = require('net'),
	filename = process.argv[2],	// Command line parameter for target file.
	server = net.createServer(function(connection) {

		// |*| : Reporting
		//
		console.log('Subscriber connected.');
		connection.write("Now watching '" + filename + "' for changes...\n");

		// |*| : Watcher setup
		//
		let watcher = fs.watch(filename, function() {
			connection.write("File '" + filename + "' changed: " + Date.now() + "\n");
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


// |*| : Create a Unix SOCK socket called 'watcher.sock.
//
//       To conntect to server: nc -U /tmp/watcher.sock
//
server.listen('/tmp/watcher.sock', function() {
  console.log('Listening for subscribers...');
});
