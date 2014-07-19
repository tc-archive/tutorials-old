/**
 * Bind a tcp socket to a specified port, and listening for incoming connections.
 */
"use strict"; 
const
	net = require('net'),
	server = net.createServer(function(connection) {
    // use connection object for data transfer
  	});

server.listen(5432);
