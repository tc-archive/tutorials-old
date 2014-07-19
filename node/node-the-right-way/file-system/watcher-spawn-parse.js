/**
 * A simple program that invokes an os 'ls' command on the specified file in a new 
 process and then pipes the results (target file details) to 'stdout'.
 */
"use strict";

const 
	fs = require('fs'),
	spawn = require('child_process').spawn, 
	filename = process.argv[2];

// |*| : Handle missing input parameters.
//
if (!filename) {
	throw Error("A file to watch must be specified!");
}

// |*| : Watch the specfied target file for changes.
//
fs.watch(filename, function() {
	let 
		ls = spawn('ls', ['-lh', filename]),	// Spawn a new process performing an 'ls' cmooand on the specified taregt file.
		output = '';							// A buffer for the ouput of the 'ls' command.

	// |*| : Register a 'data' event listener 
	//		 On 'data' events; append the data to the ouput buffer.
	//
	ls.stdout.on('data', function(chunk) {		// 'chunk' is a Buffer object.
		output += chunk.toString();
	});

	// |*| : Register a 'close' event listener
	//		 On 'close' event; split the output 
	//
	ls.on('close', function() {
		let parts = output.split(/\s+/);
		console.dir([parts[0], parts[4], parts[8]]);
	});

});

console.log("Now watching " + filename + " for changes...");
