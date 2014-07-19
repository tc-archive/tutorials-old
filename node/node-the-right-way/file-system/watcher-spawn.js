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
	let lsCmd = spawn('ls', ['-lh', filename]);		// Spawn a new process...
	lsCmd.stdout.pipe(process.stdout);				// Pipe the process output to stdout.
});

console.log("Now watching " + filename + " for changes...");