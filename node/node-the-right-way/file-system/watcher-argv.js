/**
 * A simple program that monitors a user command line specified file on the file system.
 */
const 
	fs = require('fs'),
	filename = process.argv[2];				// The third (and subsequent parameters) are user supplied cl parameters

console.log("Program:" + process.argv[0]);	// The first paramter is the excution program.
console.log("Source :" + process.argv[1]);	// The seconf paramter is the execute node source file.

if (!filename) {
	throw Error("A file to watch must be specified!");
}

// |*| : Watch the specfied target file for changes.
//
fs.watch(filename, function() {
	console.log("File " + filename + " just changed!");
});

console.log("Now watching " + filename + " for changes...");