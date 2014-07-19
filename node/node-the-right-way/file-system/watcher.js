/**
 * A simple program that monitors a specified file 'target.txt' on the file system.
 */
const fs = require('fs');

// |*| : Watch the specfied target file for changes.
//
fs.watch('target.txt', function() {
	console.log("File 'target.txt' just changed!");
});

console.log("Now watching target.txt for changes...");