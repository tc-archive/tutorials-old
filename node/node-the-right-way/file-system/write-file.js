/**
 * Writes to the specified string to the specified file.
 */
const fs = require('fs');

fs.writeFile('target.txt', 'Tim is the best!\nNu is the best!\n', function(err) {
	if (err) {
		throw err;
	}
	console.log("File saved!");
});