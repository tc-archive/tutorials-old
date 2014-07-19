/**
 * Read the specified files and write it to the output stream.
 */
const fs = require('fs');

fs.readFile('target.txt', function(err, data) {
	if (err) {
		throw err;
	}
	console.log(data.toString());
});