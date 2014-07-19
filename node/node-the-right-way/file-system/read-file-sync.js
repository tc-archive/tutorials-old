/**
 * Read the specified file with a blocking read and write the output stream.
 */
const 
	fs = require('fs'),
	data = fs.readFileSync('target.txt'); process.stdout.write(data.toString());