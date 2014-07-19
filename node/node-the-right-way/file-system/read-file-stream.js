/**
 * Read the specified file as a stream of 'data' events and write them to standard out.
 */
const 
	fs = require('fs'),
	stream = fs.createReadStream(process.argv[2]);

stream.on('data', function(chunk) {
	process.stdout.write(chunk);
});

stream.on('error', function(err) {
	process.stderr.write("ERROR: " + err.message + "\n");
});