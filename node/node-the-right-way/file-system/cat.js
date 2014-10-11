#!/usr/bin/env node --harmony 
/**
 * Reads a file stream and 'pipes' it to the standard ouput.
 */
require('fs').createReadStream(process.argv[2]).pipe(process.stdout);
