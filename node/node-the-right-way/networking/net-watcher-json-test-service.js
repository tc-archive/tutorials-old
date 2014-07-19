/**
 * A test for highlightying the 'message boundary' problem.
 Fakes spying on a file, but, return the fake update in two chunks.
 * 
 * 1) Uses a TCP socket based connection.
 * 2) Writes JSON formatted 'strings' to connected clients on file change.
 */
"use strict";
const
  
  net = require('net'),
  
  server = net.createServer(function(connection) {
    
    console.log('Subscriber connected');
    
    // send the first chunk immediately
    connection.write(
      '{"type":"changed","file":"targ'
    );
    
    // after a one second delay, send the other chunk
    let timer = setTimeout(function(){
      connection.write('et.txt","timestamp":1358175758495}' + "\n");
      connection.end();
    }, 1000);
    
    // clear timer when the connection ends
    connection.on('end', function(){
      clearTimeout(timer);
      console.log('Subscriber disconnected');
    });
    
  });

server.listen(5432, function() {
  console.log('Test server listening for subscribers...');
});


// |*| : Check with client to see error
// node --harmony net-watcher-json-client.js
//
// undefined:1
// {"type":"changed","file":"targ
//                               ^
// SyntaxError: Unexpected end of input
//     at Object.parse (native)
//     at Socket.<anonymous> (./net-watcher-json-client.js:10:20)
//     at Socket.EventEmitter.emit (events.js:96:17)
//     at TCP.onread (net.js:397:14)