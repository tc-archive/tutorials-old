/**
 * A module to handle 'line delimitted JSON messages'.
 *
 * Exports an 'LDJClient' constructor function for creating clients that buffer data input 
 * into carriage return delimitted JSON messages. 
 *
 * NB: Does not handle 'message boundaries'.
 *     Does not check 'error' or 'close' events.
 */
"use strict";

const 
	events = require('events'),
	 util = require('util'), 
	 // |*| : LDJClient constructor
	 //		  Param 'stream' : Data emitting object; socket.
	 LDJClient = function(stream) {

	 	events.EventEmitter.call(this); 	// Call the consturctor for EventEmiiter. Best Practice. Also, see inherits below...

	 	let self = this,		// Bind this to self on initial invocation.
			buffer = '';

		stream.on('data', function(data) {
			
			// |*| : Handle 'partial messages' by awaiting the terminating carriage return.
			//
			buffer += data;
			let boundary = buffer.indexOf('\n'); 

			// |*| : Handle 1 or 'multiple messages' by processing all terminating carriages 
			//       returns.
			//
			while (boundary !== -1) {
				let input = buffer.substr(0, boundary);

				buffer = buffer.substr(boundary + 1); 
				self.emit('message', JSON.parse(input)); 
				boundary = buffer.indexOf('\n');
			} 
		});
	 }; 

// Extend LDJClient from EventEmitter
util.inherits(LDJClient, events.EventEmitter);


// |*| : Export module methods
//
exports.LDJClient = LDJClient; 
	exports.connect = function(stream){
	return new LDJClient(stream); 
};


// |*| : Prototypical Usage Code
//
// const client = new LDJClient(networkStream); client.on('message', function(message) {
//   // take action for this message
// });
//
//
// |*| : Prototypical Module Usage Code
//
// const ldj = require('./ldj.js'),
// 	     client = ldj.connect(networkStream);
//
// client.on('message', function(message) { 
// 	// take action for this message
// });


