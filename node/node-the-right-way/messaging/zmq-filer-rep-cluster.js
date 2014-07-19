/**
 * Zmq Router / Dealer and Cluster Templates
 *
 */

'use strict';

const
  cluster = require('cluster'),
  fs = require('fs'),
  zmq = require('zmq');

if (cluster.isMaster) {
  
  // |*| : Master process - create ROUTER and DEALER sockets, bind endpoints
  //
  let
    router = zmq.socket('router').bind('tcp://127.0.0.1:5433'),     // 'localhost tcp' end-point.
    dealer = zmq.socket('dealer').bind('ipc://filer-dealer.ipc');   // 'inter-process connection' end-point.
  
  // |*| : Forward messages between router and dealer
  //
  router.on('message', function() {
    let frames = Array.prototype.slice.call(arguments);
    dealer.send(frames);
  });
  
  dealer.on('message', function() {
    let frames = Array.prototype.slice.call(null, arguments);
    router.send(frames);
  });
  
  // |*| : Listen for workers to come online
  //
  cluster.on('online', function(worker) {
    console.log('Worker ' + worker.process.pid + ' is online.');
  });
  
  // |*| : Fork three worker processes
  //
  for (let i = 0; i < 3; i++) {
    cluster.fork();
  }
  
} else {
  
  // |*| : Worker process - create REP socket, connect to DEALER
  //
  let responder = zmq.socket('rep').connect('ipc://filer-dealer.ipc');  // 'inter-process connection' end-point.
  
  responder.on('message', function(data) {
    
    // |*| : Parse incoming message
    //
    let request = JSON.parse(data);
    console.log(process.pid + ' received request for: ' + request.path);
    
    // |*| : Read file and reply with content
    //
    fs.readFile(request.path, function(err, data) {
      console.log(process.pid + ' sending response');
      responder.send(JSON.stringify({
        pid: process.pid,
        data: data.toString(),
        timestamp: Date.now()
      }));
    });
    
  });
  
}