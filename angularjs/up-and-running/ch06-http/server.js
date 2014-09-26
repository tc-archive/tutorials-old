// server.js (Express 4.0)
var express        = require('express');
var morgan         = require('morgan');
var bodyParser     = require('body-parser');
var methodOverride = require('method-override');
var cors           = require('cors')
var app            = express();

app.use(cors());
app.use(express.static(__dirname + '/public'));   // set the static files location /public/img will be /img for users
app.use(morgan('dev'));                           // log every request to the console
app.use(bodyParser());                            // pull information from html in POST
app.use(methodOverride());                        // simulate DELETE and PUT

var router = express.Router();

var notes = [
  {id: 1, label: 'First Note', author: 'Shyam'},
  {id: 2, label: 'Second Note', author: 'Brad'},
  {id: 3, label: 'Middle Note', author: 'Someone'},
  {id: 4, label: 'Last Note', author: 'Shyam'},
  {id: 5, label: 'Really the last Note', author: 'Shyam'}

];


//CORS middleware
// var allowCrossDomain = function(req, res, next) {
//     res.header('Access-Control-Allow-Origin', 'example.com');
//     res.header('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE');
//     res.header('Access-Control-Allow-Headers', 'Content-Type');
//     next();
// }

// app.configure(function() {
  // app.use(allowCrossDomain);
  // app.use(express.static(__dirname + '/public'));   // set the static files location /public/img will be /img for users
  // app.use(morgan('dev'));                           // log every request to the console
  // app.use(bodyParser());                            // pull information from html in POST
  // app.use(methodOverride());                        // simulate DELETE and PUT
// });



function reply(req, res, resp) {
  res.send(res, resp, 200);
}

function reply(req, res, resp, resp_code) {
  // handleCORS(req, res);
  res.send(resp, resp_code);
}


var lastId = 6;



router.options('*', function(req, res) {
  console.log('OPTIONS');
  handleCORS(req, res);
  next();
});


router.get('/note', function(req, res) {
  console.log("GET '/note ...'");
  reply(req, res, notes, 200);
});

router.post('/note', function(req, res) {
  console.log("POST '/note ...'");
  var note = req.body;
  note.id = lastId;
  lastId++;
  notes.push(note);
  reply(req, res, note, 200);
});


router.get('/note/:id', function(req, res) {
  console.log("GET '/note/:id' ...'");
  for (var i = 0; i < notes.length; i++) {
    if (notes[i].id == req.params.id) {
      reply(req, res, notes[i], 200);
      break;
    }
  }
  reply(res, {msg: 'Note not found'}, 400);
});

router.post('/note/:id', function(req, res) {
  console.log("POST '/note/:id' ...'");
  for (var i = 0; i < notes.length; i++) {
    if (notes[i].id == req.params.id) {
      notes[i] = req.body;
      reply(req, res, notes[i], 200);
      break;
    }
  }
  reply(req, res, {msg: 'Note not found'}, 404);
});


router.post('/login', function(req, res) {
  console.log('API LOGIN FOR ', req.body);
  reply(req, res, {msg: 'Login successful for ' + req.body.username}, 200);
});


app.use('/api', router);


app.listen(8000);
console.log('Open http://localhost:8000 to access the files now');      // shoutout to the user


