// File: chapter10/routing-example/app/scripts/services.js
angular.module('fifaApp')
  // The 'FifaService' gets the 'team list'; and 'individual team 
  // data' via the $http service.
  //
  .factory('FifaService', ['$http',
    function($http) {
      return {
        // GET on /api/team returns the list of teams in the system as 
        // an array.
        getTeams: function() {
          return $http.get('/api/team');
        },
        // GET on /api/team/:code, with code being the code of the 
        // team, returns the details of a particular team as a single 
        // object.
        getTeamDetails: function(code) {
          return $http.get('/api/team/' + code);
        }
      }
  }])
    // The 'UserService' gets manages whether the user is logged on, get 
    // the 'user session' and allows loging via the $http service.
  //
  .factory('UserService', ['$http', function($http) {
    var service = {
      isLoggedIn: false,

      // GET on /api/session returns either a 400 status if the user is 
      // not logged in, or an object with the user details if he is logged 
      // in. This enables the client (the web application) to verify that 
      // the user is logged in to the server.
      session: function() {
        return $http.get('/api/session')
              .then(function(response) {
          service.isLoggedIn = true;
          return response;
        });
      },

      // POST on /api/login with POST data containing {username: 'myuser', 
      // password: 'mypassword'} will try to log in the user. If successful, 
      // it returns what the session call returns. Otherwise, it returns a 
      // status 400 error if the user authentication fails with the msg field 
      // in the object containing the reason for the failure.
      login: function(user) {
        return $http.post('/api/login', user)
          .then(function(response) {
            service.isLoggedIn = true;
            return response;
        });
      }
    };
    return service;
  }]);
