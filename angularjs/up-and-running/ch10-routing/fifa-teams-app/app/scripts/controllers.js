// File: chapter10/routing-example/app/scripts/controllers.js
angular.module('fifaApp')

  // MainCtrl is used to handle the top navigation bar, and basically exposes 
  // the User Service to the view so that the HTML can hide and show the 
  // login/logout links depending on the user’s state. It also makes a call to 
  // the server to see if the user is logged in when the application loads.
  //
  .controller('MainCtrl', ['UserService',
    function(UserService) {
      var self = this;
      self.userService = UserService;

      // Check if the user is logged in when the application
      // loads
      // User Service will automatically update isLoggedIn
      // after this call finishes
      UserService.session();
  }])

  // TeamListCtrl is used for the landing route, and just uses the FifaService 
  // to fetch a list of teams when it loads. It then exposes this data for the 
  // view to display.
  //
  .controller('TeamListCtrl', ['FifaService',
    function(FifaService) {
      var self = this;
      self.teams = [];

      FifaService.getTeams().then(function(resp) {
        self.teams = resp.data;
      });
  }])

  // LoginCtrl has only a function to let the user log in when he fills in his 
  // username and password and clicks the login button. If the login is 
  // successful, it redirects the user to the home page. In the case of an 
  // error, it shows an error message in the UI.
  //
  .controller('LoginCtrl', ['UserService', '$location',
    function(UserService, $location) {
      var self = this;
      self.user = {username: '', password: ''};

      self.login = function() {
        UserService.login(self.user).then(function(success) {
          $location.path('/');
        }, function(error) {
          self.errorMessage = error.data.msg;
        })
      };
  }])

  // The TeamDetailsCtrl is the only one that does something unique and 
  // interesting, in that it loads a specific team based on the route. Suppose 
  // the user navigates to http://localhost:8000/#/team/ESP. This triggers the 
  // route that loads TeamDetailsCtrl. Now TeamDetailsCtrl has to figure out 
  // which team it has been loaded for. It can access this information from a 
  // service known as $routeParams. $route Params has the current team’s code 
  // set in it at the key code. This is set up via routing. It then loads the 
  // team details from the server based on this code from the URL.
  //
  .controller('TeamDetailsCtrl',
    ['$location', '$routeParams', 'FifaService',
    function($location, $routeParams, FifaService) {
      var self = this;
      self.team = {};
      FifaService.getTeamDetails($routeParams.code)
          .then(function(resp){
        self.team = resp.data;
      }, function(error){
        $location.path('/login');
      });
    }]);
