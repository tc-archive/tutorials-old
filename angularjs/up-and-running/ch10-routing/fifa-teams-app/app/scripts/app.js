// File: chapter10/routing-example/app/scripts/app.js

// We define our application (and create our module) in the app.js file. We 
// define our 'fifaApp' module, and specify that it depends on the 'ngRoute' 
// module so that we can use routing in our application.
//
// This is where the primary 'router' for the application is defined.
//
angular.module('fifaApp', ['ngRoute'])
  .config(function($routeProvider) {

    // The '/' route introduces nothing new, in that it loads a 'templateUrl'  
    // and attaches the 'TeamListCtrl' to it when it loads. There are no 
    // variables in the URL, nor any access control checks.
    //
    $routeProvider.when('/', {
      templateUrl: 'views/team_list.html',
      controller: 'TeamListCtrl as teamListCtrl'
    })
    .when('/login', {
      templateUrl: 'views/login.html'
    })
    .when('/team/:code', {
      templateUrl: 'views/team_details.html',
      controller:'TeamDetailsCtrl as teamDetailsCtrl',
      resolve: {
        auth: ['$q', '$location', 'UserService',
          function($q, $location, UserService) {
             return UserService.session().then(
               function(success) {},
               function(err) {
                  $location.path('/login');
                  $location.replace();
                  return $q.reject(err);
             });
        }]
      }
    });
    $routeProvider.otherwise({
      redirectTo: '/'
    });
  });
