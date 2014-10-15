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
    // The '/login' route loads a 'templateUrl'. The reason it doesn’t specify  
    // a controller in the route configuration is because the HTML defines the 
    // controller using the 'ng-controller' syntax. For each route, we can 
    // decide to include the controller directly in the HTML or using the 
    // controller configuration in the route.
    //
    .when('/login', {
      templateUrl: 'views/login.html'
    })
    // The largest and most complex of the routes is the Team Detail route. 
    // It is defined as '/team/:code'. This tells AngularJS that as part of the 
    // URL, take everything after '/team/' and make it available to the 
    // controller in case it requires it as the variable code in 
    // '$routeParams'.
    //
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
    // Lastly, there is an otherwise route, which redirects the user to the 
    // '/ 'route if the user enters a URL that the route configuration 
    // doesn’t recognize.
    //
    $routeProvider.otherwise({
      redirectTo: '/'
    });
  });



