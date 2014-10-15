// File: chapter10/routing-example/app/scripts/app.js

// We define our application (and create our module) in the app.js file. We 
// define our 'fifaApp' module, and specify that it depends on the 'ngRoute' 
// module so that we can use routing in our application.
//
// This is where the primary 'router' for the application is defined.
//
// The most complicated route is the 'TeamDetails' route. Here are the 
// details:
//
// • The route itself has the variable code defined in it, which is made 
//   available to controllers using '$routeParams' service. A controller could 
//   get the '$routeParams' service injected, and then access 
//   '$routeParams.code', as our TeamDetailsCtrl does.
//
// • The controller is defined in the route, and uses the 'controllerAs' 
//   syntax to name the instance of the controller teamDetailsCtrl.
//
// • We use a resolve object with one key, 'auth'. This key is arbitrary, but 
//   resolve is used as a way of checking with the server to see if the user is 
//   currently logged in. The auth key takes a function, using the Dependency 
//   Injection syntax, so we can inject any services we need into each 
//   individual resolve key.
//
// • The authentication resolve injects UserService into it, and makes a call to 
//   User Service.session() (which makes a server call to '/api/session'). The 
//   auth resolve function returns a promise. AngularJS then guarantees the 
//   following:
//
//    — AngularJS won’t load the page until the promise is successfully 
//      fulfilled.
//
//    — AngularJS will prevent the page from loading if the promise fails.
//
// • We also chain the promise and do nothing in the case of success. This 
//   ensures that the returned promise succeeds if the server call succeeds.
//
// • We add an error handler in the 'then' of the promise to redirect the user 
//   to the login screen, in case the server returns a non-200 response. We 
//   also make sure that we reject the promise in the case of an error, because 
//   we still want the promise to fail. If we don’t '$q.reject', that tells 
//   AngularJS that the error was handled successfully.
//
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
                  // Prevents the path the user accessed from entering the 
                  // browser’s history.
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



