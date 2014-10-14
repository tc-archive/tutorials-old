// Using Resolves for Pre-Route Checks
//
// A Resolve, defines a set of asynchronous tasks to execute before the route 
// is loaded. 
//
// A resolve is a set of keys and functions. Each function can return a value 
// or a promise. 
//

// A sample resolve, which makes a server call and returns a hardcoded value, 
//
angular.module('resolveApp', ['ngRoute'])
  .value('Constant', {MAGIC_NUMBER: 42})
  .config(['$routeProvider', function($routeProvider) {
    $routeProvider
      .when(
      // Standard Route - No Resolves...
      '/', {
        template: '<h1>Main Page, no resolves</h1>'
        })
      .when(
      // Protected Route - Uses an asynchronous resolve to check the user
      // is authenticated.
      '/protected', {
        template: '<h2>Protected Page</h2>',
        // AngularJS ensures that the route does not load until all the 
        // resolve functions are finished executing.
        //
        // If the resolve function returns a value, AngularJS immediately 
        // finishes executing and treats it as a successful resolve.
        // 
        // If the resolve function returns a promise, AngularJS waits for 
        // the promise to return and treats the resolve as successful if 
        // the promise is successful. If the promise is rejected, the 
        // resolve is treated as a failure.
        //
        // If any of the resolves encounter an error or any of the promises 
        // returned are rejected (is a failure), AngularJS doesn’t load the 
        // route.
        // 
        // The value from each of the resolve keys injected into our 
        // controller, can be obtained if required.
        //
        resolve: {
          immediate: ['Constant', function(Constant) {  // D.I.
            return Constant.MAGIC_NUMBER * 4;
          }],
          async: ['$http', function($http) {            // D.I.
            return $http.get('/api/hasAccess');
          }]
        },
      controller: [
        '$log', 'immediate', 'async',function($log, immediate, async) {
          $log.log('Immediate is ', immediate); 
          $log.log('Server returned for async', async);
          }
        ] 
      });
    }]
  );