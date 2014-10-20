// Asynchronous loader, which ensures that the pie Chart directive doesn’t try 
// drawing the chart before our API loaded. 
//
// The 'googleChartLoaderPromise' factory loads the visualization library once 
// at load, and returns a promise that can be chained on to know when the load 
// is complete.
//
angular.module('googleChartApp')
  .factory('googleChartLoaderPromise',
      ['$q', '$rootScope', '$window',
      function($q, $rootScope, $window) {

    // Create a deferred object, which represents an asynchronous task that 
    // will be fulfilled in the future. 
    //
    var deferred = $q.defer();

    // Load Google Charts API asynchronously
    //
    $window.google.load('visualization', '1',
      {
        packages: ['corechart'],
        callback: function() {
          // Inside the callback, we resolve the deferred object we created, 
          // which is the trigger for all the .then to execute. But because 
          // this callback is called outside the life cycle of AngularJS, 
          // we need to wrap it in a '$rootScope.$apply' function to ensure 
          // AngularJS knows to redraw the UI and run a complete digest 
          // cycle as needed.
          //
          $rootScope.$apply(function() {
            deferred.resolve();
          });
        }
      });

    // Return 'deferred.promise', on which users of the API can add .then() to 
    // be notified when this asynchronous task will be fulfilled (or 
    // rejected).
    //
    return deferred.promise;
  }]);
