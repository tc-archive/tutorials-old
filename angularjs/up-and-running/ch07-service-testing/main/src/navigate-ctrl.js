//-----------------------------------------------------------------------------
// A (singlemethod) controller module for altering the browser url navigation 
// path...
//
angular.module('NavModule', [])
  
  .controller('NavCtrl', ['$location', function($location) {

    var self = this; 

    self.navigate = function() {
      $location.path('/some/where/else');
    };
    
  }

]);