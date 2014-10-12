//-----------------------------------------------------------------------------
// A (two method) controller module for altering the browser url navigation 
// path...
//
angular.module('NavModule2', [])

  .controller('NavCtrl2', ['$location', '$window',
    
    function($location, $window) { 
      
      var self = this;
      
      self.navigate1 = function() { 
        $location.path('/some/where');
      };

      self.navigate2 = function() {
        $location.path('/some/where/else');
      };
    }

  ]);
