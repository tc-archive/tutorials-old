// Whenever we pass data using object binding to directives, it is done by 
// reference. AngularJS uses this fact to ensure that any changes done to the 
// variable in the controller are reflected inside the directive. But this also 
// means that if the reference to the variable gets reassigned in the directive, 
// then the data-binding breaks in AngularJS.
//
angular.module('stockMarketApp')
  .directive('stockWidget', [function() {
    return {
      templateUrl: 'stock.html',
      restrict: 'A',
      scope: {
        stockData: '='
      },
      link: function($scope, $element, $attrs) {
        $scope.getChange = function(stock) {
          return Math.ceil(((stock.price - stock.previous) /
              stock.previous) * 100);
        };

        $scope.changeStock = function() {
          $scope.stockData = {
            name: 'Directive Stock',
            price: 500,
            previous: 200
          };
        };
      }
    };
  }]);
