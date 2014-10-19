angular.module('stockMarketApp', [])
  .directive('stockWidget', [function() {
    return {
      // The rendering logic of the directive is encapsulated in 'stock.html'.
      //
      templateUrl: 'stock.html',
      // The directive is restricted to attributes.
      //
      restrict: 'A',
      // The directive has isolated scope with the stockData and the stockTitle 
      // passed in along with whenSelect, the function to be called when the 
      // user selects a stock.
      // 
      scope: {
        stockData: '=',
        stockTitle: '@',
        whenSelect: '&'
      },
      // The link function defines a function to calculate the percentage change 
      // as well as a function for the UI to call when a button is clicked.
      //
      link: function($scope, $element, $attrs) {

        $scope.getChange = function(stock) {
          return Math.ceil(
            ((stock.price - stock.previous) / stock.previous) * 100
            );
        };

        $scope.onSelect = function() {
          $scope.whenSelect({
            stockName: $scope.stockData.name,
            stockPrice: $scope.stockData.price,
            stockPrevious: $scope.stockData.previous
          });
        };
        
      }
    };
  }]);
