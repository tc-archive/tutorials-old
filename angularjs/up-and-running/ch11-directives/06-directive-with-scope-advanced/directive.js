// We want to pass a string from our controller and get a hook from our directive 
// to trigger a function in our controller any time the user clicks a button 
// inside the directive instance.
//
angular.module('stockMarketApp')
  .directive('stockWidget', [function() {
    return {
      // The 'template html' of the directive...
      templateUrl: 'stock.html',
      // Retrict to use as a 'behaviour modifying' attribute...
      restrict: 'A',
      // See: 'index.html' 'stock-widget' directive.
      scope: {
        stockData: '=',   // Bind to 'stock-data' attribute objects.
        stockTitle: '@',  // Bind to 'stock-title' attribute string/binding_expr.
        whenSelect: '&'   // Bind to 'when-select' attribute function.
      },
      // Encapsulate functions that would otherwise exist on controllers 
      // here...
      link: function($scope, $element, $attrs) {

        // Return the % chnage in stock price.
        $scope.getChange = function(stock) {
          return Math.ceil(
              ((stock.price - stock.previous) /stock.previous) * 100
            );
        };

        $scope.onSelect = function() {
          // Made available from the controller as a function via 'index.html' 
          // 'when-select' attribute.
          //
          // !!! Do note that the key names specified in the HTML have to exactly 
          //     match those provided by our directive.
          //
          $scope.whenSelect({
            stockName: $scope.stockData.name,
            stockPrice: $scope.stockData.price,
            stockPrevious: $scope.stockData.previous
          });
        };
      }

    };
  }]);
