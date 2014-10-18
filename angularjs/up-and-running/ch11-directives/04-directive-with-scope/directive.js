angular.module('stockMarketApp')
  .directive('stockWidget', [function() {
    return {
      templateUrl: 'stock.html',
      restrict: 'A',
      // AngularJS gives us the scope key in the directive definition object to 
      // have complete control over the scope of the directive element. The 
      // scope key can take one of three values:
      //
      // - 'false'
      //   This is the default value, which basically tells AngularJS that the 
      //   directive scope is the same as the 'parent scope', whichever one it is. 
      //   So the directive gets access to all the variables and functions that 
      //   are defined on the parent scope, and any modifications it makes are 
      //   immediately reflected in the parent as well.
      //
      // - 'true'
      //   This tells AngularJS that the directive scope inherits the parent 
      //   scope, but creates a 'child scope' of its own. The directive thus gets 
      //   access to all the variables and functions from the parent scope, but 
      //   any modifications it makes are not available in the parent. This is 
      //   recommended if we need access to the parent’s functions and information, 
      //   but need to make local modifications that are specific to the directive.
      //
      //  - 'object'
      //   We can also pass an object with keys and values to the scope. This 
      //   tells AngularJS to create what we call an 'isolated scope'. This scope 
      //   does not inherit anything from the parent, and any data that the 
      //   parent scope needs to share with this directive needs to be passed 
      //   in through HTML attributes. This is the best option when creating 
      //   reusable components that should be independent of how and where they 
      //   are used.
      //
      //   In the object, we can identify what attributes are to be specified in 
      //   the HTML when the directive is used, and the types of values that will 
      //   be passed in to the directive. In particular, we can specify three 
      //   types of values that can be passed in, which AngularJS will directly put 
      //   on the scope of the directive:
      //
      //   - '=''
      //    The = sign specifies that the value of the attribute in HTML is to be 
      //    treated as a JSON object, which will be bound to the scope of the 
      //    directive so that any changes done in the parent scope will be 
      //    automatically available in the directive.
      //
      //   - '@'
      //    The @ sign specifies that the value of the attribute in HTML is to 
      //    be treated as a string, which may or may not have AngularJS binding 
      //    expressions ({{ }}). The value is to be calculated and the final 
      //    value is to be assigned to the directive’s scope. Any changes in the 
      //    value will also be available in the directive.
      //
      //   - '&'
      //    The & sign specifies that the value of the attribute in HTML is a 
      //    function in some controller whose reference needs to be available to 
      //    the directive. The directive can then trigger the function whenever 
      //    it needs to.
      //
      //    To make our directive fully contained and reusable, we can now pass 
      //    the stock object to our widget. This way, if the variable is renamed 
      //    outside, the new variable can be passed to our directive, making it 
      //    independent of the name. This can be done using the = binding with the 
      //    scope object, and we can reassign the value to a consistent name on the 
      //    directive’s isolated scope. Let’s first see how we might want to change 
      //    the usage of the directive in the html file.
      //
      scope: {
        stockData: '='  // So the html would use the key 'stock-data'...
      },
      link: function($scope, $element, $attrs) {
        $scope.getChange = function(stock) {
          return Math.ceil(((stock.price - stock.previous) /
              stock.previous) * 100);
        };
      }
    };
  }]);
