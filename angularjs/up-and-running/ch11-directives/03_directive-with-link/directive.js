// The link Function
//
// The link keyword in the directive definition object is used to add what we 
// call a “link function” for the directive. The link function does for a 
// directive what a controller does for a view—it defines APIs and functions 
// that are necessary for the directive, in addition to manipulating and working 
// with the DOM.
//
angular.module('stockMarketApp')
  .directive('stockWidget', [function() {
    return {
      templateUrl: 'stock.html',
      restrict: 'AE',
      // AngularJS executes the link function for each instance of the directive, 
      // so each instance can get its own, fully contained business logic while 
      // not affecting any other instance of the directive. The link function gets 
      // a standard set of arguments passed to it that remain consistent across 
      // directives, which looks something like the following:
      //
      //    link: function($scope, $element, $attrs) {}
      //
      // The link function gets passed the scope of the element the directive is 
      // working on, the HTML DOM element the directive is operating on, and all 
      // the attributes on the element as strings. If we need to add functionality 
      // to our instance of the directive, we can add it to the scope of the element 
      // we’re working with.
      //
      // The link function is also where we can define our own listeners, work 
      // directly with the DOM element, and much more. 
      //
      link: function($scope, $element, $attrs) {
        // What’s the Scope?
        //
        // You might wonder what scope we are adding these functions to. You’re 
        // right to be worried, and it is something we should always keep in mind 
        // when we add functions to the scope in the link function. In the example 
        // in this section, ng-repeat in the main index.html file creates a scope 
        // for each stock in our array, and it is to this scope that we’re adding 
        // the functions. Because of this, we’re not affecting the controller’s 
        // scope directly, but that is an unintentional side effect of ng-repeat. 
        // If we had used our directive outside ng-repeat, we would have ended up 
        // modifying the controller’s scope directly, which is bad practice.
        // 
        // The default scope given in the link function (unless specified otherwise) 
        // is the scope that the parent has. Adding functions to the parent scope 
        // should always be frowned upon, because the parent should ideally not be 
        // changed from within a child.
        //
        $scope.getChange = function(stock) {
          return Math.ceil(
            ((stock.price - stock.previous) / stock.previous) * 100
          );
        };
      }
    };
  }]);
