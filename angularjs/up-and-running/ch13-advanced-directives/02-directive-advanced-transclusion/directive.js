// Create a 'simple-stock-repeat' trivial replacement for the 'ng-repeat' that 
// will pick up some variables from our outer scope, and add some variables for 
// each instance. 
//
// NB: We won’t make it auto-update, but will instead focus on how to use 
// transclusion to render multiple instances of our template.
//
angular.module('stockMarketApp').directive('simpleStockRepeat',
    [function() {
  return {
    restrict: 'A',
    // Capture and replace the entire element instead of just its content.
    //
    transclude: 'element',
    // A $transclude is passed in as the fifth argument to the link function.
    //
    // This transclude function is a constructor that allows us to create new 
    // instances of our template as many times as needed depending on our use 
    // case. The function takes an optional scope (if a new scope is needed for 
    // the element; otherwise, it inherits the directive’s scope) and a mandatory
    // clone linking function as the second argument.
    //
    link: function($scope, $element, $attrs, ctrl, $transclude) {

      // Evaluate the variable mentioned in the HTML along with the directive to 
      // get a handle on the array that we want to repeat on. This is 
      // accomplished by calling '$eval' on the scope with a string that contains 
      // the JavaScript we want to evaluate in the context of the scope.
      //
      var myArray = $scope.$eval($attrs.simpleStockRepeat);

      // 'transclude element' copies the entire element,it also removes the 
      // element from the HTML. So we create a container element within which to 
      // put all our instances that we create.
      //
      var container = angular.element(
        '<div class="container"></div>'
        );

      // Loop for each instance in our array, and call the transclude function 
      // that is passed to the linking function. This returns a new HTML element 
      // that is a fully compiled and linked version of our template that can then 
      // be inserted into our main body.
      //
      for (var i = 0; i < myArray.length; i++) {
        // Create an element instance with a new child scope using the clone 
        // linking function.
        //
        var instance = $transclude(
          // The first argument to the transclude function is an optional scope. 
          // In this example, we create a new child scope. This is so that any 
          // modification made to the scope does not get reflected in the parent 
          // scope. This is always a good practice to make sure no global states 
          // step on each other.
          //
          $scope.$new(),
          // The second argument passed to the transclude function is a linking 
          // function for the cloned element. This is where we add any behavior 
          // or variable that is specific to this instance of the template. In 
          // this case 'currentIndex' and 'stock'.
          //
          function(
            clonedElement, newScope
            ) {
            // Expose custom variables for the instance.
            //
            newScope.currentIndex = i;
            newScope.stock = myArray[i];
          }
        );

        // Add the new instacne to the 'container'.
        //
        container.append(instance);
      }

      // With transclude: 'element', the element gets replaced with a comment. 
      // Add our generated content after the comment.
      //
      $element.after(container);
    }
  };
  }]);
