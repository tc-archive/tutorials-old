angular.module('stockMarketApp')
  .directive('validZip', [function() {

    // Define the RegEx for valid 'zips codes'.
    var zipCodeRegex = /^\d{5}(?:[-\s]\d{4})?$/g;

    return {
      // An 'attribute' directive.
      //
      restrict: 'A',
      // Uses the 'ng-model' of the input ('mainCtrl.zip').
      //
      require: 'ngModel',
      // Link Function
      //
      link: function($scope, $element, $attrs, ngModelCtrl) {

        // Handle DOM update --> Model update
        //
        // Models '$parsers' Functions
        //
        // When the data changes in the DOM, AngularJS goes through each 
        // 'parser', and the 'parser' gets to check the validity of the data 
        // before passing it along in the chain. We add our own 'parser' 
        // to the chain of 'parsers', and perform our validity check here 
        // using the regular expression. 
        //
        // In either case, we set the validity of our directive on the 'ngModel 
        // controller'. The 'parser function' has to return the correct value 
        // (if the data is valid) or undefined (in case the data isn’t).
        //
        ngModelCtrl.$parsers.unshift(function(value) {
          // Check 'validity'...
          var valid = zipCodeRegex.test(value);
          // Set 'validity' on the controller...
          ngModelCtrl.$setValidity('validZip', valid);
          // Return 'validity'...
          return valid ? value : undefined;
        });

        // Handle Model Update --> DOM
        //
        // Models '$formatters' Functions
        //
        // We also need to handle the case where the model is updated (due to 
        // a server response, say). In this case, AngularJS runs the data 
        // through a formatting step to ensure that it’s taking the correct API. 
        // We again check for validity here and return the value.
        //
        ngModelCtrl.$formatters.unshift(function(value) {
          ngModelCtrl.$setValidity(
            'validZip',
            zipCodeRegex.test(value)
            );
          return value;
        });


        // NB:  In both of these cases, we set the validity of the element using 
        //      the name of the directive on the ngModel controller.
        //
        //      Now, we could very well do asynchronous validation in either of 
        //      these cases by making a server call through $http. So email or 
        //      username availability checks could be wrapped in their own 
        //      validators. Your imagination is the only limit with these 
        //      validators!

      }
    };

  }]);
