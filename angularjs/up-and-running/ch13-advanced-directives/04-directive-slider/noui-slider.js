angular.module('sliderApp')
  .directive('noUiSlider', [function() {
    return {

      // Create an element directive, which requires that the 'ngModel' 
      // directive be used on the same element as the 'noUiSlider directive' 
      // that we’re creating.
      // 

      // Restrict to 'element directive' uasge.
      //
      restrict: 'E',
      // Require the 'ng-model' from the directive element 
      // ("mainCtrl.selectedValue").
      //
      require: 'ngModel',


      link: function($scope, $element, $attr, ngModelCtrl) {

        // Create the 'noUiSlider' by calling its constructor with the 
        // appropriate parameters. We use the string attributes from the HTML,  
        // converted to numbers.
        //
        // Because 'noUiSlider' is a jQuery plugin,and jQuery was loaded before
        // loafing AngularJS in index.html, the 'noUiSlider function' can be 
        // called immediately on the element.
        //
        $element.noUiSlider({
          // We might not have the initial value in ngModelCtrl yet
          start: 0,
          range: {
            // $attrs by default gives us string values 'nouiSlider' expects 
            // numbers, so convert.
            //
            min: Number($attr.rangeMin),
            max: Number($attr.rangeMax)
          }
        });

        // To finish integrating 'ngModel' into the third-party input integration
        // there are two steps:
        //
        //  a.  When the data changes within AngularJS, we need to update the 
        //      third-party UI component. We do this by overriding the 
        //      '$render' method on the 'ngModelCtrl', and setting the value 
        //      in the third-party component inside of it. The latest value 
        //      that’s currently set in the variable referred to by 'ngModel' 
        //      is available in the 'ngModelCtrl' in the '$viewValue' variable. 
        //      AngularJS calls the '$render method whenever the model value 
        //      changes inside AngularJS (for example, when it is initialized 
        //      to a value in our controller).
        //
        //  b.  When the data changes outside AngularJS, we need to update 
        //      AngularJS with the new value. We do this by calling the 
        //      '$setViewValue' function on the 'ngModelCtrl' with the latest 
        //      and greatest value inside the set listener.
        //

        // When data changes inside AngularJS notify the third party directive 
        // of the change...
        ngModelCtrl.$render = function() {
          $element.val(ngModelCtrl.$viewValue);
        };

        // When data changes outside of AngularJS
        $element.on('set', function(args) {

          // Also tell AngularJS that it needs to update the UI.
          //
          // Also, as mentioned in the AngularJS life cyle, AngularJS updates 
          // the UI whenever it knows that things within its control have 
          // changed. A third-party UI component is outside the AngularJS life 
          // cycle, so we need to manually call '$scope.$apply()' to ensure 
          // that AngularJS updates the UI. The '$scope.$apply()' call takes 
          // an optional function as an argument and ensures that the 
          // AngularJS digest cycle that’s responsible for updating the UI 
          // with the latest values is triggered.
          //
          $scope.$apply(function() {
            // Set the data within AngularJS.
            //
            ngModelCtrl.$setViewValue($element.val());
          });
        });
      }
    };
  }]);
