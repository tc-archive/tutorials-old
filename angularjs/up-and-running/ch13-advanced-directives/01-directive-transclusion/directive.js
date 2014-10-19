// Transclusion
//
// The default behavior of any AngularJS directive, in that if it has a 
// template or templateUrl, it will remove the content of the element where 
// the directive is found and replace it with the template specified in the 
// directive definition object.
//
// However, in cases, where we want AngularJS to respect both the inner 
// contents where the directive is used, as well as the original template 
// of the directive, we use the concept of 'transclusion'.
//
angular.module('stockMarketApp')
  .directive('stockWidget', [function() {
    return {
      templateUrl: 'stock.html',
      restrict: 'A',
      // Basic Transclusion
      //
      // Basic transclusion can be thought of as a two-step process:
      //
      // 1. First, we tell the directive that we are going to use transclusion as 
      //    part of this directive. This tells AngularJS that whenever the 
      //    directive is  encountered in the HTML, to make a copy of its content 
      //    and store it so that it’s not lost when AngularJS replaces it with the 
      //    directive’s template. This is accomplished by setting the key 
      //    transclude to true as part of the directive definition object.
      //
      // 2. Second, we need to tell AngularJS where to put the content that was 
      //    stored in the template. This is accomplished by using the 
      //    'ng-transclude' directive, which ensures that the content that was 
      //    captured is made a child of the element in the directive template.
      //
      transclude: true,
      scope: {
        stockData: '='
      },
      link: function($scope, $element, $attrs) {
        $scope.getChange = function(stock) {
          return Math.ceil(((stock.price - stock.previous) /
              stock.previous) * 100);
        };
      }
    };
  }]);
