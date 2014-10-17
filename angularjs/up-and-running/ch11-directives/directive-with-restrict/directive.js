// A - Restricts the directive to use as attributes on existing tags.
//     Example: <div stock-widget></div>). 
//     This is the default value.
//     Best for 'behaviour modifiers' e.g. 'ng-show', 'ng-class', etc.
//
// E - Restricts the directive to use as new HTML elements.
//     Example: <stock-widget></stock-widget>.
//     Best for new HTML content. May not work in old browsers.
//
// C - Restricts the directive to use as class definition on existing tags.
//     Example: <div class="stock-widget"> </div>).
//     Best for 'rendering' related work. e.g. image loading; hide/show, etc.
//
// M - Restricts the directive to use as an HTML 'comment'.
//     Example: <!-- directive: stock-widget ->
//
angular.module('stockMarketApp')
  .directive('stockWidget', [function() {
    return {
      templateUrl: 'stock.html',
      restrict: 'AE'
    };
  }]);
