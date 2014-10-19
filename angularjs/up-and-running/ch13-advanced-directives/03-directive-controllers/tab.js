// 'tab' directive - A 'child' tab' directive for 'parent' 'tags' 'container'.
//
angular.module('stockMarketApp')
  .directive('tab', [function() {
    return {
      restrict: 'E',
      // Set up transclusion, because it defines a template of its own.
      //
      transclude: true,
      // The template uses 'ng-transclude' to add the content inside a div, 
      // and add a condition to selectively hide and show the div based on a 
      // selected variable on the scope.
      //
      template: '<div ng-show="selected" ng-transclude></div>',
      // We add a new key, require, and use the value '^tabs' (^: See “require 
      // options”). This tells AngularJS that for the 'tab directive' to work, 
      // it requires that one of the parent elements in the HTML be the 
      // 'tabs directive', and we want its controller to be made available to 
      // the tab directive.
      //
      require: '^tabs',
      // Define a new scope for this directive so that local variables don’t 
      // override anything in the parent scope.
      //
      scope: true,
      // In the link function, we get the controller we required as the fourth 
      // argument(after scope, element, and attributes). This is an instance of 
      // the controller we defined in the 'tabs directive', and is dynamically 
      // injected based on what AngularJS finds.
      //
      link: function($scope, $element, $attr, tabCtrl) {
        // Register the tab with the parent 'tabs directive' function that we 
        // defined earlier.
        //
        tabCtrl.registerTab($attr.title, $scope);
      }
    };
  }]);
