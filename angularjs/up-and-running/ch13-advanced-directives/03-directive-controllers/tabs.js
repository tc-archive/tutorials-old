// 'tabs' directive - A 'parent' 'container' for the 'tab' directive.
//
angular.module('stockMarketApp')
  .directive('tabs', [function() {
    return {
      // Element only!
      //
      restrict: 'E',
      // The directive uses transclusion (not element-level transclusion, though) 
      // to pick up the individual tabs and add the tab titles above them.
      //
      transclude: true,
      // The tabs directive also defines its owns cope,because it needs to add 
      // certain functions to the scope, and we don’t want to collide or override 
      // any properties or functions on the parent.
      //
      scope: true,
      // The directive template defines a section to repeat over individual tabs 
      // (stored in a tabs array on the scope) and display them. The template also 
      // handles clicking an individual tab as well as highlighting the selected 
      // tab using functions on the scope of the directive.
      //
      // The tabs directive template defines a 'div' in the template where the 
      // contents are translcuded into using 'ng-transclude'. This is where the 
      // entire content of the tabs directive in the HTML (each individual tab) 
      // gets placed during runtime.
      //
      template: 
        '<div class="tab-headers">' +
        '  <div ng-repeat="tab in tabs"' +
        '       ng-click="selectTab($index)"' +
        '       ng-class="{selected: isSelectedTab($index)}">' +
        '     <span ng-bind="tab.title"></span>' +
        '  </div>' +
        '</div>' +
        '<div ng-transclude></div> ',
      // Instead of defining a link function, we define a directive controller. 
      // The reason we do this is because we want children directives of the tabs 
      // directive to be able to access certain functionality from the tabs 
      // directive. Whenever we need to communicate between child and parent 
      // directives, or between sibling directives, we should consider using 
      // directive controllers.
      //
      // A directive controller is a function that gets the scope and element 
      // injected in. This is similar to the link function that we’ve been using 
      // so far, but the difference is that functions we define in the controller 
      // on this can be accessed by child or sibling controllers. Thus, the 
      // controller can define functions that are specific to the directive 
      // instance by defining them on '$scope' as we have been doing so far, and 
      // define the API or accessible functions and variables by defining them on 
      // this or the controller’s instance.
      //
      // Functions are defined on this scope so they can be accessed by child 
      // directives; and so the parent directive can update the rednered view.
      //
      controller: function($scope) {

        var currentIndex = 0;
        
        // Scope state: tabs
        $scope.tabs = [];
        
        // Define function 'registerTab' on 'tabs' parent scope.
        //
        this.registerTab = function(title, scope) {
          if ($scope.tabs.length === 0) {
            scope.selected = true;
          } else {
            scope.selected = false;
          }
          $scope.tabs.push({title: title, scope: scope});
        };

        // Define function 'selectTab' on 'tabs' parent scope.
        //
        $scope.selectTab = function(index) {
          currentIndex = index;
          for (var i = 0; i < $scope.tabs.length; i++) {
            $scope.tabs[i].scope.selected = currentIndex === i;
          }
        };

        // Define function 'isSelectedTab' on 'tabs' parent scope.
        //
        $scope.isSelectedTab = function(index) {
          return currentIndex === index;
        };

      }
    };
  }]);
