// Angular Directives: Controller
//
// Directive controllers can be used to communicate between 'directives' and  
// 'shared state'. 
//
// Directive controllers are used in AngularJS for inter-directive communication, 
// By inter-directive communication, we mean when one directive on an element 
// wants to communicate with another directive on its parent or on the same 
// element. This encompasses sharing state or variables, or even functions.
//
// NB: 'link functions' are fully contained and specific to the directive instance. 
//      They provider for a different use-case: 'the interaction of components 
//      'within' directive', not, the interaction 'between directives'.
//
//
// Let’s see how we might create and use directive controllers to create a tabs 
// directive. Now instead of letting the top-level tabs directive recursively 
// create tabs inside of it, we will also create a tab directive for each 
// individual tab in the set of tabs. Now obviously, the tab directive can only 
// be used in the context of the set of tabs. It also needs to be able to let 
// the parent know it was selected so the tab set can decide to hide and show 
// content accordingly. 
//
// Let’s see how we can accomplish this using a directive controller.
//
angular.module('stockMarketApp', [])
  .controller('MainCtrl', [function() {
    var self = this;

    // Will be shown in first tab...
    self.startedTime = new Date().getTime();

    // Will be shown in second tab...
    self.stocks = [
      {name: 'First Stock', price: 100, previous: 220},
      {name: 'Second Stock', price: 140, previous: 120},
      {name: 'Third Stock', price: 110, previous: 110},
      {name: 'Fourth Stock', price: 400, previous: 420}
    ];
  }]);
