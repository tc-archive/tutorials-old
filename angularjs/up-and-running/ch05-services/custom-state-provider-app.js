  // Service Function
  //
  // Service definition function is now a JavaScript class function. It doesn’t 
  // return anything. It takes in a configuration object.
  //
  // The list is private (decalred as a 'var')
  //
  // The service publically expose two interface methods:
  // 1) list (getter).
  // 2) add (mutator)
  //
  //
function ItemService(opt_items) {
  var items = opt_items || [];
  this.list = function() {
    return items;
  };
  this.add = function(item) {
    items.push(item);
  };
}

/**
 * The ItemService gets instantiated once when the application loads and the SubCtrl is 
 * loaded, at which point AngularJS decides it needs an instance of the ItemService.
 *
 * After it is created, all other controllers that ask for the ItemService will get the 
 * exact same instance that was returned the very first time.
 *
 * You should use module.provider() to define your services if:
 *
 * - You follow a Class/OO  style of programming.
 * - You prefer to define classes and types instead of functions and objects.
 * - You eed to set up some configuration for our service before our application 
 *   loads.
 *
 */
angular.module('customStateServiceApp', []).

  // Provider
  //
  // the provider does not use the same notation as factory and service. 
  // It doesn’t take an array as the second argument because providers cannot 
  // have dependencies on other services.
  //
  provider('ItemService', function() {

    var haveDefaultItems = true;

    // We define a function in the provider called disableDefaultItems. 
    // This can be called in the configuration phase of an AngularJS 
    // application. That is, this can be called before the AngularJS app 
    // has loaded and the service has been initialized.
    this.disableDefaultItems = function() {
      haveDefaultItems = false;
    };

    // This function gets our dependencies, not the // provider above
    // The provider also declares a $get function on its instance, which 
    // is what gets called when the service needs to be initialized. At 
    // this point, it can use the state that has been set up in the 
    // configuration to instantiate the service as needed.
    this.$get = [
      function() {
        var optItems = [];
        if (haveDefaultItems) {
          optItems = [
          {id: 1, label: 'Item 0'},
          {id: 2, label: 'Item 1'}
          ];
        }
      return new ItemService(optItems);
      }
    ]; 

  }).

  // Config
  //
  // The config function executes before the AngularJS app executes. So we c
  // an be assured that this executes before our controllers, services, and 
  // other functions.
  //
  // The config function follows the same Dependency Injection pattern.
  //
  // The config function could also set up URL endpoints, locale information, 
  // routing configuration for our application, and so on: things that need to 
  // be executed and initialized before our application starts.
  //
  config(['ItemServiceProvider', function(ItemServiceProvider) {
    
    // To see how the provider can change configuration, change the value of
    // shouldHaveDefaults to true and try running the example
    var shouldHaveDefaults = false;

    // Get configuration from server Set shouldHaveDefaults somehow
    // Assume it magically changes for now...
    if (!shouldHaveDefaults) {
      ItemServiceProvider.disableDefaultItems();
    }

  }]).


  // Main Controller
  //
  // Manage the 'tab' view state.
  //
  controller('MainCtrl', [function() {

    var self = this;

    self.tab = 'first';

    self.open = function(tab) {
      self.tab = tab;
      };

  }]).


  // SubController
  //
  // Manage the ListService view state,
  //
  // Has the ItemService injected into it upon creation.
  //
  // Uses the services exposed interface to add state to and 
  // from the controller.
  //
  controller('SubCtrl', ['ItemService', function(ItemService) {

    var self = this;

    self.list = function() {
      return ItemService.list();
      };

    self.add = function() {
      ItemService.add({
        id: self.list().length + 1,
        label: 'Item ' + self.list().length
      });
    };

}]);
