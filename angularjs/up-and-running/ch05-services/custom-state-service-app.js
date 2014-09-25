  // Service Function
  //
  // Service definition function is now a JavaScript class function. It doesn’t 
  // return anything.
  //
  // The list is private (decalred as a 'var')
  //
  // The service publically expose two interface methods:
  // 1) list (getter).
  // 2) add (mutator)
  //
  //
function ItemService() {
  var items = [
    {id: 1, label: 'Item 0'},
    {id: 2, label: 'Item 1'}
    ];
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
 * You should use module.service() to define your services if:
 *
 * - You follow a Class/OO  style of programming.
 * - You prefer to define classes and types instead of functions and objects.
 *
 */
 angular.module('customStateServiceApp', []).

  service('ItemService', [ItemService]).

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




