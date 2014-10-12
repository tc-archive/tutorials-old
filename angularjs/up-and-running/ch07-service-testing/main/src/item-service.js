//-----------------------------------------------------------------------------
// A controller module for an 'Item Service'...
//
angular.module('ItemServiceMod', [])
  
  // Define a 'factory' for the module that can create an 'ItemService' 
  // instance.
  //
  .factory('ItemService', [function() {
    
    var items = [
      {id: 1, label: 'Item 0'}, 
      {id: 2, label: 'Item 1'}
    ]; 

    // Return a wrapper object/tuple for the state...
    return {
      list: function() {
        return items;
      }, 
      add: function(item) {
        items.push(item);
      }
    };

  }])

  // Add a 'controller' (called 'ItemServiceCtrl') for the module 
  .controller('ItemServiceCtrl', ['ItemService', function(ItemService) {
    var self = this;  // Capture the 'this' context on creation...
    self.items = ItemService.list();
  }]);