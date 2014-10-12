//-----------------------------------------------------------------------------
//
// This module implements a 'global mock' of the 'ItemService'.
//
angular.module('ItemServiceModMock', []) 

  .factory('ItemService', [function() {
    return {
      list: function() {
      return [{id: 1, label: 'Mock'}]; }
    }; 
  }]);