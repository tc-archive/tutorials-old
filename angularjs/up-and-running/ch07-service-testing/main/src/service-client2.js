//-----------------------------------------------------------------------------
// A service client controller module that utilises the $http service to fetch 
// 'note data' from the server.
//
// Here the NotesService is refactored out into a seperate factory service.
//
angular.module('ServiceClientMod2', [])

  // The 'ServiceClientCtrl'.
  //
  .controller('ServiceClientCtrl2', ['NoteService', function(NoteService) {
    
    var self = this; 
    
    self.items = []; 
    self.errorMessage = '';
    
    // Invoke the 'NoteService'.
    //
    // NB: The '$http.get' invocation returns an 'http promise' ('$q').
    //     The '$q.then' function calls one of the success or error callbacks 
    //     asynchronously as soon as the result is available.
    //
    NoteService.query().then(
      function(response) {
        self.items = response.data;
      }, 
      function(errResponse) { 
        self.errorMessage = errResponse.data.msg;
      }
    ); 
  }])

  // The refactored 'NotesService'.
  //
  .factory('NoteService', ['$http', function($http) { 
    return {
      query: function() {
        return $http.get('/api/note');
      } 
    };

  }]);