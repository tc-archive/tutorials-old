//-----------------------------------------------------------------------------
// A service client controller module that utilises the $http service to fetch 
// 'note data' from the server.
//
angular.module('ServiceClientMod', [])

  // The 'ServiceClientCtrl'.
  //
  .controller('ServiceClientCtrl', ['$http', function($http) {
    var self = this; 

    self.items = []; 
    self.errorMessage = '';

    // The 'embedded 'NotesService'.
    //
    $http.get('/api/note').then(
      function(response) {
        // Success Handler - Save the returned data to the ctrl state.
        self.items = response.data;
      }, 
      function(errResponse){
        // Error Handler
        self.errorMessage = errResponse.data.msg;
      }
    );

  }]);