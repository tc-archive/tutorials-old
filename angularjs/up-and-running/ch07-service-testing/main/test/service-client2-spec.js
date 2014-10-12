
//=============================================================================
// Angular '$httpBackend' Seperate Service Integration Test Example
//=============================================================================
describe('ServiceClientMod2 Integration Test', function() {

  var ctrl, mockBackend; 

  // SetUp: Reload the 'ServiceClientMod2'.
  //
  beforeEach(module('ServiceClientMod2'));

  // SetUp: Create and inject a new '$httpBackend' to mock the 'ServiceClientCtrl'.   
  //        This replaces the original service.
  //
  // SetUp: Inject a new $controller 'ctrl' into the 'ServiceClientMod'.
  //
  beforeEach(inject(function($controller, $httpBackend) {
    mockBackend = $httpBackend;
    mockBackend.expectGET('/api/note').respond(404, {msg: 'Not Found'});
    ctrl = $controller('ServiceClientCtrl2');
    // At this point, a server request will have been made
  }));


  // Tests
  //
  it('should handle error while loading items', function() { 

    // Initially, before the server responds, the items should be empty.
    expect(ctrl.items).toEqual([]);
    
    mockBackend.flush();

    // No items from server, only an error, so, items should still be empty.
    expect(ctrl.items).toEqual([]);

    // and check the error message...
    expect(ctrl.errorMessage).toEqual('Not Found');
  
    });

  // TearDown: Verify there are no exceptions...
  //
  // NB: Good practice recomendation.
  //
  afterEach(function() {

    // Ensure that all expects set on the $httpBackend were actually called 
    mockBackend.verifyNoOutstandingExpectation();

    // Ensure that all requests to the server have actually responded 
    // (using flush()) 
    mockBackend.verifyNoOutstandingRequest();
  }); 

});