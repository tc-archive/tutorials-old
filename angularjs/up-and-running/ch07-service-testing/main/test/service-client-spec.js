//-----------------------------------------------------------------------------

//=============================================================================
// Angular '$httpBackend' Mock Test Example
//=============================================================================
describe('ServiceClientCtrl Server Call Mock Test', function() {

  var ctrl, mockBackend;

  // SetUp: Reload the 'ServiceClientMod'.
  //
  beforeEach(module('ServiceClientMod'));
 
  // SetUp: Create and inject a new '$httpBackend' to mock the 
  //        'ServiceClientCtrl'. This replaces the original service.
  //
  // SetUp: Inject a new $controller 'ctrl' into the 'ServiceClientMod'.
  //
  beforeEach(inject(function($controller, $httpBackend) {
    mockBackend = $httpBackend;
    // Also PUT, POST, etc. arg1: URL; arg2: Data
    mockBackend.expectGET('/api/note').respond([{id: 1, label: 'Mock'}]);
    ctrl = $controller('ServiceClientCtrl');
    // At this point, a server request will have been made...
  }));

  // Tests
  //
  it('should load items from server', function() {
    // Initially, before the server responds, the items should be empty.
    expect(ctrl.items).toEqual([]);
    // Simulate a server response...
    mockBackend.flush();
    // Now retrieve and test the set-up response...
    expect(ctrl.items).toEqual([{id: 1, label: 'Mock'}]);
  });

  // TearDown: Verify there are no exceptions...
  //
  // NB: Good practice recomendation.
  //
  afterEach(function() {
    // Ensure that all 'expects' set on the $httpBackend were actually called. 
    mockBackend.verifyNoOutstandingExpectation();
    // Ensure that all requests to the server have actually responded (using 
    // flush()) .
    mockBackend.verifyNoOutstandingRequest();
  });

});