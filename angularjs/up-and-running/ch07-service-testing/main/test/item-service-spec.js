//-----------------------------------------------------------------------------
describe('ItemCtrl with inline mock', function() { 

  // SetUp: Reload the 'ItemServiceMod'.
  beforeEach(module('ItemServiceMod'));

  // SetUp: Use the '$provide' service to provide an 'inline mock' of the 
  //        'ItemService'. This replaces the original service.
  var ctrl, mockService;
  beforeEach(module(function($provide) {    
    mockService = {
      list: function() {
        return [{id: 1, label: 'Mock'}]; 
      }
    };
    $provide.value('ItemService', mockService);
  }));

  // SetUp: Inject a new $controller 'ctrl' into the 'NavModule'.
  beforeEach(inject(function($controller) {
    ctrl = $controller('ItemCtrl');
  }));

  // Test
  it('should load mocked out items', function() {
    expect(ctrl.items).toEqual([{id: 1, label: 'Mock'}]);
  }); 


});


describe('ItemCtrl With global mock', function() {

  var ctrl; 
  beforeEach(module('ItemServiceMod')); 
  
  beforeEach(module('ItemServiceModMock'));
  
  beforeEach(inject(function($controller) { 
    ctrl = $controller('ItemCtrl');
  }));

  it('should load mocked out items', function() { 
    expect(ctrl.items).toEqual([{id: 1, label: 'Mock'}]);
  }); 

});