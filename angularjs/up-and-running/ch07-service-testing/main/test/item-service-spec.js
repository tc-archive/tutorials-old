//-----------------------------------------------------------------------------
// Angular Controller Tests
//
// NB: It is recommended that you set up all your mocks and spies before 
// instantiating your controllers.
//

//=============================================================================
// Inline Mock Test Example
//=============================================================================
describe('ItemCtrl with inline mock', function() { 

  var ctrl, mockService;

  // SetUp: Reload the 'ItemServiceMod'.
  //
  beforeEach(module('ItemServiceMod'));

  // SetUp: Use the '$provide' service to provide an 'inline mock' of the 
  //        'ItemService'. This replaces the original service.
  //
  beforeEach(module(function($provide) {    
    mockService = {
      list: function() {
        return [{id: 1, label: 'Mock'}]; 
      }
    };
    $provide.value('ItemService', mockService);
  }));

  // SetUp: Inject a new $controller 'ctrl' into the 'NavModule'.
  //
  beforeEach(inject(function($controller) {
    ctrl = $controller('ItemCtrl');
  }));

  // Test
  it('should load mocked out items', function() {
    expect(ctrl.items).toEqual([{id: 1, label: 'Mock'}]);
  }); 


});


//=============================================================================
// Global Mock Test Example
//=============================================================================
describe('ItemCtrl With global mock', function() {

  var ctrl;

  // SetUp: Reload the 'ItemServiceMod'.
  //
  beforeEach(module('ItemServiceMod')); 
  
  // SetUp: Use the 'ItemServiceModMock' global mock to provide an 
  //        'ItemService'. This replaces the original service.
  //
  beforeEach(module('ItemServiceModMock'));

  // SetUp: Inject a new $controller 'ctrl' into the 'NavModule'.
  //
  beforeEach(inject(function($controller) { 
    ctrl = $controller('ItemCtrl');
  }));

  it('should load mocked out items', function() { 
    expect(ctrl.items).toEqual([{id: 1, label: 'Mock'}]);
  }); 

});


//=============================================================================
// Jasmine Spy Test with Actual Underlying Call Example
//=============================================================================
describe('ItemCtrl with spies', function() { 

  var ctrl, itemService;

  // SetUp: Reload the 'ItemServiceMod'.
  //
  beforeEach(module('ItemServiceMod'));

  // SetUp: Create and inject a new 'spy proxy' on the 'ItemService'. This  
  //        replaces the original service.
  //
  // SetUp: Inject a new $controller 'ctrl' into the 'NavModule'.
  //
  beforeEach(inject(function($controller, ItemService) {
    // Call the 'spyOn' Jasmine function with an object as the first argument, 
    // and a string with the function name that we want to hook on to as the 
    // second argument. 
    // 
    // Here Jasmine will spy on the 'list' function of the 'ItemService', and 
    // will 'call through' to the actual underlying service.
    //
    spyOn(ItemService, 'list').andCallThrough(); 

    itemService = ItemService;
    ctrl = $controller('ItemCtrl');
  }));

  it('should load mocked out items', function() { 

    // Expect that the 'itemService.list()' functions has been called once.
    //
    expect(itemService.list).toHaveBeenCalled();
    expect(itemService.list.callCount).toEqual(1); 
    
    expect(ctrl.items).toEqual([
      {id: 1, label: 'Item 0'},
      {id: 2, label: 'Item 1'}
    ]);
  });

});


//=============================================================================
// Jasmine Spy Test with Mocked Underlying Call Example
//=============================================================================
describe('ItemCtrl with SpyReturn', function() {

  var ctrl, itemService; 

  // SetUp: Reload the 'ItemServiceMod'.
  //
  beforeEach(module('ItemServiceMod'));

  // SetUp: Create and inject a new 'spy proxy' on the 'ItemService'. This  
  //        replaces the original service.
  //
  // SetUp: Inject a new $controller 'ctrl' into the 'NavModule'.
  //
  beforeEach(inject(function($controller, ItemService) {
    // Call the 'spyOn' Jasmine function with an object as the first argument, 
    // and a string with the function name that we want to hook on to as the 
    // second argument. 
    // 
    // Here Jasmine will spy on the 'list' function of the 'ItemService', and 
    // will 'mock the returned data' instead of calling the actual underlying 
    // service.
    //
    // NB: It is recommended to mock before creating the controller.
    //
    spyOn(ItemService, 'list').andReturn([{id: 1, label: 'Mock'}]);
    
    itemService = ItemService;
    ctrl = $controller('ItemCtrl');
  }));

  it('should load mocked out items', function() {
    // Expect that the 'itemService.list()' functions has been called once.
    //
    expect(itemService.list).toHaveBeenCalled();
    expect(itemService.list.callCount).toEqual(1);
    expect(ctrl.items).toEqual([{id: 1, label: 'Mock'}]);
  });

});









