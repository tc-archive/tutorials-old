//-----------------------------------------------------------------------------
describe('Test Controller: ListCtrl', function() {

  // Before each unit test... 
  // ... instantiate a new version of the specified 'module'.
  //
  // This has the effect of freshly loading all the controllers, services, 
  // directives, and filters associated with that module before each specific 
  // unit test.
  //
  // This module function is one of the helper methods that the 
  // angular-mocks.js AngularJS library file provides, as well as many others.
  //
  beforeEach(module('CntrlModule'));

  // Before each unit test... 
  // ... instantiate a new instance of the 'controller'.
  //
  // The function passed to inject can take multiple arguments, each of which 
  // is an AngularJS service that AngularJS then creates and injects into 
  // the function.
  //
  // There is an AngularJS service called $controller that we can use to 
  // instantiate new instances of our controller.
  //
  // Use the$controllerservice to create an instance of the 'ListCtrl' by 
  // passing the name of the controller as a string to the $controller service, 
  //
  var ctrl;
  beforeEach(inject(function($controller) {
    ctrl = $controller('ListCtrl');
  }));

  // Test the 'constructor' function sets up the correct initial state.
  //
  it('should have items available on load', function() {

    expect(ctrl.items).toEqual([
      {id: 1, label: 'First', done: true},
      {id: 2, label: 'Second', done: false} 
    ]);
  });

  // Test the 'getDoneClass' function sets the class correctly
  //
  it('should have highlight items based on state', function() {

    var item = {id: 1, label: 'First', done: true};
    var actualClass = ctrl.getDoneClass(item);     // Execute function

    expect(actualClass.finished).toBeTruthy(); 
    expect(actualClass.unfinished).toBeFalsy();

    item.done = false;
    actualClass = ctrl.getDoneClass(item);         // Execute function

    expect(actualClass.finished).toBeFalsy(); 
    expect(actualClass.unfinished).toBeTruthy();

  }); 

});
