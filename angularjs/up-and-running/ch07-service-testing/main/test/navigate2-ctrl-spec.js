describe('SimpleCtrl2', function() { 

  // For each test - Reload the module.
  //
  beforeEach(module('Nav2Module'));

  // Inject a new $controller 'ctrl' and a new $location service '$loc' into 
  // the 'NavModule'.  var ctrl, $loc; 
  //
  beforeEach(inject(function($controller, $location) {
    ctrl = $controller('SimpleCtrl2');
    $loc = $location;
  }));

  // Test 1
  //
  it('should navigate away from the current page', function() {
    expect($loc.path()).toEqual('');
    $loc.path('/here');
    ctrl.navigate1();
    expect($loc.path()).toEqual('/some/where');
  });

  // Test 2
  //
  it('should navigate away from the current page', function() {
    expect($loc.path()).toEqual('');
    $loc.path('/there');
    ctrl.navigate2();
    expect($loc.path()).toEqual('/some/where/else');
  }); 

});