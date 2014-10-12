describe('NavCtrl2', function() { 

  //---------------------------------------------------------------------------
  // SetUp
  //---------------------------------------------------------------------------

  // For each test - Reload the module.
  //
  beforeEach(module('NavModule2'));

  // Inject a new $controller 'ctrl' and a new $location service '$loc' into 
  // the 'NavModule'.  var ctrl, $loc; 
  //
  beforeEach(inject(function($controller, $location) {
    ctrl = $controller('NavCtrl2');
    $loc = $location;
  }));

  //---------------------------------------------------------------------------
  // Tests
  //--------------------------------------------------------------------------- 

  it('should navigate away from the current page', function() {
    expect($loc.path()).toEqual('');
    $loc.path('/here');
    ctrl.navigate1();
    expect($loc.path()).toEqual('/some/where');
  });

  it('should navigate away from the current page', function() {
    expect($loc.path()).toEqual('');
    $loc.path('/there');
    ctrl.navigate2();
    expect($loc.path()).toEqual('/some/where/else');
  }); 

});