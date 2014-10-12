//-----------------------------------------------------------------------------
describe('NavCtrl', function() { 

  var ctrl, $loc; 
  
  //---------------------------------------------------------------------------
  // SetUp
  //---------------------------------------------------------------------------

  // Initialise a new module...
  beforeEach(module('NavModule'));
  
  // Inject a new $controller 'ctrl' and a new $location service '$loc' into 
  // the 'NavModule'.
  beforeEach(inject(function($controller, $location) {
    ctrl = $controller('NavCtrl');
    $loc = $location;
  }));

  //---------------------------------------------------------------------------
  // Tests
  //--------------------------------------------------------------------------- 

  it('should navigate away from the current page', function() {
    $loc.path('/here');
    ctrl.navigate(); 
    expect($loc.path()).toEqual('/some/where/else');
  });

});