//-----------------------------------------------------------------------------
describe('NavCtrl', function() { 

  // Initialise a new module...
  beforeEach(module('NavModule'));
  
  // Inject a new $controller 'ctrl' and a new $location service '$loc' into 
  // the 'NavModule'.
  var ctrl, $loc; 
  beforeEach(inject(function($controller, $location) {
    ctrl = $controller('NavCtrl');
    $loc = $location;
  }));

  // Test the navigation
  it('should navigate away from the current page', function() {
    $loc.path('/here');
    ctrl.navigate(); 
    expect($loc.path()).toEqual('/some/where/else');
  });

});