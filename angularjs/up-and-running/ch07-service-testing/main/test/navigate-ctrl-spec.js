describe('SimpleCtrl', function() { 

  // Initialise a new module...
  beforeEach(module('NavModule'));
  
  // Initialise a new 'ctrl controller' and '$location service'  
  var ctrl, $loc; 
  beforeEach(inject(function($controller, $location) {
    ctrl = $controller('SimpleCtrl');
    $loc = $location;
  }));

  // Test the navigation
  it('should navigate away from the current page', function() {
    $loc.path('/here');
    ctrl.navigate(); 
    expect($loc.path()).toEqual('/some/where/else');
  });

});