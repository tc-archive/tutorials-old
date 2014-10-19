//=============================================================================
// Directive Render Test Example
//=============================================================================

// Tests the 'rendering' of the directive.
//

describe('Stock Widget Directive Rendering', function() {

  var compile, mockBackend, rootScope;

  //---------------------------------------------------------------------------
  // SetUp
  //---------------------------------------------------------------------------

  // SetUp: Reload the 'stockMarketApp'.
  //
  beforeEach(module('stockMarketApp'));


  // SetUp:  Inject '$compile', '$httpBackend', '$rootScope' test dependencies.
  //
  beforeEach(inject(function($compile, $httpBackend, $rootScope) {
    // Step 1
    //
    // The very first thing we do is inject all the necessary services to create 
    // and test our directive in beforeEach. This includes '$compile', which is 
    // necessary to create instances of our directive; '$rootScope' to be able  
    // to create scopes to test our directives against; and the '$httpBackend',  
    // to simulate and handle the server call to load the template.
    // 
    compile = $compile;
    mockBackend = $httpBackend;
    rootScope = $rootScope;
  }));

  //---------------------------------------------------------------------------
  // Tests
  //--------------------------------------------------------------------------- 

  it('should render HTML based on scope correctly', function() {
    // Step 2 - Mock Scope
    //
    // We set up scope against which we will create our directive instance. This 
    // is similar to the controller, which will have our data. We create the stock 
    // instance myStock as well as a title variable on this scope.
    //
    var scope = rootScope.$new();
    scope.myStock = {
      name: 'Best Stock',
      price: 100,
      previous: 200
    };
    scope.title = 'the best';

    // Step 3 - Mock Backend retrieved HTML template. 
    //
    // Because our directive loads the template by URL, and because there is no 
    // server in a unit test, we have to set expectations in $httpBackend on what 
    // server template will be loaded and what its content will be. Because it is a 
    // unit test, we just give it some dummy HTML that can be used to test element 
    // rendering and data accuracy.
    //
    mockBackend.expectGET('stock.html').respond(
      '<div ng-bind="stockTitle"></div>' +
      '<div ng-bind="stockData.price"></div>');

    // Step 4 - Mock Directive
    //
    // We create an instance of the directive. We first compile the HTML that 
    // triggered our directive. This returns a compiled function, which we then 
    // call with scope to compile it against.
    //
    var element = compile('<div stock-widget' +
      ' stock-data="myStock"' +
      ' stock-title="This is {{title}}"></div>'
      )(scope);

    // Step 5 - PRocess ('digest') the initialised scope.
    //
    // We digest the scope and flush the server requests. This is done to tell 
    // AngularJS to update all the bindings in the HTML and ensures that the HTML 
    // that we specified in the '$httpBackend' call gets loaded and rendered to 
    // write the rest of our test.
    //
    scope.$digest();
    mockBackend.flush();


    // !!!
    // These first 5 steps are identicial to ''stoct-directive-behavior-spec' 
    // test.


    // Step 6
    //
    // At this point, we have a fully instantiated version of our directive. 
    // Here, we write the expectations and tests for rendering to see if the 
    // data was picked up from the scope correctly and that the HTML attributes 
    // were correctly passing along the data.
    //
    expect(element.html()).toEqual(
      '<div ng-bind="stockTitle" class="ng-binding">' +
        'This is the best' +
      '</div>' +
      '<div ng-bind="stockData.price" class="ng-binding">' +
        '100' +
      '</div>');
  });


});
