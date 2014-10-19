//=============================================================================
// Directive Business Logic Test Example
//=============================================================================

// Tests the 'business logic' of the directive.
//

describe('Stock Widget Directive Behavior', function() {

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

  it('should have functions and data on scope correctly',
      function() {
    // Step 2 - Mock Scope
    //
    // We set up scope against which we will create our directive instance. This 
    // is similar to the controller, which will have our data. We create the stock 
    // instance myStock as well as a title variable on this scope.
    //
    var scope = rootScope.$new();
    var scopeClickCalled = '';
    scope.myStock = {
      name: 'Best Stock',
      price: 100,
      previous: 200
    };
    scope.title = 'the best';
    scope.userClick = function(stockPrice,
                               stockPrevious,
                               stockName) {
      scopeClickCalled = stockPrice +
        ';' + stockPrevious +
        ';' + stockName;
    };

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
    var element = compile(
        '<div stock-widget' +
        ' stock-data="myStock"' +
        ' stock-title="This is {{title}}"' +
        ' when-select="userClick(stockPrice, ' +
            'stockPrevious, stockName)">' +
        '</div>'
    )(scope);

    // Step 5 - Process ('digest') the initialised scope.
    //
    // We digest the scope and flush the server requests. This is done to tell 
    // AngularJS to update all the bindings in the HTML and ensures that the HTML 
    // that we specified in the '$httpBackend' call gets loaded and rendered to 
    // write the rest of our test.
    //
    scope.$digest();
    mockBackend.flush();

    // !!!
    // These first 5 steps are identicial to ''stock-directive-render-spec' 
    // test.


    // Step 6 - Check scope.
    //
    // We ask for the isolated scope of the element we’re working with. This 
    // is different from element.scope(), which would give us the parent scope 
    // if called on an element with a directive. We then check if the directive 
    // has the correct stock data on its own scope. We also check if the 
    // getChange function defined in the directive works as expected.
    //
    var compiledElementScope = element.isolateScope();

    expect(compiledElementScope.stockData)
        .toEqual(scope.myStock);
    expect(compiledElementScope.getChange(
      compiledElementScope.stockData)).toEqual(-50);


    // Step 7 - Check function callback.
    //
    // The last thing we test is the function callback. We have a variable 
    // defined in our test, which is initially set to empty. We use this as a 
    // log of what happened in the test. We then trigger the directive’s onSelect 
    // function (we could have also triggered it through the UI, if our rendered 
    // HTML had a button). This should trigger the scope userClick function, which 
    // sets the string variable. We then check if it is called with the right 
    // values after the function is triggered.
    //
    expect(scopeClickCalled).toEqual('');

    compiledElementScope.onSelect();

    expect(scopeClickCalled).toEqual('100;200;Best Stock');
  });
});
