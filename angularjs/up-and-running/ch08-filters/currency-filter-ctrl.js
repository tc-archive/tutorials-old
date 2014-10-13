angular.module('myModule', [])
  .controller('MyCtrl', ['currencyFilter',
    function(currencyFilter) {
      // Attaching the word “Filter” after any AngularJS filter allows 
      // us to inject it into our controllers or services.
      //
      // For Example: The 'number' filter becomes 'numberFilter' and 
      // 'filter' (becomes the convoluted) 'filterFilter'...
      //
      // With the HTML, pipe syntax is used to pass the filter its input 
      // When we get a handle on it in our controller or service, the 
      // underlying 'function' is injected.
      //
    }]);