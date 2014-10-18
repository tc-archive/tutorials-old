// We want to pass a string from our controller and get a hook from our directive 
// to trigger a function in our controller any time the user clicks a button 
// inside the directive instance.
//
angular.module('stockMarketApp', [])
  .controller('MainCtrl', [function() {
    var self = this;
    self.stocks = [
      {name: 'First Stock', price: 100, previous: 220},
      {name: 'Second Stock', price: 140, previous: 120},
      {name: 'Third Stock', price: 110, previous: 110},
      {name: 'Fourth Stock', price: 400, previous: 420}
    ];


    // The getChange function was removed from this controller when we moved 
    // it into the 'link' function of the directive.
    //
    /*
    self.getChange = function(stock) {
      return Math.ceil(
        ((stock.price - stock.previous) / stock.previous) * 100
      );
    */


    // Add a function to be triggered whenever a stock is selected. This function 
    // is called with the price and name of the stock that was selected, and just 
    // logs it to the console. 
    //
    self.onStockSelect = function(price, name) {
      console.log('Selected Price ', price, 'Name ', name);
    };
  }]);
