// A simple controller for the pie-chart data.
//
angular.module('googleChartApp', [])
  .controller('MainCtrl', [function() {
    var self = this;

    // The pie-chart data.
    //
    self.pieChartData = [
      {label: 'First', value: 25},
      {label: 'Second', value: 54},
      {label: 'Third', value: 75}
    ];

    // The pie-chart config data.
    //
    self.pieChartConfig = {
      title: 'One Two Three Chart',
      firstColumnHeader: 'Counter',
      secondColumnHeader: 'Actual Value'
    };

    // A simple function to chnage the data.
    //
    self.changeData = function() {
      self.pieChartData[1].value = 25;
    };
  }]);
