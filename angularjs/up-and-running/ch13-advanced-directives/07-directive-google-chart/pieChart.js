angular.module('googleChartApp')
  // Inject the 'googleChartLoaderPromise'.
  //
  .directive('pieChart', ['googleChartLoaderPromise',
      function(googleChartLoaderPromise) {

    // Define a function 'convertToPieChartDataTableFormat', which takes the 
    // data that we have in our controller and converts it into a format 
    // that we can pass to the Google Charts API.
    //
    var convertToPieChartDataTableFormat =
        function(firstColumnName, secondColumnName, data) {

      var pieChartArray = [[firstColumnName, secondColumnName]];
      for (var i = 0; i < data.length; i++) {
        pieChartArray.push([data[i].label, data[i].value]);
      }

      return google.visualization.arrayToDataTable(
          pieChartArray
          );
      };

    // Define a directive with an isolated scope that defines the attributes 
    // that need to be passed to it.
    //
    return {
      restrict: 'A',
      scope: {
        chartData: '=',
        chartConfig: '='
      },
      // In the link function, we use the promise returned from the service, and 
      // do our work in the success handler inside the then of the promise. This 
      // ensures that we don’t try calling a Google Charts API unless and until 
      // the Google Charts API has successfully finished loading as per our 
      // service.
      //
      link: function($scope, $element) {

        googleChartLoaderPromise.then(
          // Inside the success handler of the promise, we create an instance 
          // of a Google Pie Chart, using the element that we are currently on 
          // as the target. This ensures that we don’t go looking for a random 
          // element in our body, or use ID-based selectors, each of which would 
          // make our directive hard to reuse.
          //
          function() {
            var chart = new google.visualization.PieChart($element[0]);

            // Add a watch on the 'chartData' field on the scope, and give it a 
            // function to call as the second argument, and the Boolean true as 
            // the third argument. This tells AngularJS to do what we call a 
            // deep watch on $scope.chartData, and whenever it (or any element 
            // inside of it) changes, call the function.
            //
            $scope.$watch('chartData', 
              // The change function is called with both the old and the new 
              // value. When we geta valid new value, we draw the chart after 
              // converting the data from the format passed to the directive 
              // to a format that Google Charts understands.
              //
              // Whenever the data in AngularJS changes (either because of a 
              // user change or newer data from the server), this function is 
              // automatically called, so we don’t have to manually do any other 
              // work to ensure that our chart is updated.
              //
              function(newVal, oldVal) {
                var config = $scope.chartConfig;
                if (newVal) {
                  chart.draw(convertToPieChartDataTableFormat(
                    config.firstColumnHeader, config.secondColumnHeader, newVal),
                    {title: $scope.chartConfig.title}
                    );
                }
              }, true);

          }
        ); // end_then()
      } // end_link()
    };

  }]);

