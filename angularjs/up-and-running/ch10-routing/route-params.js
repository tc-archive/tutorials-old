// 'routeParams'
//
// In a Single Page Application, a controller and a route should be able 
// to independently bootstrap themselves and not expect that the user 
// first goes to a list page and then to the details page. 
//
// In AngularJS these URL parameters don’t have to be parsed from the URL, 
// but can directly be accessed from a convenient service that called 
// '$routeParams'

angular.module('resolveApp', ['ngRoute'])
  .config(['$routeProvider', function($routeProvider) {
    $routeProvider
      .when(
        '/', {
          template: '<h1>Main Page</h1>'
        })
      // This route tells the AngularJS routing that there will be a value 
      // after the '/detail' in the URL that needs to be picked up, stored, 
      // and provided as detId to the controller.
      .when(
        '/detail/:detId', {
          template: '<h2>' +
                      'Loaded {{myCtrl.detailId}} ' +  
                      ' and query String is {{myCtrl.qStr}}' + 
                    '</h2>', 
          controller: ['$routeParams', function($routeParams) {
            this.detailId = $routeParams.detId;
            this.qStr = $routeParams.q; 
            }],
          controllerAs: 'myCtrl'
        }
      );
    }
  ]);
