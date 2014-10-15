// Work has to be done for Anlytics to work on an SPA site.
//
// Tools to help include 'angularytics'.
// 
angular.module('myTrackingApp', ['angularytics'])

  .config(['AngularyticsProvider', function(AngularyticsProvider) {
    AngularyticsProvider.setEventHandlers(
      ['GoogleUniversal', 'Console']);
    }])
  
  .run(['Angularytics', function(Angularytics) {
          Angularytics.init();
  }])