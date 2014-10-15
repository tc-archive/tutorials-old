// To enable HTML5 mode, three things are needed:
//
// • Enable HTML5 mode as part of the application config on the client side 
//   as below...
//
// • In index.html, add the <base> tag with an href attribute to the <head> 
//   portion. 
//
//   This is to tell the browser where, in relation to the URL, the static 
//   resources are served from, so that if the application requests an image 
//   or CSS file with a relative path, it doesn’t take it from the current 
//   URL necessarily. To resolve all paths to 'server_url/app/xxx'
//
//   <html><head><base href="/app"/></head></html>
//
// • On the server side, we need a rule that states that when the server 
//   sees a request for '/first/page' , '/second/page', etc., it needs to 
//   serve the content that it normally serves for the '/'' request, which 
//   is usually 'index.html'.
//
//
angular.module('myHtml5App', ['ngRoute']) 
  
  .config(['$locationProvider', '$routeProvider',
    function($locationProvider, $routeProvider) {

      // To set HTML5 mode, we ask for '$locationProvider' as part of the 
      // configuration, and call the function 'html5Mode' with 'true' on 
      // it.
      // 
      // It’s recommended that we also set the hashPrefix as '!'' to easily 
      // support SEO.
      // This is all we  need to do on the client side for nonhash URLs in 
      // AngularJS.
      //
      $locationProvider.html5Mode(true);
      //Optional
      $locationProvider.hashPrefix('!');
      // Route configuration here as normal // Route for /first/page
      // Route for /second/page
  }]);