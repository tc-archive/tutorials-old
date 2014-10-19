//=============================================================================
// Commands for setting up Karma for Jasmine and Chrome (NB: Requires NPM)
//=============================================================================

//-----------------------------------------------------------------------------
// GLOBAL - Install the 'Karma-Cli' (if not yest installed)
//-----------------------------------------------------------------------------
// npm install -g karma-cli


//-----------------------------------------------------------------------------
// LOCAL - Install the 'Karma' and the 'Plugins' for 'Jasmine' and 'Chrome'.
//-----------------------------------------------------------------------------
// npm install karma karma-jasmine karma-chrome-launcher


//-----------------------------------------------------------------------------
// LOCAL - Generate a default Config file (Answer questions as approriate!)
//-----------------------------------------------------------------------------
// karma init


//-----------------------------------------------------------------------------
// LOCAL - Run Karma
//-----------------------------------------------------------------------------
// karma start my.conf.js


// Karma configuration

module.exports = function(config) {
  config.set({
    basePath: '',
    frameworks: ['jasmine'],
    files: [
      'src/lib/angular.min.js',
      'src/*.js',
      'test/lib/angular.mocks.js',
      'test/*.js'
    ],
    exclude: [],
    port: 8080,
    logLevel: config.LOG_INFO,
    autoWatch: true,
    browsers: ['Chrome'],
    singleRun: false
  });
};
