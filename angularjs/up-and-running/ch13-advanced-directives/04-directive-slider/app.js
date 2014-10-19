// The 'MainCtrl' controller.
//
angular.module('sliderApp', [])
  .controller('MainCtrl', [function() {
    var self = this;

    // UI Slide bar state.
    self.selectedValue = 2000;
    // TextBox input state.
    self.textValue = 4000;

    // Function to select new 'selectValue' slider state 
    // fomr the specified text box 'textValue' state.
    self.setSelectedValue = function() {
      self.selectedValue = self.textValue;
    };

  }]);
