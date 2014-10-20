// When we execute this application, we will see a form with two input fields 
// and the error messages for required immediately. As we type in, we will see 
// the required message switch with another validation message before finally 
// showing the valid form.
//
// As mentioned before, compile is only used in the rarest of cases, where you 
// need to do major DOM transformations at runtime. In a majority of cases, you
//  might be able to accomplish the same with transclusion, or pure link 
// function. But it does give you that extra flexibility when you need it.
//
angular.module('dynamicFormApp')
  .directive('formElement', [function() {

    return {
      restrict: 'E',
      // We add require to ensure the 'formElement' directive is used as a 
      // subchild of any form
      //
      require: '^form',
      // Give it a new child scope so that any functions we add are restricted 
      // and do not override any global variables or functions.
      //
      scope: true,
      // The compile function, which gets called with the 'element' and the  
      // 'attributes'. It executes before the scope is available, so it does 
      // not get the 'scope' injected in.
      //
      compile: function($element, $attrs) {
        var expectedInputAttrs = {
          'required': 'required',
          'ng-minlength': 'ngMinlength',
          'ng-pattern': 'ngPattern'
          // More here to be implemented
        };

        // Start extracting content from the HTML

        // Start extracting and parsing the existing form-element tag from the 
        // HTML, and picking out the validation rules, messages, and existing 
        // attributes that we care about.
        //
        var validationKeys = $element.find('validation');
        var presentValidationKeys = {};
        var inputName = $attrs.name;
        angular.forEach(validationKeys, function(validationKey) {
          validationKey = angular.element(validationKey);
          presentValidationKeys[validationKey.attr('key')] =
            validationKey.text();
        });

        // Start generating the new HTML that will be used for the directive. 
        // Because we will be adding AngularJS directives dynamically, we are 
        // doing this in the compile. 
        // 
        // If we do this in the link step, AngularJS won’t detect these 
        // directives and our application won’t work.
        //
        // Add the input tag with the name 'ng-model' and all the validations 
        // that were present in the HTML.
        //
        var elementHtml = '<div>' +
          '<label>' + $attrs.label + '</label>';
        elementHtml += '<input type="' + $attrs.type +
                            '" name="' + inputName +
                            '" ng-model="' + $attrs.bindTo + '"';

        $element.removeAttr('type');
        $element.removeAttr('name');
        for (var i in expectedInputAttrs) {
          if ($attrs[expectedInputAttrs[i]] !== undefined) {
            elementHtml += ' ' + i + '="' +
                $attrs[expectedInputAttrs[i]] + '"';
          }
          $element.removeAttr(i);
        }
        elementHtml += '>';

        elementHtml +=
            '<span ng-repeat="(key, text) in validators" ' +
                 ' ng-show="hasError(key)"' +
                 ' ng-bind="text"></span>';

        elementHtml += '</div>';

        // Replace the existing content of the directive with this newly generated 
        // content.
        //
        $element.html(elementHtml);

        // Return a 'postLink' function (we cannot have a link keyword along with 
        // compile; we need to return the link function from within compile 
        // instead), which adds the 'validators' array and a 'hasError' function to 
        // show each of the validation messages under the correct conditions. 
        //
        return function($scope, $element, $attrs, formCtrl) {
          $scope.validators = angular.copy(presentValidationKeys);
          $scope.hasError = function(key) {
            return !!formCtrl[inputName]['$error'][key];
          };
        };
      }
    };
  }]);
