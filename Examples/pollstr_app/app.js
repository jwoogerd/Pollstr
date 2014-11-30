'use strict';

// Declare app level module which depends on views, and components
angular.module('pollstr', [
  'ngRoute',
  'ngUnderscore',
  'pollstr.demo',
  'pollstr.navbar',
  'pollstr.version'
]).
config(['$routeProvider', function($routeProvider) {
  $routeProvider.otherwise({redirectTo: '/demo'});
}]);
