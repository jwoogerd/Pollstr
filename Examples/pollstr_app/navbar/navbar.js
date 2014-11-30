'use strict';

angular.module('pollstr.navbar', [])
    .controller('NavbarCtrl', ['$http', '$scope',
    function($http, $scope) {

        $http.get('data/demo.json').success(function (data) {
            var meta = angular.fromJson(data).meta;
            $scope.title = meta.title;
        });
}]);