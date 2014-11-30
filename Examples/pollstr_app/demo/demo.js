'use strict';

angular.module('pollstr.demo', ['ngRoute', 'ngUnderscore'])

.config(['$routeProvider', function($routeProvider) {
  $routeProvider.when('/demo', {
    templateUrl: 'demo/demo.html',
    controller: 'DemoCtrl'
  });
}])

.controller('DemoCtrl', ['$scope', '$log', '$http', 'underscore', 
    function($scope, $log, $http, underscore) {

        var _ = underscore,
            prev_section = [],
            prev_item = [];

        $scope.answer = {};
        $scope.section_index = 0;
        $scope.item_index = 0;
        $scope.showSubmit = false;
        $scope.showThanks = false;

        $http.get('data/demo.json').success(function (data) {
            $scope.sections = angular.fromJson(data).sections;
            $scope.current_section = $scope.sections[$scope.section_index];
            $scope.current_item = $scope.current_section.items[$scope.item_index];
        });

    $scope.showNext = function(response) {
        var section = $scope.current_section,
            skips = $scope.current_item.skips;

        $scope.answer[$scope.current_item.id] = response;
        prev_item.push($scope.item_index);
        prev_section.push($scope.section_index);

        if ($scope.item_index < section.items.length - 1) {
            if (skips !== undefined) {
                if(_.contains(skips.resp, response)) {
                    var next = _($scope.sections).chain()
                        .pluck('items')
                        .flatten()
                        .findWhere({id: skips.to})
                        .value();
                    $scope.current_item = next;
                    _($scope.sections).forEach(function(section) {
                        var index = section.items.indexOf(next);
                        if (index > 0) {
                            $scope.item_index = index;
                            $scope.current_section = section;
                            $scope.section_index = $scope.sections.indexOf(section);
                        }
                    });
                } else {
                    $scope.item_index++;
                    $scope.current_item = section.items[$scope.item_index];
                }
            } else {
                $scope.item_index++;
                $scope.current_item = section.items[$scope.item_index];
            }
        } else if ($scope.section_index < $scope.sections.length - 1) {
            $scope.section_index++;
            $scope.current_section = $scope.sections[$scope.section_index];
            $scope.item_index = 0;
            $scope.current_item = $scope.current_section.items[$scope.item_index];
        } else {
            $scope.showSubmit = true;
        }
    };

    $scope.back = function() {
        $scope.item_index = prev_item.pop();
        $scope.section_index = prev_section.pop();
        $scope.current_section = $scope.sections[$scope.section_index];
        $scope.current_item = $scope.current_section.items[$scope.item_index];
        $scope.showSubmit = false;
    };

    $scope.submit = function(answer) {
        // for now route to thank you page
        $scope.showThanks = true;
    };

    $scope.restart = function() {
        prev_item = [];
        $scope.answer = {};
        $scope.section_index = 0;
        $scope.item_index = 0;
        $scope.showSubmit = false;
        $scope.showThanks = false;
        $scope.current_section = $scope.sections[$scope.section_index];
        $scope.current_item = $scope.current_section.items[$scope.item_index];
    };
}]);