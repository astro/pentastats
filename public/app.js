var app = angular.module('pentastats', []);

app.controller('MainController', function($scope, $http) {
    $scope.panes = [];
    $scope.addPane = function() {
	var last = $scope.panes[$scope.panes.length - 1];
	fileKey = last && last.fileKey;
	$scope.panes.push({
	    fileKey: fileKey
	});
    };
    $scope.addPane();

    $scope.paths = [];
    $http({
	method: 'GET',
	url: "data/index.json"
    }).success(function(data) {
	$scope.paths = data;
    });
    // TODO: http error handling
});

app.directive('chartContainer', function() {
    return {
	link: function(scope, element, attrs) {
		// element.attr('width', window.innerWidth);
		// element.attr('height', 200);
	    var plot;

	    scope.$watch('data', function() {
		if (!scope.data)
		    return;
		if (plot) {
		    plot.shutdown();
		    $(element[0]).empty();
		}

		console.log("plot", scope.data);
		plot = $.plot(element[0], scope.data, {
		    xaxis: {
			mode: 'time',
			timeformat: "%Y-%m-%d"
		    }
		});
	    });
	}
    };
});

app.controller('PaneController', function($scope, $http) {
    $scope.$watch('pane.fileKey', function() {
	$http({
	    method: 'GET',
	    url: "data/" + $scope.pane.fileKey + ".json"
	}).success(function(data) {
	    var k, startDate, stopDate;
	    $scope.data = [];

	    var series = {
		bars: {
		    show: true,
		    barWidth: 86400 * 1000
		},
		data: []
	    };
	    for(k in data)
		if (/^\d{4}-\d{2}-\d{2}$/.test(k)) {
		    var time = new Date(k).getTime();
		    series.data.push([time, data[k]]);
		}

	    series.data = series.data.sort();
	    $scope.data = [series];
	});
	// TODO: http error handling
    });
});

function pad(s, padding, len) {
    if (typeof s !== 'string')
	s = "" + s;

    while(s.length < len)
	s = padding + s;

    return s;
}

function fmtDate(d) {
    return pad(d.getUTCFullYear(), "0", 4) + "-" +
	pad(d.getUTCMonth(), "0", 2) + "-" +
	pad(d.getUTCDate(), "0", 2);
}
