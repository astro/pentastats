var MAX_KEYS = 6;

var app = angular.module('pentastats', []);
// app.config(function($routeProvider, $locationProvider) {
//     $routeProvider.
// 	otherwise({
// 	    controller: 'GraphsController'
// 	});
// });

function cmpByK(p1, p2) {
    if (p1.k < p2.k)
	return -1;
    else if (p1.k > p2.k)
	return 1;
    else
	return 0;
}

app.controller('SelectController', function($scope, $http, $rootScope, $location) {
    $rootScope.paths = [];
    $http({
	method: 'GET',
	url: "data/index.json"
    }).success(function(data) {
	$rootScope.groups = {};
	$scope.maxDownloads = 0;
	var k;
	for(k in data)
	    if (data.hasOwnProperty(k)) {
		var path = mapPath(k);
		if (path) {
		    var ps = path.split(/\//g);
		    var pLast = ps.pop();
		    var xs = pLast.split(/\./);
		    var base = ps.join("/") + "/" + xs[0];
		    var ext = xs[1] || "";
		    if (!$rootScope.groups.hasOwnProperty(base))
			$rootScope.groups[base] = [];
		    data[k].ext = ext;
		    $rootScope.groups[base].push(data[k]);
		}
	    }
	var paths = {};
	for(k in $rootScope.groups) {
	    var g = $rootScope.groups[k];
	    var downloads = 0;
	    g.forEach(function(path) {
		downloads += path.downloads;
	    });
	    if (downloads > $scope.maxDownloads)
		$scope.maxDownloads = downloads;
	    var m, base, k1;
	    if ((m = k.match(/^(.+?:\/\/[^\/]+)(.*)$/))) {
		base = m[1];
		k1 = m[2];
	    } else {
		base = "<unknown>";
		k1 = k;
	    }

	    if (base && k1) {
		if (!paths.hasOwnProperty(base))
		    paths[base] = {
			title: base,
			k: base,
			downloads: 0,
			children: {}
		    };
		paths[base].downloads += downloads;
		paths[base].children[k1] = {
		    k: k,
		    title: k1,
		    downloads: Math.ceil(downloads)
		};
	    } else if (base) {
		if (!paths.hasOwnProperty(base))
		    paths[base] = {
			downloads: 0,
			children: {}
		    };
		paths[base].k = k;
		paths[base].title = base;
		paths[base].downloads += downloads;
	    }
	}
	$scope.maxDownloadsSqrt = Math.sqrt($scope.maxDownloads);
	$scope.paths = Object.keys(paths).map(function(base) {
	    var path = paths[base];
	    path.children =
		Object.keys(path.children).map(function(k1) {
		    return path.children[k1];
		}).sort(cmpByK);
	    path.downloads = Math.ceil(path.downloads);
	    return path;
	}).sort(cmpByK);
    });
    // TODO: http error handling

    $scope.select = function(p) {
	console.log("select", p);
	$location.path(p.k);
    };

    $scope.counterColor = function(p) {
	var shade =
	    Math.max(
		0,
		127 - Math.ceil(127 * Math.sqrt(p.downloads) / $scope.maxDownloadsSqrt)
	    );
	return "rgb(" +
	    shade + "," + 
	    shade + "," + 
	    shade + ")";
    };
});

app.directive('chartContainer', function() {
    return {
	link: function(scope, element, attrs) {
	    var plot;

	    scope.$watch(function() {
		var data = scope.$eval(attrs.chartContainer);
		if (!data)
		    return;

		if (plot) {
		    plot.shutdown();
		    $(element[0]).empty();
		}

		setTimeout(function() {
		    plot = $.plot(element[0], data, {
			xaxis: {
			    mode: 'time',
			    timeformat: "%Y-%m-%d"
			},
			legend: {
			    show: true,
			    sorted: 'reverse'
			}
		    });
		}, 1);
	    });
	}
    };
});


function dataToChart(ks) {
    var chart = [];
    var day, key, keyTotals = {};
    for(key in ks) {
	if (!keyTotals.hasOwnProperty(key))
	    keyTotals[key] = 0;
	for(var day in ks[key]) {
	    keyTotals[key] += ks[key][day];
	}
    }
    var keyTotalsSorted = Object.keys(keyTotals).sort(function(k1, k2) {
	var t1 = keyTotals[k1];
	var t2 = keyTotals[k2];
	if (t1 > t2)
	    return -1;
	else if (t1 < t2)
	    return 1;
	else
	    return 0;
    });
    var topKeys, otherKeys;
    if (keyTotalsSorted.length > MAX_KEYS) {
	topKeys = keyTotalsSorted.slice(0, MAX_KEYS);
	if (topKeys.indexOf("*") < 0)
	    topKeys.push("*");
	otherKeys = keyTotalsSorted.slice(MAX_KEYS);
	if (otherKeys.length > 0 && !ks.hasOwnProperty("*"))
	    ks["*"] = {};
	otherKeys.forEach(function(key) {
	    if (key != "*") {
		for(var day in ks[key]) {
		    if (!ks["*"].hasOwnProperty(day))
			ks["*"][day] = 0;
		    ks["*"][day] += ks[key][day];
		}
		delete ks[key];
	    }
	});
    } else {
	topKeys = keyTotalsSorted;
	otherKeys = [];
    }
    /* For stacking: */
    var dayHeight = {};
    topKeys.reverse().forEach(function(key) {
	var series = {
	    label: (key == "*" ? "Other" : key) +
		" (" + Math.round(keyTotals[key]) + ")",
	    bars: {
		show: true,
		barWidth: Math.ceil(86400 * 1000),
		lineWidth: 0,
		fill: 1
	    },
	    data: []
	};
	for(var day in ks[key]) {
	    if (/^\d{4}-\d{2}-\d{2}$/.test(day)) {
		var time = new Date(day).getTime();
		var height = ks[key][day];
		if (!dayHeight.hasOwnProperty(day))
		    dayHeight[day] = 0;
		series.data.push([time, dayHeight[day] + height, dayHeight[day]]);
		dayHeight[day] += height;
	    }
	}
	chart.push(series);
    });

    return chart;
};

app.controller('GraphsController', function($scope, $rootScope, $location, $http) {
    $scope.currentPath = null;
    $scope.load = function() {
	if (!$rootScope.groups)
	    /* defer */
	    return;

	if ($scope.currentPath == $location.path())
	    return;
	$scope.currentPath = $location.path();

	var g, k;
	if ($rootScope.groups) {
	    k = $scope.currentPath;
	    g = $rootScope.groups[k];
	    if (!g) {
		k = $scope.currentPath.replace(/^\//, "");
		g = $rootScope.groups[k];
	    }
	}
	if (!g)
	    return;
	$scope.selectedPath = k;

	$scope.loading = true;
	var datas = {
	    ext: {},
	    user_agents: {},
	    geo: {}
	};
	g.forEach(function(path) {
	    $http({
		method: 'GET',
		url: "data/" + path.json + ".json"
	    }).success(function(res) {
		$scope.loading = false;
		/* Transpose & merge into datas.* */

		if (!datas.ext.hasOwnProperty(path.ext))
		    datas.ext[path.ext] = {};
		for(var day in res.downloads) {
		    if (!datas.ext[path.ext].hasOwnProperty(day))
			datas.ext[path.ext][day] = 0;
		    datas.ext[path.ext][day] += res.downloads[day];
		}

		for(var day in res.user_agents) {
		    for(var ua in res.user_agents[day]) {
			if (!datas.user_agents.hasOwnProperty(ua))
			    datas.user_agents[ua] = {};
			if (!datas.user_agents[ua].hasOwnProperty(day))
			    datas.user_agents[ua][day] = 0;
			datas.user_agents[ua][day] += res.user_agents[day][ua];
		    }
		}

		for(var day in res.geo) {
		    for(var country in res.geo[day]) {
			if (!datas.geo.hasOwnProperty(country))
			    datas.geo[country] = {};
			if (!datas.geo[country].hasOwnProperty(day))
			    datas.geo[country][day] = 0;
			datas.geo[country][day] += res.geo[day][country];
		    }
		}

		$scope.chart = {
		    ext: dataToChart(datas.ext),
		    geo: dataToChart(datas.geo),
		    user_agents: dataToChart(datas.user_agents),
		};
	    });
	    // TODO: http error handling
	});

    };

    $rootScope.$watch(function() {
	$scope.load();
    });
    $scope.load();
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
