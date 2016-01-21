(function() {
  var MoveStatus, flatten, mod, oneDecimal, zip,
      __slice = [].slice;

  mod = angular.module('PoseTeaching', []);

  mod.directive('modifiable', ['$timeout', function($timeout) {
    return {
      restrict: 'E',
      transclude: true,
      template: '<span class="{{inputClass}} modifiable" ng-hide="modifying" ng-click="modifying = modifying ? false : true" ng-transclude></span><input type="{{inputType}}" ng-model="modData" ng-show="modifying" ng-blur="modifying = false" />',
      scope: {
        modData: '='
      },
      link: {
        pre: function(scope, element, attrs) {
          scope.inputType = attrs.number ? "number" : "text";
          scope.inputClass = attrs.number ? "hljs-number" : "hljs-string";
        },
        post: function(scope, element) {
          scope.$watch('modifying', function(newValue, oldValue) {
            var inputElement;
            if (newValue && !oldValue) {
              inputElement = element.children()[1];
              $timeout(function() {
                inputElement.focus();
                inputElement.select();
              }, 10);
            }
          });
        }
      }
    };
  }]);


  /*
   mod.directive('robotManager', ->
   restrict: 'E'
   link: (scope, elem) ->
   elem.append(Linkbots.managerElement())
   )
   */

  MoveStatus = (function() {
    function MoveStatus(scope) {
      this.scope = scope;
      this.timeout = null;
      this.index = -1;
    }

    MoveStatus.prototype.stop = function() {
      return this.index = -1;
    };

    MoveStatus.prototype.running = function() {
      return this.timeout !== null;
    };

    MoveStatus.prototype.runningAt = function(idx) {
      return this.running() && idx === this.index;
    };

    MoveStatus.prototype.pausedAt = function(idx) {
      return !this.running() && idx === this.index;
    };

    MoveStatus.prototype.stopped = function() {
      return this.index === -1;
    };

    MoveStatus.prototype.pause = function() {
      clearTimeout(this.timeout);
      this.timeout = null;
      return this.decrementIndex();
    };

    MoveStatus.prototype.decrementIndex = function() {
      var p;
      p = this.scope.m.poses;
      return this.index = p.length > 0 ? (this.index + p.length - 1) % p.length : -1;
    };

    MoveStatus.prototype.incrementIndex = function() {
      return this.index = (this.index + 1) % this.scope.m.poses.length;
    };

    return MoveStatus;

  })();

  mod.controller('actions', [
    '$scope', function($scope) {
      var allRobotWheelPositions, doRecordPose, max_dTs, move, moveRobots, pauseProgram, runProgram, setupRobot, stopProgram;
      $scope.m = {
        loop: true,
        poses: [],
        robots: [],
        dT: 1,
        moveDelay: 0,
        defaultSpeeds: [90, 90, 90],
        speeds: [],
        moveStatus: new MoveStatus($scope)
      };
      $scope.addRobot = function() {
        var x;
        x = Linkbots.acquire(1);
        if (x.robots.length === 1) {
          setupRobot(x.robots[0]);
          return $scope.clearProgram();
        }
      };
      $scope.recordPose = function() {
        return doRecordPose();
      };
      $scope.clearProgram = function() {
        $scope.m.poses = [];
        return $scope.m.moveStatus.stop();
      };
      $scope.toggleRun = function() {
        if ($scope.m.moveStatus.running()) {
          return pauseProgram();
        } else {
          return runProgram();
        }
      };
      allRobotWheelPositions = function(callback) {
        var f, robots, values;
        robots = $scope.m.robots.slice();
        values = [];
        f = function() {
          var r;
          if (robots.length > 0) {
            r = robots.shift();
            return r.wheelPositions(function(v) {
              values.push(v.values.map(oneDecimal));
              return f();
            });
          } else {
            return callback(values);
          }
        };
        return f();
      };
      doRecordPose = function() {
        if (!$scope.m.moveStatus.running()) {
          return allRobotWheelPositions(function(values) {
            return $scope.$apply(function() {
              if ($scope.m.moveStatus.stopped()) {
                return $scope.m.poses.push(values);
              } else {
                $scope.m.poses.splice($scope.m.moveStatus.index + 1, 0, values);
                return $scope.m.moveStatus.incrementIndex();
              }
            });
          });
        }
      };
      setupRobot = function(robo) {
        var addPose, deletePose, regObj;
        robo.stop();
        $scope.m.robots.push(robo);
        $scope.m.speeds.push($scope.m.defaultSpeeds.slice());
        addPose = function() {
          if (!$scope.m.moveStatus.running()) {
            return allRobotWheelPositions(function(values) {
              return $scope.$apply(function() {
                if ($scope.m.moveStatus.stopped()) {
                  return $scope.m.poses.push(values);
                } else {
                  $scope.m.poses.splice($scope.m.moveStatus.index + 1, 0, values);
                  return $scope.m.moveStatus.incrementIndex();
                }
              });
            });
          }
        };
        deletePose = function() {
          if (!$scope.m.moveStatus.running()) {
            if ($scope.m.moveStatus.stopped()) {
              return $scope.m.poses.pop();
            } else {
              $scope.m.poses.splice($scope.m.moveStatus.index, 1);
              return $scope.m.moveStatus.decrementIndex();
            }
          }
        };
        regObj = {
          button: {}
        };
        regObj.button[robo.BUTTON_POWER] = {
          callback: function() {
            return $scope.$apply(function() {
              return deletePose();
            });
          }
        };
        regObj.button[robo.BUTTON_A] = {
          callback: function() {
            return $scope.$apply(function() {
              return addPose();
            });
          }
        };
        regObj.button[robo.BUTTON_B] = {
          callback: function() {
            return $scope.$apply(function() {
              return $scope.toggleRun();
            });
          }
        };
        return robo.register(regObj);
      };
      pauseProgram = function() {
        $scope.m.moveStatus.pause();
        return $scope.m.robots.map(function(r) {
          return r.stop();
        });
      };
      stopProgram = function() {
        pauseProgram();
        return $scope.m.moveStatus.stop();
      };
      runProgram = function() {
        var robots;
        robots = $scope.m.robots;
        if (!(robots.length > 0 && $scope.m.poses.length > 0)) {
          return;
        }
        zip(function(r, s) {
          return r.angularSpeed.apply(r, s);
        }, robots, $scope.m.speeds);
        return allRobotWheelPositions(function(values) {
          return move(values);
        });
      };
      move = function(curPositions) {
        var dT, destPositions, idx, nextCmd;
        idx = $scope.m.moveStatus.incrementIndex();
        destPositions = $scope.m.poses[idx];
        dT = max_dTs(curPositions, destPositions);
        moveRobots(destPositions);
        nextCmd = $scope.m.poses.length > 1 && ($scope.m.loop || idx !== $scope.m.poses.length - 1) ? function() {
          return move(destPositions);
        } : function() {
          return stopProgram();
        };
        return $scope.m.moveStatus.timeout = setTimeout(function() {
          return $scope.$apply(nextCmd);
        }, dT * 1000);
      };
      moveRobots = function(destPositions) {
        return zip(function(r, pos) {
          return r.moveTo.apply(r, pos);
        }, $scope.m.robots, destPositions);
      };
      return max_dTs = function(starts, dests) {
        var dT, dTs, max_dT;
        dT = function(start, dest, speeds) {
          return zip((function(s, d, v) {
            return Math.abs(d - s) / v;
          }), start, dest, speeds);
        };
        dTs = zip(dT, starts, dests, $scope.m.speeds);
        return max_dT = flatten(dTs).reduce(function(a, b) {
          return Math.max(a, b);
        });
      };
    }
  ]);

  oneDecimal = function(n) {
    return Math.round(n * 10) / 10;
  };

  flatten = function(a, n) {
    var i, register, _i, _ref;
    if (n == null) {
      n = 1;
    }
    register = a;
    for (i = _i = 1; 1 <= n ? _i <= n : _i >= n; i = 1 <= n ? ++_i : --_i) {
      register = (_ref = []).concat.apply(_ref, register);
    }
    return register;
  };

  zip = function() {
    var arrays, fn, i, len, _i, _ref, _results;
    fn = arguments[0], arrays = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
    len = arrays.map(function(a) {
      return a.length;
    }).reduce(function(a, b) {
      return Math.min(a, b);
    });
    if (len > 0) {
      _results = [];
      for (i = _i = 0, _ref = len - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; i = 0 <= _ref ? ++_i : --_i) {
        _results.push(fn.apply(null, arrays.map(function(a) {
          return a[i];
        })));
      }
      return _results;
    } else {
      return [];
    }
  };

}).call(this);
