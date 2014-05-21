mod = angular.module('PoseTeaching', [])

mod.directive('modifiable', ->
  restrict: 'E'
  transclude: true
  template: '''
    <span ng-hide="modifyIt"
          ng-click="modifyIt = modifyIt ? false : true"
          ng-transclude>
    </span><input type="{{inputType}}" ng-model="modData"
          ng-show="modifyIt"
          ng-blur="modifyIt = false">
    '''
  scope:
    modData: "="

  link:
    pre: (scope, elem, attrs) ->
      scope.inputType =
        if attrs.number
          "number"
        else
          "text"
    post: (scope, elem) ->
      scope.$watch('modifyIt', (newValue, oldValue) ->
        if newValue && !oldValue
          inputElem = elem.children()[1]
          inputElem.focus()
          inputElem.select()
      )
)

mod.controller('actions', ['$scope', ($scope) ->
  $scope.m =
    poses: []
    robots: []
    dT: 1
    moveDelay: 0
    defaultSpeeds: [90,90,90]
    speeds: []
    moveStatus:
      timeout: null
      index: -1

  $scope.connect = () ->
    rid = $scope.m.robotIdInput
    $scope.m.robotIdInput = null
    for r in $scope.m.robots
      return if rid == r._id
    try
      setupRobot(rid)
      $scope.m.poses = []
    catch e
      console.log e

  $scope.clearProgram = () ->
    $scope.m.poses = []
    $scope.m.moveStatus.index = -1

  $scope.toggleRun = () ->
    if $scope.m.moveStatus.timeout
      pauseProgram()
    else
      runProgram()

  ##
  # Subfunctions
  ##

  allRobotWheelPositions = ->
    $scope.m.robots.map(
      (r) -> r.wheelPositions().map(oneDecimal)
    )

  setupRobot = (rid) ->
    # connect() may throw, of course, short circuiting the rest of this
    # function.
    robo = Linkbots.connect(rid)
    robo.stop()
    $scope.m.robots.push robo
    $scope.m.speeds.push $scope.m.defaultSpeeds.slice()

    addPose = ->
      # Only add if paused
      if ! $scope.m.moveStatus.timeout
        # FIXME make this "if $scope.m.moveStatus.stopped()"
        if $scope.m.moveStatus.index < 0
          $scope.m.poses.push allRobotWheelPositions()
        else
          $scope.m.poses.splice(
            $scope.m.moveStatus.index + 1
            0
            allRobotWheelPositions()
          )

    deletePose = ->

    robo.register(
      button:
        0: callback: -> $scope.$apply(-> addPose())
        1: callback: -> $scope.$apply(-> $scope.toggleRun())
        2: callback: deletePose
    )

  pauseProgram = () ->
    clearTimeout($scope.m.moveStatus.timeout)
    $scope.m.robots.map((r) -> r.stop())
    $scope.m.moveStatus.timeout = null
    decrementMoveIndex()

  stopProgram = () ->
    pauseProgram()
    $scope.m.moveStatus.index = -1

  runProgram = () ->
    robots = $scope.m.robots
    return unless robots.length > 0 and $scope.m.poses.length > 0

    zip(
      (r, s) -> r.angularSpeed(s...)
      robots
      $scope.m.speeds
    )

    curPositions = allRobotWheelPositions()
    move(curPositions)

  ##
  # Sub functions
  ##

  move = (curPositions) ->
    idx = incrementMoveIndex()
    destPositions = $scope.m.poses[idx]
    dT = max_dTs(curPositions, destPositions)

    moveRobots(destPositions)

    nextCmd =
      if $scope.m.poses.length > 1
        -> move(destPositions)
      else
        -> stopProgram()

    $scope.m.moveStatus.timeout = setTimeout(
      -> $scope.$apply(nextCmd)
      dT * 1000
    )

  incrementMoveIndex = ->
    $scope.m.moveStatus.index =
      ($scope.m.moveStatus.index + 1) % $scope.m.poses.length

  decrementMoveIndex = ->
    s = $scope.m.moveStatus
    p = $scope.m.poses
    s.index =
      (s.index + p.length - 1) % p.length

  moveRobots = (destPositions) ->
    zip(
      (r,pos) -> r.moveTo(pos...)
      $scope.m.robots
      destPositions
    )

  max_dTs = (starts, dests) ->
    # Given three 1-d arrays; the start, dest, and speed of a single robot;
    # dT is:
    dT = (start, dest, speeds) ->
      zip(((s, d, v) -> Math.abs(d - s)/v), start, dest, speeds)

    dTs = zip(dT, starts, dests, $scope.m.speeds)

    # Now flatten and find the max
    max_dT = flatten(dTs).reduce((a, b) -> Math.max(a,b))

])

##
# General utilities
##

oneDecimal = (n) ->
  (Math.round(n * 10) / 10)

# Flatten n levels of a nested array
flatten = (a, n = 1) ->
  register = a
  for i in [1..n]
    register = [].concat(register...)
  register

# zip: our old friend
zip = (fn, arrays...) ->
  len = arrays.map((a) -> a.length)
              .reduce((a, b) -> Math.min(a,b))
              # .reduce(Math.min) returns NaN as of 2014-05-14....
  if len > 0
    for i in [0..len-1]
      fn.apply(null, arrays.map((a) -> a[i]))
  else []
