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
  allRobotWheelPositions = ->
    $scope.m.robots.map(
      (r) -> r.wheelPositions().map(oneDecimal)
    )

  $scope.m =
    poses: []
    robots: []
    dT: 1
    moveDelay: 0
    defaultSpeeds: [90,90,90]
    speeds: []
    timeout: null

  $scope.connect = () ->
    rid = $scope.m.robotIdInput
    $scope.m.robotIdInput = null
    for r in $scope.m.robots
      return if rid == r._id
    try
      robo = Linkbots.connect(rid)
      robo.stop()
      $scope.m.robots.push robo
      $scope.m.speeds.push $scope.m.defaultSpeeds
      handleButton = (r,m,e) ->
        $scope.$apply(->
          $scope.m.poses.push allRobotWheelPositions()
        )

      robo.register(
        button:
          1: callback: handleButton
      )
      $scope.m.poses = []
    catch e
      console.log e

  $scope.clearProgram = () ->
    $scope.m.poses = []

  $scope.stopProgram = () ->

  $scope.toggleRun = () ->
    if $scope.m.timeout
      stopProgram()
    else
      runProgram()

  ##
  # Subfunctions for toggleRun: stopProgram() and runProgram()
  ##
  stopProgram = () ->
    clearTimeout($scope.m.timeout)
    $scope.m.robots.map((r) -> r.stop())
    $scope.m.timeout = null

  runProgram = () ->
    robots = $scope.m.robots
    return unless robots.length > 0 and $scope.m.poses.length > 0

    zip(
      (r, s) -> r.angularSpeed(s...)
      robots
      $scope.m.speeds
    )

    destPositions = $scope.m.poses
    # First start position is current position
    startPositions = $scope.m.poses.slice()
    startPositions.unshift(allRobotWheelPositions())

    moves = zip(
      makeMove
      startPositions
      destPositions
    )

    # Create a matrioshka doll of setTimeouts.
    allMoves = moves.reduceRight(
      (rest, move) ->
        ->
          move.cmd()
          $scope.m.timeout =
            setTimeout(rest, (move.dT + $scope.m.moveDelay) * 1000)
      -> $scope.$apply(-> stopProgram())
    )

    allMoves()

  ##
  # Sub functions for runProgram:
  ##

  # makeMove
  #
  # Returns an obj: a function that will move all robots to the
  # destination, and the amount of time dT the move takes.
  #
  # starts and dests are 2-d arrays keyed by (robot, wheel).
  makeMove = (starts, dests) ->

    cmd: ->
      zip(
        (r, d) -> r.moveTo(d...)
        $scope.m.robots
        dests
      )
    dT: max_dTs(starts, dests)

  # max_dTs
  #
  # Given an array of starts and dests (same as makeMove), calculated dXs,
  # then dTs given speeds in the scope, then return max dT.
  max_dTs = (starts, dests) ->
    # Given three 1-d arrays, the start, dest, and speed of a single robot,
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
