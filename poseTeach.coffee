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

oneDecimal = (n) ->
  (Math.round(n * 10) / 10)

mod.controller('actions', ['$scope', ($scope) ->
  $scope.m =
    poses: []
    robots: []
    dT: 1
    moveDelay: 0
    defaultSpeeds: [90,90,90]
    speeds: []

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
          positions =
            $scope.m.robots.map(
              (r) -> r.wheelPositions().map(oneDecimal)
            )
          $scope.m.poses.push positions
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

  $scope.runProgram = () ->
    robot = $scope.m.robot
    return unless robot? and $scope.m.poses.length > 0

    robot.angularSpeed($scope.m.speeds...)

    destPositions = $scope.m.poses
    # First start position is current position
    startPositions = $scope.m.poses.slice()
    startPositions.unshift(robot.wheelPositions())

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
          setTimeout(rest, (move.dT + $scope.m.moveDelay) * 1000)
      -> robot.stop()
    )

    allMoves()

  ##
  # Sub functions for runProgram:
  ##

  # makeMove: helper function to make a single move.
  #
  # Calculate max time (dT) for the whole move.
  # start and dest are both Arrays of wheel positions.
  makeMove = (start, dest) ->
    dXs = zip(((s, d) -> d - s), start, dest)
    max_dT = zip(
      ((dX, v) -> Math.abs(dX)/v)
      dXs
      $scope.m.speeds
    ).reduce((a, b) -> Math.max(a,b))

    {
      cmd: -> $scope.m.robot.moveTo(dest...)
      dT: max_dT
    }

  # zip: our old friend
  zip = (fn, arrays...) ->
    len = arrays.map((a) -> a.length)
                .reduce((a, b) -> Math.min(a,b))
                # .reduce(Math.min) returns NaN as of 2014-05-14....
    if len > 0
      for i in [0..len-1]
        fn.apply(null, arrays.map((a) -> a[i]))
    else []

])
