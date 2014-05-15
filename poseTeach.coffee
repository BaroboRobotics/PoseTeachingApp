mod = angular.module('PoseTeaching', [])

mod.controller('actions', ['$scope', ($scope) ->
  $scope.m =
    poses: []
    robot: null
    dT: 1
    moveDelay: 0

  $scope.connect = () ->
    rid = $scope.m.robotIdInput
    try
      robo = Linkbots.connect(rid)
      robo.stop()
      $scope.m.robot = robo
      handleButton = (r,m,e) ->
        $scope.$apply(->
          $scope.m.poses.push r.wheelPositions()
        )

      robo.register(
        button:
          1: callback: handleButton
      )
      $scope.m.robotIdInput = null
    catch e
      console.log e

  $scope.clearProgram = () ->
    $scope.m.poses = []

  $scope.stopProgram = () ->

  $scope.runProgram = () ->
    robot = $scope.m.robot
    return unless robot?

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
          move()
          setTimeout(rest, ($scope.m.dT + $scope.m.moveDelay) * 1000)
      -> $scope.m.robot.stop()
    )

    allMoves()

  ##
  # Sub functions for runProgram:
  ##

  # makeMove: helper function to make a single move.
  #
  # Speed set so dT (i.e. time taken to make the move) is constant.
  # start and dest are both Arrays of wheel positions.
  makeMove = (start, dest) ->
    dXs = zip(((s, d) -> d - s), start, dest)
    vs = dXs.map((dX) -> Math.abs(dX)/$scope.m.dT)
    ->
      $scope.m.robot.angularSpeed(vs...)
      $scope.m.robot.moveTo(dest...)

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
