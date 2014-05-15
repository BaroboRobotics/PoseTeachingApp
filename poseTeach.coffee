actions = ($scope) ->
  $scope.m =
    poses: []
    robot: null

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

  $scope.runProgram = () ->

  $scope.clearProgram = () ->
    $scope.m.poses = []

  $scope.stopProgram = () ->
