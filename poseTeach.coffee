actions = ($scope) ->
  $scope.m =
    poses: []

  $scope.connect = () ->
    rid = $scope.m.robotId
    try
      robo = Linkbots.connect(rid)
      robo.stop()
      handleButton = (r,m,e) ->
        $scope.$apply(->
          $scope.m.poses.push r.wheelPositions()
        )

      robo.register(
        button:
          1: callback: handleButton
      )
    catch e
      console.log e

  $scope.runProgram = () ->

  $scope.clearProgram = () ->
    $scope.m.poses = []

  $scope.stopProgram = () ->
