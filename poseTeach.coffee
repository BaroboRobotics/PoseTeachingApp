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
###
mod.directive('robotManager', ->
  restrict: 'E'
  link: (scope, elem) ->
    elem.append(Linkbots.managerElement())
)
###

class MoveStatus
  constructor: (@scope) ->
    @timeout = null
    @index = -1

  stop: -> @index = -1
  running: -> @timeout != null
  runningAt: (idx) ->
    @running() && idx == @index
  pausedAt: (idx) ->
    !@running() && idx == @index
  stopped: -> @index == -1
  pause: ->
    clearTimeout(@timeout)
    @timeout = null
    @decrementIndex()
  decrementIndex: ->
    p = @scope.m.poses
    @index =
      if p.length > 0
        (@index + p.length - 1) % p.length
      else
        -1
  incrementIndex: ->
    @index =
      (@index + 1) % @scope.m.poses.length


mod.controller('actions', ['$scope', ($scope) ->
  $scope.m =
    loop: true
    poses: []
    robots: []
    dT: 1
    moveDelay: 0
    defaultSpeeds: [90,90,90]
    speeds: []
    moveStatus: new MoveStatus($scope)

  $scope.addRobot = () ->
    x = Linkbots.acquire(1)
    if x.robots.length == 1
      setupRobot(x.robots[0])
      $scope.clearProgram()

  $scope.recordPose = () ->
    doRecordPose()

  $scope.clearProgram = () ->
    $scope.m.poses = []
    $scope.m.moveStatus.stop()

  $scope.toggleRun = () ->
    if $scope.m.moveStatus.running()
      pauseProgram()
    else
      runProgram()

  ##
  # Subfunctions
  ##

  allRobotWheelPositions = (callback) ->
    robots = $scope.m.robots.slice();
    values = []
    
    f = ->
      if (robots.length > 0)
        r = robots.shift();  
        r.wheelPositions((v) ->
          values.push(v.values.map(oneDecimal));
          f()
        )
      else
        callback(values)
    f()

  doRecordPose = ->
    if ! $scope.m.moveStatus.running()
      allRobotWheelPositions((values) ->
        $scope.$apply(() ->
          if $scope.m.moveStatus.stopped()
            $scope.m.poses.push values
          else
            $scope.m.poses.splice(
              $scope.m.moveStatus.index + 1
              0
              values
            )
            $scope.m.moveStatus.incrementIndex()
        )
      )

  setupRobot = (robo) ->
    robo.stop()
    $scope.m.robots.push robo
    $scope.m.speeds.push $scope.m.defaultSpeeds.slice()

    addPose = ->
      if ! $scope.m.moveStatus.running()
        allRobotWheelPositions((values) ->
          $scope.$apply(() ->
            if $scope.m.moveStatus.stopped()
              $scope.m.poses.push values
            else
              $scope.m.poses.splice(
                $scope.m.moveStatus.index + 1
                0
                values
              )
              $scope.m.moveStatus.incrementIndex()
          )
        )
        

    deletePose = ->
      if ! $scope.m.moveStatus.running()
        if $scope.m.moveStatus.stopped()
          $scope.m.poses.pop()
        else
          $scope.m.poses.splice(
            $scope.m.moveStatus.index
            1
          )
          $scope.m.moveStatus.decrementIndex()

    regObj = button:{}
    regObj.button[robo.BUTTON_POWER] = callback: -> $scope.$apply(-> deletePose())
    regObj.button[robo.BUTTON_A] = callback: -> $scope.$apply(-> addPose())
    regObj.button[robo.BUTTON_B] = callback: -> $scope.$apply(-> $scope.toggleRun())
    robo.register(regObj)

  pauseProgram = () ->
    $scope.m.moveStatus.pause()
    $scope.m.robots.map((r) -> r.stop())

  stopProgram = () ->
    pauseProgram()
    $scope.m.moveStatus.stop()

  runProgram = () ->
    robots = $scope.m.robots
    return unless robots.length > 0 and $scope.m.poses.length > 0

    zip(
      (r, s) -> r.angularSpeed(s...)
      robots
      $scope.m.speeds
    )
    allRobotWheelPositions((values) ->
      move(values)
    )
#    curPositions = allRobotWheelPositions()
#    move(curPositions)

  ##
  # Sub functions
  ##

  move = (curPositions) ->
    idx = $scope.m.moveStatus.incrementIndex()
    destPositions = $scope.m.poses[idx]
    dT = max_dTs(curPositions, destPositions)

    moveRobots(destPositions)

    nextCmd =
      if $scope.m.poses.length > 1 &&
            ($scope.m.loop || idx != $scope.m.poses.length - 1)
        -> move(destPositions)
      else
        -> stopProgram()

    $scope.m.moveStatus.timeout = setTimeout(
      -> $scope.$apply(nextCmd)
      dT * 1000
    )

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
