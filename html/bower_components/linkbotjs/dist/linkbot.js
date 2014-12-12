baroboBridge = (function(main) {
    var _i, _j, _len, _len1;
    if (main.baroboBridge && main.baroboBridge !== null) {
      return main.baroboBridge;
    } else {
      methods = ['angularSpeed', 'availableFirmwareVersions', 'buttonChanged', 'buzzerFrequency',
        'connectRobot', 'disconnectRobot', 'enableButtonSignals', 'enableMotorSignals', 'enableAccelSignals', 'disableAccelSignals',
        'disableButtonSignals', 'disableMotorSignals', 'firmwareVersion', 'getMotorAngles', 'moveTo',
        'scan', 'setMotorEventThreshold', 'stop', 'moveContinuous'];
      signals = ['accelChanged', 'motorChanged', 'buttonChanged'];
      obj = {
        mock: true
      };
      var randomInt = function(min,max) {
        return Math.floor(Math.random()*(max-min+1)+min);
      };
      var colorMap = {};
      var emptyFunction = function() { };
      for (_i = 0, _len = methods.length; _i < _len; _i++) {
        k = methods[_i];
        obj[k] = emptyFunction;
      }
      for (_j = 0, _len1 = signals.length; _j < _len1; _j++) {
        k = signals[_j];
        obj[k] = {
          connect: emptyFunction,
          disconnect: emptyFunction
        };
      }
      obj.getLEDColor = function(id) {
        if (!colorMap[id]) {
          colorMap[id] = {red:randomInt(0,255), green:randomInt(0,255), blue:randomInt(0,255)};
        }
        return colorMap[id];
      };
      obj.setLEDColor = function(id, r, g, b) {
        colorMap[id] = {red:r, green:g, blue:b};
      };
      return obj;
    }
})(this);;function RobotStatus() {
  var status = this;
  // Private


  // Public
  this.robots = [];

  this.list = function() {
    return status.robots;
  };

  this.acquire = function(n) {
    var readyBots = status.robots.filter(function(r) {
      return r.status === "ready";
    });
    var ret = {
        robots: [],
        registered: status.robots.length,
        ready: readyBots.length
    };
    if (ret.ready >= n) {
        rs = readyBots.slice(0, n);
        rs.map(function(r) {
          r.status = "acquired";
          return r.status;
        });
        ret.robots = rs.map(function(r) {
          return r.linkbot;
        });
        ret.ready -= n;
    }
    return ret;
  };

  this.add = function(id) {
    if (status.robots.map(function(x) {
      return x.id;
    }).indexOf(id) < 0) {
      return status.robots.push({
        status: "new",
        id: id
      });
    } else {
      return false;
    }
  };

  this.remove = function(id) {
    var idx = status.robots.map(function(x) {
      return x.id;
    }).indexOf(id);
    if (idx >= 0) {
      return status.robots.splice(idx, 1);
    } else {
      return false;
    }
    
  };

  this.relinquish = function(bot) {
    var idx = status.robots.map(function(x) {
      return x.id;
    }).indexOf(bot._id);
    if (idx >= 0 && status.robots[idx].status === "acquired") {
        status.robots[idx].status = "ready";
        return status.robots[idx].status;
      } else {
        return false;
      }
  };

  this.ready = function(idx, bot) {
    var robot = status.robots[idx];
    var result;
    if (robot !== null) {
      robot.linkbot = bot;
      robot.status ="ready";
    }
  };

  this.fail = function(idx) {
    var robot = status.robots[idx];
    if (robot !== null) {
      robot.status = "failed";
    }
  };
}
;function RobotManager(document) {
    // Private
    var manager = this;
    var events = {
        'add': [],
        'remove': [],
        'moved': []
    };
    var _controlPanelRobot = null;
    var _robotStatusInterval = null;

    function _ledChanged(id, red, green, blue) {
        manager.redraw();
    }

    function _checkRobotStatus() {
        var robotList, color, r, i, bot, redraw;
        redraw = false;
        robotList = manager.robots.list();
        for (i = 0; i < robotList.length; i++) {
            r = robotList[i];
            if (r.linkbot) {
                color = r.linkbot.getColor();
                if (color.red === 0 && color.green === 0 && color.blue === 0) {
                    // then possibly offline and need to reconnect.
                    redraw = true;
                    bot = new Linkbot(r.id);
                    if (bot._id !== null) {
                        manager.robots.ready(i, bot);
                        bot.register({
                            led: {
                                callback: _ledChanged
                            }
                        });
                    } else {
                        manager.robots.fail(i);
                    }
                }
            } else {
                // Attempt to connect to robot.
                bot = new Linkbot(r.id);
                if (bot._id !== null) {
                    manager.robots.ready(i, bot);
                    bot.register({
                        led: {
                            callback: _ledChanged
                        }
                    });
                    redraw = true;
                } else {
                    manager.robots.fail(i);
                }
            }
        }
        if (redraw === true) {
            manager.redraw();
        }
    }

    function controlZeroPressed() {
        _controlPanelRobot.linkbot.moveTo(0, 0, 0);
    }

    function controlBeepClicked() {
        var val = LinkbotControls.slider.getValue('buzzer-frequency-id');
        _controlPanelRobot.linkbot.buzzerFrequency(val);
        setTimeout(function() { _controlPanelRobot.linkbot.buzzerFrequency(0); }, 250);
    }

    function controlBeepDown() {
        var val = LinkbotControls.slider.getValue('buzzer-frequency-id');
        _controlPanelRobot.linkbot.buzzerFrequency(val);
    }

    function controlBeepUp() {
        _controlPanelRobot.linkbot.buzzerFrequency(0);
    }

    function controlSpeedChanged(value) {
        var j1, j2;
        j1 = LinkbotControls.slider.getValue('speed-joint-1');
        j2 = LinkbotControls.slider.getValue('speed-joint-2');
        _controlPanelRobot.linkbot.angularSpeed(j1, 0, j2);
    }

    function controlKnobChanged(value) {
        var j1, j2;
        j1 = LinkbotControls.knob.getInternalValue('position-joint-1');
        j2 = LinkbotControls.knob.getInternalValue('position-joint-2');
        _controlPanelRobot.linkbot.moveTo(j1, 0, j2);
    }

    function controlAccelChanged(robot, data, event) {
        LinkbotControls.slider.get('accel-xaxis').setValue(event.x);
        LinkbotControls.slider.get('accel-yaxis').setValue(event.y);
        LinkbotControls.slider.get('accel-zaxis').setValue(event.z);
        var mag = Math.sqrt((event.x * event.x)  + (event.y * event.y) + (event.z * event.z));
        LinkbotControls.slider.get('accel-mag').setValue(mag);
    }

    function showControlPanel(e, r) {
        if (r.status == 'failed') {
            // TODO add error message here.
            return;
        }
        // Show control panel.
        var contrlEl = document.getElementById('robomgr-control-panel');
        contrlEl.className = '';
        var overlay = document.getElementById('robomgr-overlay');
        overlay.className = '';
        var linkbotName = document.getElementById('robomgr-control-panel-linkbot');
        linkbotName.innerText = 'Linkbot ' + r.id;
        // Set tabs correctly.
        document.getElementById('robomgr-tab-control-panel').className = '';
        document.getElementById('robomgr-tab-sensors-panel').className = 'robomgr-hide';
        document.getElementById('robomgr-tab-control').parentElement.className='robomgr-active';
        document.getElementById('robomgr-tab-sensors').parentElement.className='';
        document.getElementById('linkbotjs-led-color').value = '#' + _colorToHex(r.linkbot.getColor());
        // TODO handle logic for robot control
        _controlPanelRobot = r;
        _controlPanelRobot.linkbot.angularSpeed(50, 0, 50);
        LinkbotControls.slider.get('speed-joint-1').setValue(50);
        LinkbotControls.slider.get('speed-joint-2').setValue(50);
        LinkbotControls.slider.get('buzzer-frequency-id').setValue(440);
        pos = _controlPanelRobot.linkbot.wheelPositions();
        if (pos) {
            LinkbotControls.knob.get('position-joint-1').setValue(pos[0]);
            LinkbotControls.knob.get('position-joint-2').setValue(pos[3]);
        }
        _controlPanelRobot.linkbot.register({
            accel: {
                callback: controlAccelChanged
            },
            wheel: {
                0: {
                    distance: 1,
                    callback: function(robot, data, event) {
                        LinkbotControls.knob.get('position-joint-1').setValueWithoutChange(event.position);
                    }
                },
                2: {
                    distance: 1,
                    callback: function(robot, data, event) {
                        LinkbotControls.knob.get('position-joint-2').setValueWithoutChange(event.position);
                    }
                }
            },
            button: {
                0: {
                    callback: function(robot, data, event) {
                        console.log(event);
                    }
                },
                1: {
                    callback: function() {
                        console.log(event);
                    }
                },
                2: {
                    callback: function() {
                        console.log(event);
                    }
                }
            }
        });

    }

    function hideControlPanel() {
        // Dismiss Control panel.
        var controlPanel = document.getElementById('robomgr-control-panel');
        var overlay = document.getElementById('robomgr-overlay');
        controlPanel.setAttribute('class', 'robomgr-hide');
        overlay.setAttribute('class', 'robomgr-hide');
        _controlPanelRobot.linkbot.unregister(false);
        _controlPanelRobot = null;
        _uiMenuSlide();
    }

    function _hexToRgb(hex) {
        var bigint;
        if (hex.substr(0, 1) === '#') {
            bigint = parseInt(hex.substring(1), 16);
        } else {
            bigint = parseInt(hex, 16);
        }
        return {
            'red':((bigint >> 16) & 255),
            'green':((bigint >> 8) & 255),
            'blue':(bigint & 255)
        };
    }

    function _rgbToHex(value) {
        if (!value || value === null || value === "undefined") {
            return "00";
        }
        var val = Math.round(value);
        val = val.toString(16);
        if (val.length < 2) {
            val = "0" + val;
        }
        return val;
    }

    function _colorToHex(color) {
        var red = _rgbToHex(color.red);
        var green = _rgbToHex(color.green);
        var blue = _rgbToHex(color.blue);
        return red + green + blue;
    }

    function findRobomgrId(element) {
        if (element) {
            var id = element.getAttribute('id');
            if (id && /^robomgr\-id\-/.test(id)) {
                return element;
            }
            return findRobomgrId(element.parentElement);
        }
        return null;
    }

    function dragStart(e) {
        var id = e.target.getAttribute('id');
        if (id && /^robomgr\-id\-/.test(id)) {
            e.dataTransfer.setData('text/html', e.target.innerHTML);
            e.dataTransfer.effectAllowed = 'move';
            source = e.target;
        }
    }

    function dragOver(e) {
        if (e.preventDefault) {
            e.preventDefault();
        }
        e.dataTransfer.dropEffect = 'move';
    }

    function drop(e) {
        if (e.stopPropagation) {
            e.stopPropagation();
        }
        if (source != e.target) {
            var destination = findRobomgrId(e.target);
            if (!destination || destination === null) {
                return true;
            }
            var srcId = source.getAttribute('id').replace(/robomgr-id-/, '');
            var destId = destination.getAttribute('id').replace(/robomgr-id-/, '');
            var olElement = destination.parentElement;
            if (destination == olElement.lastChild) {
                olElement.removeChild(source);
                olElement.insertBefore(source, null);
            } else if (destination == source.nextSibling) {
                olElement.removeChild(source);
                olElement.insertBefore(source, destination.nextSibling);
            } else {
                olElement.removeChild(source);
                olElement.insertBefore(source, destination);
            }
            // Moved Event Callback.
            var evt = events.moved;
            for (var i = 0; i < evt.length; i++) {
                evt[i](srcId, destId);
            }
            return false;
        }
        return true;
    }

    function _uiAdd(e) {
        var idInput;
        e.preventDefault();
        idInput = manager.element.querySelector('input#robotInput');
        manager.add(idInput.value);
        idInput.value = "";
        manager.connect();
        manager.redraw();
    }

    function _closeMenuSlide(e) {
        var spanBtn = manager.element.querySelector('span');
        var left = /robomgr-left/.test(spanBtn.className);
        if (!left) {
            return;
        }
        _uiMenuSlide(e);
    }

    function _uiMenuSlide(e) {
        var container, left, spanBtn, overlay, i, color;
        if (e && e.preventDefault) {
            e.preventDefault();
        }
        spanBtn = manager.element.querySelector('span');
        container = document.querySelector('#robomgr-container');
        left = /robomgr-left/.test(spanBtn.className);
        overlay = document.getElementById('robomgr-slideout-overlay');
        if (left) {
            var slideElements = document.getElementsByClassName('robomgr-slide-element');
            for (i = 0; i < slideElements.length; i++) {
                slideElements[i].className = 'robomgr-slide-element robomgr-slide-element-left';
            }
            spanBtn.className = 'robomgr-pulloutbtn robomgr-right';
            container.className = 'robomgr-container-hidden';
            document.body.style.marginLeft = '';
            overlay.style.display = 'none';
            //clearInterval(_robotStatusInterval);
            _robotStatusInterval = null;
        } else {
            spanBtn.className = 'robomgr-pulloutbtn robomgr-left';
            container.className = 'robomgr-container-open';
            document.body.style.marginLeft = '300px';
            overlay.style.display = 'block';
           // _checkRobotStatus();
            //_robotStatusInterval = setInterval(_checkRobotStatus, 3000);
        }
        return e;
    }

    function _uiSlideOut(e, r) {
        var slideElements, divElement, right, i;
        divElement = e.target;
        if (!/robomgr-slide-element/.test(divElement.className)) {
            return;
        }
        right = /robomgr-slide-element-right/.test(divElement.className);
        if (right) {
            divElement.className = 'robomgr-slide-element robomgr-slide-element-left';
            hideControlPanel();
        } else {
            slideElements = document.getElementsByClassName('robomgr-slide-element');
            for (i = 0; i < slideElements.length; i++) {
                slideElements[i].className = 'robomgr-slide-element robomgr-slide-element-left';
            }
            divElement.className = 'robomgr-slide-element robomgr-slide-element-right';
            showControlPanel(e, r);
        }
        e.stopPropagation();
    }

    function _uiRemoveFn(e, id) {
        e.preventDefault();
        manager.robots.remove(id);
        var evt = events.remove;
        for (var i = 0; i < evt.length; i++) {
            evt[i](id);
        }
        manager.redraw();
    }

    function _robotLi(doc, r) {
        var li = doc.createElement('li');
        var div = doc.createElement('div');
        var rm = doc.createElement('span');
        rm.setAttribute('class', "robomgr-remove-btn");
        rm.innerText = 'trash';
        var beep = doc.createElement('span');
        beep.setAttribute('class', 'robomgr-beep-btn');
        beep.innerText = 'beep';
        li.setAttribute('draggable', 'true');
        li.setAttribute('class', "robomgr--" + r.status);
        li.setAttribute('id', 'robomgr-id-' + r.id);
        if (r.linkbot) {
            li.style.background = "#" + _colorToHex(r.linkbot.getColor());
        } else {
            li.style.background = "#606060";
        }
        li.appendChild(beep);
        li.appendChild(rm);
        div.setAttribute('class', 'robomgr-slide-element robomgr-slide-element-left');
        var htmlVal = ['',
            '<span id="robot-id-' + r.id + '-name" class="robomgr-robot-name">Linkbot ' + r.id + '</span><br/>',
            '<span id="robot-id-' + r.id + '-status" class="robomgr-robot-status">' + ((r.status == 'failed') ? 'offline' : r.status)  + '</span>'
        ].join('');
        div.innerHTML = htmlVal;
        li.appendChild(div);
        // Add event Listeners.
        li.addEventListener('dragstart', dragStart);
        li.addEventListener('dragover', dragOver);
        li.addEventListener('drop', drop);
        div.addEventListener('click', function(e) {
            _uiSlideOut(e, r);
        }, true);
        rm.addEventListener('click', function(e) {
            _uiRemoveFn(e, r.id);
        });
        beep.addEventListener('click', function(e) {
            if (r.status !== 'failed' ) {
                r.linkbot.buzzerFrequency(500);
                setTimeout(function() { r.linkbot.buzzerFrequency(0); }, 250);
            }
        });
        return li;
    }

    function _constructTopNav(doc) {
        var el = doc.createElement('div');
        el.setAttribute('id', 'robomgr-top-navigation');
        var htmlVal = ['',
            '<h1 class="robomgr-logo"><a href="/index.html">Linkbot Labs</a></h1>',
            '<div class="robomgr-top-nav-info">',
            ' <p id="ljs-top-nav-breadcrumbs" class="robomgr-top-nav-breadcrumbs">&nbsp;</p>',
            ' <h1 id="ljs-top-nav-title" class="robomgr-top-nav-title">&nbsp;</h1>',
            '</div>'
        ].join('');
        el.innerHTML = htmlVal;
        return el;
    }

    function _constructElement(doc) {
        var addBtn, el, controlPanel, overlay, pulloutBtn, slideOverlay, btn;
        el = doc.createElement('div');
        slideOverlay = doc.createElement('div');
        slideOverlay.setAttribute('id', 'robomgr-slideout-overlay');
        overlay = doc.createElement('div');
        overlay.setAttribute('id', 'robomgr-overlay');
        overlay.setAttribute('class', 'robomgr-hide');
        el.setAttribute('class', 'robomgr-container-hidden');
        el.setAttribute('id', 'robomgr-container');
        var htmlVal = ['',
            '<div class="robomgr-pullout">',
            '  <span class="robomgr-pulloutbtn robomgr-right"></span>',
            '</div>',
            '<div id="robomgr-container-content">',
            '  <form>',
            '    <div id="robotFormContainer">',
            '      <label for="robotInput" id="robotInputLabel" class="sr-only">Linkbot ID</label>',
            '      <input name="robotInput" id="robotInput" type="text" placeholder="Linkbot ID" />',
            '      <button id="robomgr-add">Add</button>',
            '    </div>',
            '  </form><ol id="robomgr-robot-list"></ol>',
            '</div>'
        ].join('');
        el.innerHTML = htmlVal;
        addBtn = el.querySelector('button');
        pulloutBtn = el.querySelector('.robomgr-pullout');
        addBtn.addEventListener('click', _uiAdd);
        pulloutBtn.addEventListener('click', _uiMenuSlide);

        slideOverlay.addEventListener('click', _closeMenuSlide);

        controlPanel = doc.createElement('div');
        controlPanel.setAttribute('class', 'robomgr-hide');
        controlPanel.setAttribute('id', 'robomgr-control-panel');
        var controlPanelHtml = ['',
            '<div class="robomgr-control-nav">',
            ' <div class="robomgr-control-img">close</div>',
            ' <div class="robomgr-control-title">',
            '   <h1 id="robomgr-control-panel-linkbot">Linkbot</h1>',
            '   <span>control panel</span>',
            ' </div>',
            '</div>',
            '<div>',
            ' <ul class="robomgr-tabs">',
            '   <li class="robomgr-active"><a id="robomgr-tab-control">control</a></li>',
            '   <li><a id="robomgr-tab-sensors">sensors</a></li>',
            ' </ul>',
            '</div>',
            '<div id="robomgr-tab-control-panel">',
            ' <div class="robomgr-row">',
            '   <div class="robomgr-control-col">position',
            '     <div class="robomgr-control-poster" style="padding: 10px;">',
            '       <div style="float: left;">',
            '         <input type="text" class="linkbotjs-knob" id="position-joint-1" /> <p style="margin:0;">Joint 1</p>',
            '       </div>',
            '       <div style="margin-left: 125px; width: 125px;">',
            '         <input type="text" class="linkbotjs-knob" id="position-joint-2" /> <p style="margin:0;">Joint 2</p>',
            '       </div>',
            '     </div>',
            '   </div>',
            '   <div class="robomgr-control-col">',
            '     joint control',
            '     <div class="robomgr-control-poster">',
            '       <div style="display: inline-table;">',
            '         <button id="robomgr-joint1-up" class="robomgr-btn-up joint-control-btn">joint 1 up</button>',
            '         <button id="robomgr-joint1-stop" class="robomgr-btn-stop joint-control-btn">joint 1 stop</button>',
            '         <button id="robomgr-joint1-down" class="robomgr-btn-down joint-control-btn">joint 1 down</button>',
            '         joint1',
            '       </div>',
            '       <div style="display: inline-table;">',
            '         <button id="robomgr-joint2-up" class="robomgr-btn-up joint-control-btn">joint 2 up</button>',
            '         <button id="robomgr-joint2-stop" class="robomgr-btn-stop joint-control-btn">joint 2 stop</button>',
            '         <button id="robomgr-joint2-down" class="robomgr-btn-down joint-control-btn">joint 2 down</button>',
            '         joint2',
            '       </div>',
            '     </div>',
            '   </div>',
            ' </div>',
            ' <div class="robomgr-row">',
            '   <div class="robomgr-control-col">',
            '     drive control',
            '     <div class="robomgr-control-poster">',
            '       <div><button id="robomgr-drive-up" class="drive-control-btn-sm robomgr-btn-up">up</button></div>',
            '       <div>',
            '         <button id="robomgr-drive-left" class="drive-control-btn-sm robomgr-btn-left">left</button>',
            '         <button id="robomgr-drive-down" class="drive-control-btn-sm robomgr-btn-down">down</button>',
            '         <button id="robomgr-drive-right" class="drive-control-btn-sm robomgr-btn-right">right</button>',
            '       </div>',
            '       <div><button id="robomgr-drive-zero" class="drive-control-btn-lg robomgr-btn-zero">zero</button></div>',
            '       <div><button id="robomgr-drive-stop" class="drive-control-btn-lg robomgr-btn-stop">stop</button></div>',
            '     </div>',
            '   </div>',
            '   <div class="robomgr-control-col">speed',
            '     <div class="robomgr-control-poster" style="padding: 10px;">',
            '       <div style="float: left; width: 110px;">',
            '         <div id="speed-joint-1" class="linkbotjs-slider" data-min="10" data-max="200"></div> <p style="padding-top: 10px;">Joint 1: <span id="speed-joint-1-value">10</span></p>',
            '       </div>',
            '       <div style="margin-left: 132px; width: 110px;">',
            '         <div id="speed-joint-2" class="linkbotjs-slider" data-min="10" data-max="200"></div> <p style="padding-top: 10px;">Joint 2: <span id="speed-joint-2-value">10</span></p>',
            '       </div>',
            '     </div>',
            '     acceleration',
            '     <div class="robomgr-control-poster" style="padding: 10px;">',
            '       <div style="float: left; width: 110px;">',
            '         <div id="acceleration-joint-1" class="linkbotjs-slider" data-min="10" data-max="200"></div> <p style="padding-top: 10px;">Joint 1: <span id="acceleration-joint-1-value">10</span></p>',
            '       </div>',
            '       <div style="margin-left: 132px; width: 110px;">',
            '         <div id="acceleration-joint-2" class="linkbotjs-slider" data-min="10" data-max="200"></div> <p style="padding-top: 10px;">Joint 2: <span id="acceleration-joint-2-value">10</span></p>',
            '       </div>',
            '     </div>',
            '   </div>',
            ' </div>',
            ' <div class="robomgr-row">',
            '   <div class="robomgr-control-col" style="visibility: hidden;"></div>',
            ' </div>',
            '</div>',
            '<div id="robomgr-tab-sensors-panel" class="robomgr-hide">',
            ' <div class="robomgr-row" style="text-align: center;">buzzer control',
            '     <div class="robomgr-control-poster" style="padding: 10px 30px;">',
            '     <div>',
            '       <div style="float: left; width: 300px;">',
            '         <span>buzzer frequency (hz):</span> <span id="buzzer-frequency-id-value">440</span>',
            '         <div id="buzzer-frequency-id" class="linkbotjs-slider" data-min="130" data-max="1000"></div>',
            '       </div>',
            '       <div style="width: 100px; margin-left: 305px;">',
            '         <span id="buzzer-frequency-button" class="robomgr-beep-btn">beep</span>',
            '       </div>',
            '     </div>',
            '   </div>',
            ' </div>',
            ' <div class="robomgr-row" style="text-align: center;">accelerometer',
            '   <div class="robomgr-control-poster" style="height: 275px; padding: 20px;">',
            '     <div style="float: left; width: 140px; height: 100%;"><div id="accel-xaxis" style="height: 90%; margin: 0 auto;" class="linkbotjs-vslider" data-type="float" data-min="-5" data-max="5"></div><p style="padding-top: 10px;">x axis: <span id="accel-xaxis-value">0</span></p></div>',
            '     <div style="float: left; width: 140px; height: 100%;"><div id="accel-yaxis" style="height: 90%; margin: 0 auto;" class="linkbotjs-vslider" data-type="float" data-min="-5" data-max="5"></div><p style="padding-top: 10px;">y axis: <span id="accel-yaxis-value">0</span></p></div>',
            '     <div style="float: left; width: 140px; height: 100%;"><div id="accel-zaxis" style="height: 90%; margin: 0 auto;" class="linkbotjs-vslider" data-type="float" data-min="-5" data-max="5"></div><p style="padding-top: 10px;">z axis: <span id="accel-zaxis-value">0</span></p></div>',
            '     <div style="width: 130px; margin-left: 420px; height: 100%;"><div id="accel-mag" style="height: 90%; margin: 0 auto;" class="linkbotjs-vslider" data-type="float" data-min="0" data-max="5"></div><p style="padding-top: 10px;">mag: <span id="accel-mag-value">0</span></p></div>',
            '   </div>',
            ' </div>',
            ' <div class="robomgr-row" style="text-align: center;">LED Color',
            '   <div class="robomgr-control-poster" style="padding: 10px 30px;">',
            '     <input type="color" id="linkbotjs-led-color" />',
            '   </div>',
            ' </div>',
            '</div>'
        ].join('');
        controlPanel.innerHTML = controlPanelHtml;
        // Order matters.
        el.appendChild(slideOverlay);
        el.appendChild(overlay);
        el.appendChild(controlPanel);
        overlay.addEventListener('click', hideControlPanel);
        controlPanel.addEventListener('click', function(e) {
            e.stopPropagation();
        });
        var ledColorElement = controlPanel.querySelector('#linkbotjs-led-color');
        ledColorElement.addEventListener('input', function(e) {
            var value = _hexToRgb(ledColorElement.value);
            _controlPanelRobot.linkbot.color(value.red, value.green, value.blue);
        });

        var imgElements = controlPanel.getElementsByClassName('robomgr-control-img');
        imgElements[0].addEventListener('click', function(e) {
            hideControlPanel();
        });
        // Enable controls.
        var i;
        var knobElements = controlPanel.getElementsByClassName('linkbotjs-knob');
        for (i = 0; i < knobElements.length; i++) {
            LinkbotControls.knob.add(knobElements[i]);
        }
        var sliderElements = controlPanel.getElementsByClassName('linkbotjs-slider');
        for (i = 0; i < sliderElements.length; i++) {
            LinkbotControls.slider.add(sliderElements[i]);
        }
        var vsliderElements = controlPanel.getElementsByClassName('linkbotjs-vslider');
        for (i = 0; i < vsliderElements.length; i++) {
            LinkbotControls.slider.add(vsliderElements[i]);
        }
        //Add event handling:
        LinkbotControls.slider.addChangeCallback('speed-joint-1', function(value) {
            document.getElementById('speed-joint-1-value').innerText = value;
            controlSpeedChanged(value);
        });
        LinkbotControls.slider.addChangeCallback('speed-joint-2', function(value) {
            document.getElementById('speed-joint-2-value').innerText = value;
            controlSpeedChanged(value);
        });
        LinkbotControls.slider.addChangeCallback('acceleration-joint-1', function(value) {
            document.getElementById('acceleration-joint-1-value').innerText = value;
        });
        LinkbotControls.slider.addChangeCallback('acceleration-joint-2', function(value) {
            document.getElementById('acceleration-joint-2-value').innerText = value;
        });
        LinkbotControls.slider.addChangeCallback('buzzer-frequency-id', function(value) {
            document.getElementById('buzzer-frequency-id-value').innerText = value;
        });
        LinkbotControls.slider.addChangeCallback('accel-xaxis', function(value) {
            document.getElementById('accel-xaxis-value').innerText = Math.round(value * 10000) / 10000;
        });
        LinkbotControls.slider.addChangeCallback('accel-yaxis', function(value) {
            document.getElementById('accel-yaxis-value').innerText = Math.round(value * 10000) / 10000;
        });
        LinkbotControls.slider.addChangeCallback('accel-zaxis', function(value) {
            document.getElementById('accel-zaxis-value').innerText = Math.round(value * 10000) / 10000;
        });
        LinkbotControls.slider.addChangeCallback('accel-mag', function(value) {
            document.getElementById('accel-mag-value').innerText = Math.round(value * 10000) / 10000;
        });
        LinkbotControls.knob.addChangeCallback('position-joint-1', controlKnobChanged);
        LinkbotControls.knob.addChangeCallback('position-joint-2', controlKnobChanged);
        var tabs = controlPanel.getElementsByTagName('a');
        /* jshint ignore:start */
        for (i in tabs) {
            if (tabs[i].id == 'robomgr-tab-control') {
                tabs[i].addEventListener('click', function(e) {
                    document.getElementById('robomgr-tab-control-panel').className = '';
                    document.getElementById('robomgr-tab-sensors-panel').className = 'robomgr-hide';
                    document.getElementById('robomgr-tab-control').parentElement.className='robomgr-active';
                    document.getElementById('robomgr-tab-sensors').parentElement.className='';
                    e.stopPropagation();
                });
            } else if (tabs[i].id == 'robomgr-tab-sensors') {
                tabs[i].addEventListener('click', function(e) {
                    document.getElementById('robomgr-tab-control-panel').className = 'robomgr-hide';
                    document.getElementById('robomgr-tab-sensors-panel').className = '';
                    document.getElementById('robomgr-tab-control').parentElement.className='';
                    document.getElementById('robomgr-tab-sensors').parentElement.className='robomgr-active';
                    e.stopPropagation();
                });
            }
        }
        var beepBtns = controlPanel.getElementsByClassName('robomgr-beep-btn');
        i = 0;
        for (i in beepBtns) {
            var beepBtn = beepBtns[i];
            if (beepBtn && beepBtn.addEventListener) {
                beepBtn.addEventListener('onmousedown', controlBeepDown);
                beepBtn.addEventListener('onmouseup', controlBeepUp);
                beepBtn.addEventListener('click', controlBeepClicked);
            }
        }
        var buttons = controlPanel.getElementsByTagName('button');
        for (i in buttons) {
            btn = buttons[i];
            if (btn && btn.id) {
                if (btn.id === 'robomgr-drive-stop') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.stop();
                    });
                } else if (btn.id == 'robomgr-joint1-stop') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveJointContinuous(0, 0);
                    });
                } else if (btn.id == 'robomgr-joint2-stop') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveJointContinuous(2, 0);
                    });
                } else if (btn.id == 'robomgr-drive-zero') {
                    btn.addEventListener('click', controlZeroPressed);
                }
            }
        }
        var ctrl_buttons = controlPanel.getElementsByTagName('button');
        for (i = 0; i < ctrl_buttons.length; i++) {
            btn = ctrl_buttons[i];
            if (btn.id) {
                if (btn.id === 'robomgr-drive-up') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveForward();
                    });
                } else if (btn.id === 'robomgr-drive-down') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveBackward();
                    });
                } else if (btn.id === 'robomgr-drive-left') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveLeft();
                    });
                } else if (btn.id === 'robomgr-drive-right') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveRight();
                    });
                } else if (btn.id === 'robomgr-joint1-up') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveJointContinuous(0, 1);
                    });
                } else if (btn.id === 'robomgr-joint1-down') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveJointContinuous(0, -1);
                    });
                } else if (btn.id === 'robomgr-joint2-up') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveJointContinuous(2, -1);
                    });
                } else if (btn.id === 'robomgr-joint2-down') {
                    btn.addEventListener('click', function() {
                        _controlPanelRobot.linkbot.moveJointContinuous(2, 1);
                    });
                }
            }
        }
        /* jshint ignore:end */
        return el;
    }

    // Public
    this.robots = new RobotStatus();
    this.element = _constructElement(document);
    this.topNav = _constructTopNav(document);

    this.acquire = function(n) {
        var x = manager.robots.acquire(n);
        manager.redraw();
        return x;
    };

    this.relinquish = function(l) {
        l.disconnect();
        manager.robots.relinquish(l);
        manager.redraw();
    };

    this.add = function() {
        var ids = (1 <= arguments.length) ? [].slice.call(arguments, 0) : [];
        var evt = events.add;
        ids.map(function(i) {
            manager.robots.add(i);
            for (var j = 0; j < evt.length; j++) {
                evt[j](i);
            }
        });
    };

    this.redraw = function() {
        var doc = manager.element.ownerDocument;
        var ol = doc.createElement('ol');
        ol.setAttribute('id', 'robomgr-robot-list');
        var robotList = manager.robots.list();
        for (var i = 0; i < robotList.length; i++) {
            var r = robotList[i];
            ol.appendChild(_robotLi(doc, r));
        }
        var robotListElement = manager.element.querySelector('ol#robomgr-robot-list');
        robotListElement.parentElement.replaceChild(ol, robotListElement);
    };

    this.connect = function() {
        var robotList = manager.robots.list();
        var results = [];
        for (var i = 0; i < robotList.length; i++) {
            var r = robotList[i];
            if (r.status === "new") {
                bot = new Linkbot(r.id);
                if (bot._id !== null) {
                    results.push(manager.robots.ready(i, bot));
                    bot.register({
                      led: {
                        callback: _ledChanged
                      }
                    });
                } else {
                    results.push(manager.robots.fail(i));
                }
            } else {
                results.push(void 0);
            }
        }
        return results;
    };

    this.registerEvent = function(type, func) {
        var evt = events[type];
        if (evt) {
            evt.push(func);
        }
    };

    this.unregisterEvent = function(type, func) {
        var evt = events[type];
        if (evt) {
            evt.pop(func);
        }
    };

    this.selectedControlPanelRobot = function() {
      return _controlPanelRobot;
    };
}
;function Linkbot(_id) {
  // Private
  var bot = this;
  var ledCallbacks = [];
  var wheelSlotCallback = null;
  var buttonSlotCallback = null;
  var accelSlotCallback = null;
  var joinDirection = [0, 0, 0];

  bot._id = _id;
  err = baroboBridge.connectRobot(_id);
  if (err < 0) {
    bot._id = null;
    return;
  }
  for (var m = 1; m <= 3; m++) {
    baroboBridge.setMotorEventThreshold(bot._id, m, 1e10);
  }
  bot._wheelPositions = baroboBridge.getMotorAngles(bot._id);
  bot._firmwareVersion = baroboBridge.firmwareVersion(bot._id);
  if (!baroboBridge.mock) {
    var blessedFW = baroboBridge.availableFirmwareVersions();
    if (blessedFW.indexOf(bot._firmwareVersion) < 0) {
      idAsURI = encodeURIComponent(bot._id);
      baroboBridge.stop(bot._id);
      document.location = "../LinkbotUpdate/index.html?badRobot=" + idAsURI;
    }
  }

  function accelSlot(robot, callback, model) {
    if (model === null) {
      model = {};
    }
    return function(robotID, x, y, z) {
      if (robotID === robot._id) {
        return callback(robot, model, {
          x: x,
          y: y,
          z: z
        });
      }
    };
  }

  function buttonSlot(robot, buttonId, callback, model) {
    if (model === null) {
      model = {};
    }
    return function(robID, btnID, press) {
      if (press === 1 && robot._id === robID && buttonId === btnID) {
        return callback(robot, model, {
          button: btnID
        });
      }
    };
  }

  function wheelSlot(robot, wheelId, callback, model) {
    if (model === null) {
      model = {};
    }
    return function(robID, _wheelId, angle) {
      var diff;
      if (robot._id === robID && wheelId === _wheelId) {
        diff = angle - robot._wheelPositions[wheelId - 1];
        robot._wheelPositions[wheelId - 1] = angle;
        return callback(robot, model, {
          triggerWheel: wheelId,
          position: angle,
          difference: diff
        });
      }
    };
  }

  // Public

  this._wheelRadius = 1.75;

  this.color = function(r, g, b) {
    var i = 0;
    baroboBridge.setLEDColor(bot._id, r, g, b);
    for (i = 0; i < ledCallbacks.length; i++) {
      ledCallbacks[i](bot._id, r, g, b);
    }
  };

  this.getColor = function() {
    var color = null;
    if (baroboBridge.getLEDColor) {
      color = baroboBridge.getLEDColor(bot._id);
    }
    if (!color || color === null) {
      color = {red:96, green:96, blue:96, mock:true};
    }
    color.id = bot._id;
    return color;
  };

  this.angularSpeed = function(s1, s2, s3) {
      if (s2 === null) {
        s2 = s1;
      }
      if (s3 === null) {
        s3 = s1;
      }
      return baroboBridge.angularSpeed(bot._id, s1, s2, s3);
  };

  this.move = function(r1, r2, r3) {
    return baroboBridge.move(bot._id, r1, r2, r3);
  };

  this.moveTo = function(r1, r2, r3) {
    return baroboBridge.moveTo(bot._id, r1, r2, r3);
  };

  this.moveForward = function() {
    joinDirection[0] = 1;
    joinDirection[2] = -1;
    baroboBridge.moveContinuous(bot._id, joinDirection[0], joinDirection[1], joinDirection[2]);
  };
  this.moveBackward = function() {
    joinDirection[0] = -1;
    joinDirection[2] = 1;
    baroboBridge.moveContinuous(bot._id, joinDirection[0], joinDirection[1], joinDirection[2]);
  };
  this.moveLeft = function() {
    joinDirection[0] = -1;
    joinDirection[2] = -1;
    baroboBridge.moveContinuous(bot._id, joinDirection[0], joinDirection[1], joinDirection[2]);
  };
  this.moveRight = function() {
    joinDirection[0] = 1;
    joinDirection[2] = 1;
    baroboBridge.moveContinuous(bot._id, joinDirection[0], joinDirection[1], joinDirection[2]);
  };
  this.moveJointContinuous = function(joint, direction) {
    if (joint >= 0 && joint <= 2) {
      if (direction > 0) {
        joinDirection[joint] = 1;
      } else if (direction < 0) {
        joinDirection[joint] = -1;
      } else {
        joinDirection[joint] = 0;
      }
      baroboBridge.moveContinuous(bot._id, joinDirection[0], joinDirection[1], joinDirection[2]);
      return true;
    }
    return false;
  };
  this.wheelPositions = function() {
    bot._wheelPositions = baroboBridge.getMotorAngles(bot._id);
    return bot._wheelPositions;
  };

  this.stop = function() {
    joinDirection[0] = 0;
    joinDirection[2] = 0;
    return baroboBridge.stop(bot._id);
  };

  this.buzzerFrequency = function(freq) {
    return baroboBridge.buzzerFrequency(bot._id, freq);
  };

  this.disconnect = function() {
    bot.stop();
    bot._id = null;
    return bot._id; 
  };

  this.register = function(connections) {
    var buttonId, registerObject, slot, wheelId, _ref, _results, _wheelId;
    if (connections.button && connections.button !== null) {
      _ref = connections.button;
      if (buttonSlotCallback === null) {
        buttonSlotCallback = [];
      }
      for (buttonId in _ref) {
        //if (!__hasProp.call(_ref, buttonId)) continue;
        registerObject = _ref[buttonId];
        slot = buttonSlot(bot, parseInt(buttonId), registerObject.callback, registerObject.data);
        baroboBridge.buttonChanged.connect(slot);
        buttonSlotCallback.push(slot);
        baroboBridge.enableButtonSignals(bot._id);
      }
    }
    if (connections.wheel && connections.wheel !== null) {
      _ref = connections.wheel;
      _results = [];
      if (wheelSlotCallback === null) {
        wheelSlotCallback = [];
      }
      for (_wheelId in _ref) {
        // if (!__hasProp.call(_ref, _wheelId)) continue;
        registerObject = _ref[_wheelId];
        wheelId = parseInt(_wheelId);
        slot = wheelSlot(bot, wheelId, registerObject.callback, registerObject.data);
        baroboBridge.setMotorEventThreshold(bot._id, wheelId, registerObject.distance);
        baroboBridge.motorChanged.connect(slot);
        wheelSlotCallback.push(slot);
        _results.push(baroboBridge.enableMotorSignals(bot._id));
      }
    }
    if (connections.accel && connections.accel !== null) {
      _ref = connections.accel;
      accelSlotCallback = accelSlot(bot, _ref.callback, _ref.data);
      baroboBridge.accelChanged.connect(accelSlotCallback);
      baroboBridge.enableAccelSignals(bot._id);
    }
    if (connections.led && connections.led !== null) {
      _ref = connections.led;
      ledCallbacks.push(_ref.callback);
    }
    return _results;
  };

  this.unregister = function(includeLed) {
    try {
      if (wheelSlotCallback && wheelSlotCallback !== null) {
        baroboBridge.disableMotorSignals(bot._id);
        for (var a in wheelSlotCallback) {
          baroboBridge.motorChanged.disconnect(wheelSlotCallback[a]);
        }
        wheelSlotCallback = null;
      }
    } catch (err) {
      console.log(err);
    }
    try {
      if (buttonSlotCallback && buttonSlotCallback !== null) {
        baroboBridge.disableButtonSignals(bot._id);
        for (var b in buttonSlotCallback) {
          baroboBridge.buttonChanged.disconnect(buttonSlotCallback[b]);
        }
        buttonSlotCallback = null;
      }
      
    } catch (err) {
      console.log(err);
    }
    try {
      if (accelSlotCallback !== null) {
        baroboBridge.disableAccelSignals(bot._id);
        baroboBridge.accelChanged.disconnect(accelSlotCallback);
      }
    } catch (err) {
      console.log(err);
    }
    if (includeLed === undefined || includeLed === null) {
      ledCallbacks = [];
    } else if (includeLed) {
      ledCallbacks = [];
    }
  };
}
;function Storage(config) {
  var settings = (config) ? config : {
    DBNAME: "robotsdb",
    DBVER: 1.0,
    DBDESC: "Robots Database, used for persistance",
    TABLE: "robots"
  };
  var db = null;
  try {
    db = openDatabase(settings.DBNAME, settings.DBVER, settings.DBDESC, settings.DBSIZE);
  } catch(e) {
    // TODO implement alternative storage engines.
    this.remove = function(name, callback) {
      console.log('OpenDatabase not available');
    };
    this.add = function(name, status, callback) {
      console.log('OpenDatabase not available');
    };
    this.getAll = function(callback) {
      console.log('OpenDatabase not available');
    };
    this.updateOrder = function(callback) {
      console.log('OpenDatabase not available');
    };
    this.printRows = function() {
      console.log('OpenDatabase not available');
    };
    this.changePosition = function(currentPosition, newPosition, callback) {
      console.log('OpenDatabase not available');
    };
    return;
  }
  db.transaction(function(tx) {
    tx.executeSql("CREATE TABLE IF NOT EXISTS " + settings.TABLE + " (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT unique not null, status INTEGER not null, rorder INTEGER not null)",
      [],
      function(tx, results) {
      },
      function(tx, error) {
        console.log(error);
      }
    );
  });
  this.remove = function(name, callback) {
    db.transaction(function(tx) {
      tx.executeSql("DELETE FROM " + settings.TABLE + " WHERE name = ?", [name],
        function(tx, result) {
          if (callback) {
            callback(true);
          }
        },
        function(tx, error) {
          if (callback) {
            callback(false, error);
          }
        });
    });
  };
  this.add = function(name, status, callback) {
    db.transaction(function(tx) {
      tx.executeSql("SELECT COUNT(*) AS c FROM " + settings.TABLE, [],
      function(tx, countResult) {
        tx.executeSql("INSERT INTO " + settings.TABLE + " (name, status, rorder) values (?,?,?)", [name, status, countResult.rows.item(0).c],
          function(tx, insertResult) {
            if (callback) {
              callback(true);
            }
          },
          function(tx, insertError) {
            if (callback) {
              callback(false, insertError);
            }
          });
      },
      function(tx, error) {
        if (callback) {
          callback(false, error);
        }
      });
    });
  };
  this.getAll = function(callback) {
    db.transaction(function(tx) {
      tx.executeSql("SELECT * FROM " + settings.TABLE + " ORDER BY rorder", [],
        function(tx, result) {
          var rows = result.rows;
          var allRobots = [];
          for (var i = 0; i < result.rows.length; i++) {
            allRobots.push({ "id":rows.item(i).id, "name": rows.item(i).name, "status":rows.item(i).status, "order":rows.item(i).rorder});
          }
          if (callback) {
            callback(allRobots);
          }
        },
        function(tx, error) {
          if (callback) {
            callback([], error);
          }
        });
    });
  };
  this.updateOrder = function(callback) {
    db.transaction(function(tx) {
      tx.executeSql("SELECT id FROM " + settings.TABLE + " ORDER BY rorder", [],
          function(tx, result) {
              var success = function(tx, results) {}; // do nothing
              var error = function(tx, error) {
                  if (callback) {
                      callback(false, results);
                  }
              };
              for (var i = 0; i < result.rows.length; i++) {
                  tx.executeSql("UPDATE " + settings.TABLE + " SET rorder = ? WHERE id = ?", [i, result.rows.item(i).id],
                      success,
                      error);
              }
              if (callback) {
                  callback(true);
              }
          },
          function(tx, error) {
              if (callback) {
                  callback(false, results);
              }
          });
    });
  };

  // For debugging.
  this.printRows = function() {
    db.transaction(function(tx) {
      tx.executeSql("SELECT * FROM " + settings.TABLE + " ORDER BY rorder", [],
        function(tx, result) {
          var rows = result.rows;
          for (var i = 0; i < result.rows.length; i++) {
            console.log('ID:' + rows.item(i).id + ', NAME:' + rows.item(i).name + ', STATUS:' + rows.item(i).status + ', ORDER:' + rows.item(i).rorder);
          }
        },
        function(tx, error) {
          console.log(error);
        });
    });
  };
  this.changePosition = function(currentPosition, newPosition, callback) {
    if (currentPosition === newPosition) {
      return;
    }
    db.transaction(function(tx) {
      var start = currentPosition;
      var end = newPosition;
      if (currentPosition > newPosition) {
        start = newPosition;
        end = currentPosition;
      }
      tx.executeSql("SELECT * from " + settings.TABLE + " WHERE rorder BETWEEN ? and ? ORDER BY rorder", [start, end],
        function(tx, result) {
          var i = 0;
          var _modifier = -1;
          var rows = result.rows;
          if (currentPosition > newPosition) {
            _modifier = 1;
          }
          var success = function(tx, results) {
            if (callback) {
              callback(true, results);
            }
          };
          var error = function(tx, error) {
            if (callback) {
              callback(false, results);
            }
          };
          for (i = 0; i < result.rows.length; i++) {
            var setorder = rows.item(i).rorder + _modifier;
            var setid = rows.item(i).id;
            if (rows.item(i).rorder == currentPosition) {
              setorder = newPosition;
            }
            tx.executeSql("UPDATE " + settings.TABLE + " SET rorder = ? WHERE id = ?", [setorder, setid],
              success,
              error);
          }
          if (callback) {
            callback(true);
          }
        },
        function(tx, error) {
          if (callback) {
            callback(false,error);
          }
        });
    });
  };
}
;var LinkbotControls = (function(parent, doc) {
	"use strict";
	var me = parent.knob = parent.knob || {};
	var knobs = [];
	var knobsMap = {};

	function mouseUp(e) {
		for (var i in knobs) {
			var knob = knobs[i];
			knob.setDown(false);
		}
	}
	function mouseMove(e) {
		var clickX = e.clientX || e.pageX;
		var clickY = e.clientY || e.pageY;
		for (var i in knobs) {
			var knob = knobs[i];
			if (knob.isDown()) {
				knob.updateXY(clickX, clickY);
			}
		}
	}

	function KnobControl(element) {
			var inputElement = element;
			var parent = element.parentElement;
			var wrapper = document.createElement('div');
			var imgElement = document.createElement('img');
			var down = false;
			var rad2deg = 180/Math.PI;
			var inputValue = 0;
			var internalValue = 0;
			var changeRegister = [];

			wrapper.setAttribute('class', 'linkbotjs-knob-container');
			parent.replaceChild(wrapper, inputElement);
			inputElement.setAttribute('value', inputValue);
			imgElement.setAttribute('width', '100%');
			imgElement.draggable = false;
			wrapper.appendChild(imgElement);
			wrapper.appendChild(inputElement);
			

			function getPosition(element) {
			    var xPosition = 0;
			    var yPosition = 0;
			  
			    while(element) {
			        xPosition += (element.offsetLeft - element.scrollLeft + element.clientLeft);
			        yPosition += (element.offsetTop - element.scrollTop + element.clientTop);
			        element = element.offsetParent;
			    }
			    return { x: xPosition, y: yPosition };
			}

			
			
			return {
				getKnob: function() {
					return wrapper;
				},
				getKnobInput: function() {
					return inputElement;
				},
				getKnobImage: function() {
					return imgElement;
				},
				setDown: function(value) {
					down = value;
				},
				isDown: function() {
					return down;
				},
				getPosition: function() {
					return getPosition(wrapper);
				},
				getCenter: function() {
					var position = getPosition(wrapper);
					var box = [position.x, position.y, wrapper.offsetWidth, wrapper.offsetHeight];
					var center = { x:(box[0] + (box[2] / 2)),
						   y:(box[1] + (box[3] / 2))};
					return center;
				},
				updateXY: function(x, y) {
					var ydiff, xdiff, deg, position, box, center, i, originalDeg, pos, neg;
					originalDeg = inputValue;
					position = getPosition(wrapper);
					box = [position.x, position.y, wrapper.offsetWidth, wrapper.offsetHeight];
					center = { x:(box[0] + (box[2] / 2)),
						   y:(box[1] + (box[3] / 2))};
					xdiff = center.x - x;
					ydiff = center.y - y;
					deg = ((Math.atan2(ydiff,xdiff) * rad2deg) + 270) % 360;
					deg = Math.round(deg);

					if (originalDeg >= deg) {
						neg = originalDeg - deg;
						pos = 360 - originalDeg + deg;

					} else {
						pos = deg - originalDeg;
						neg = originalDeg + 360 - deg;
					}
					if (pos <= neg) {
						internalValue += pos;
					} else {
						internalValue -= neg;
					}
					imgElement.style.transform = "rotate(" + deg + "deg)";
					imgElement.style.webkitTransform  = "rotate(" + deg + "deg)";
					inputElement.value = deg;
					inputValue = deg;
					for (i = 0; i < changeRegister.length; i++) {
						changeRegister[i](deg);
					}
				},
				setValue: function(value) {
					var intValue, i;
					intValue = parseInt(value);
					if (isNaN(intValue)) {
						inputElement.value = inputValue;
						return;
					}
					internalValue = intValue;
					intValue = intValue % 360;
					while (intValue < 0) {
						intValue = 360 + intValue; 
					}
					imgElement.style.transform = "rotate(" + intValue + "deg)";
					imgElement.style.webkitTransform  = "rotate(" + intValue + "deg)";
					inputValue = intValue;
					inputElement.value = inputValue;
					for (i = 0; i < changeRegister.length; i++) {
						changeRegister[i](intValue);
					}
				},
				setValueWithoutChange: function(value) {
					var intValue, i;
					intValue = parseInt(value);
					if (isNaN(intValue)) {
						inputElement.value = inputValue;
						return;
					}
					internalValue = intValue;
					intValue = intValue % 360;
					while (intValue < 0) {
						intValue = 360 + intValue; 
					}
					imgElement.style.transform = "rotate(" + intValue + "deg)";
					imgElement.style.webkitTransform  = "rotate(" + intValue + "deg)";
					inputValue = intValue;
					inputElement.value = inputValue;
				},
				getValue: function() {
					return inputValue;
				},
				getInternalValue: function() {
					return internalValue;
				},
				addChangeCallback: function(callback) {
					changeRegister.push(callback);
				}
			};
	}
	me.add = function(param) {
		var element = null;
		if (param instanceof HTMLElement) {
			element = param;
		} else {
			element = doc.getElementById(param);
		}
		if (!element || element === null) {
			return;
		}
		var knobControl = new KnobControl(element);
		knobControl.getKnob().addEventListener('mousedown', function(e) {
			if (e.button === 0) {
				knobControl.setDown(true);
			}
		});
		knobControl.getKnob().addEventListener('click', function(e) {
			var clickX = e.clientX || e.pageX;
			var clickY = e.clientY || e.pageY;
			knobControl.updateXY(clickX, clickY);
		});
		knobControl.getKnobInput().addEventListener('click', function(e) { e.stopPropagation(); }, true);
		knobControl.getKnobInput().addEventListener('onchange', function(e) {
		 	knobControl.setValue(e.target.value);
		});
		knobControl.getKnobInput().addEventListener('keyup', function(e) {
			knobControl.setValue(e.target.value);
		});
		knobControl.getKnobInput().addEventListener('mousedown', function(e) { e.stopPropagation(); }, true);
		if (element.id) {
			knobsMap[element.id] = knobControl;
		}
		knobs.push(knobControl);
	};
	me.init = function() {
		doc.addEventListener('mouseup', mouseUp);
		doc.addEventListener('mousemove', mouseMove);
		var elements = document.getElementsByClassName('linkbotjs-knob');
		for (var i = 0; i < elements.length; i++) {
			me.add(elements[i]);
		}
	};
	me.get = function(id) {
		return knobsMap[id];
		
	};
	me.getValue = function(id) {
		var knob = knobsMap[id];
		if (knob) {
			return knob.getValue();
		}
		
		return null;
	};
	me.getInternalValue = function(id) {
		var knob = knobsMap[id];
		if (knob) {
			return knob.getInternalValue();
		}
	};
	me.addChangeCallback = function(id, callback) {
		var knob = knobsMap[id];
		if (knob) {
			return knob.addChangeCallback(callback);
		}
	};
	return parent;
}(LinkbotControls || {}, document));
;var LinkbotControls = (function(parent, doc) {
	"use strict";
	parent.slider = parent.slider || {};
	var me = parent.slider;
	var sliders = [];
	var slidersMap = {};

	function mouseMove(e) {
		var clickX = e.clientX || e.pageX;
		var clickY = e.clientY || e.pageY;
		for (var i in sliders) {
			var slider = sliders[i];
			if (slider.isDown()) {
				slider.updateXY(clickX, clickY);
				if(e.stopPropagation) e.stopPropagation();
			    if(e.preventDefault) e.preventDefault();
			    e.cancelBubble=true;
			    e.returnValue=false;
			    return false;
			}
		}
	}

	function mouseUp(e) {
		for (var i in sliders) {
			var slider = sliders[i];
			slider.setDown(false);
		}
	}

	function SliderControl(element) {
		var value = 0;
		var min = 0;
		var max = 100;
		var down = false; // mouse down.
		var changeRegister = [];
		var type = "int";
		if (element.dataset.type) {
			type = element.dataset.type;
		}
		if (element.dataset.min) {
			min = parseInt(element.dataset.min);
		}
		if (element.dataset.max) {
			max = parseInt(element.dataset.max);
		}
		var handleElement = document.createElement('span');
		var vertical = false;
		if (element.classList.contains('linkbotjs-vslider')) {
			vertical = true;
			handleElement.style.top = "0";
			handleElement.setAttribute('class', 'linkbotjs-vslider-handle');
		} else {
			handleElement.setAttribute('class', 'linkbotjs-slider-handle');
			handleElement.style.left = "0";
		}
		var sliderElement = element;
		sliderElement.innerHTML = "";
		sliderElement.appendChild(handleElement);

		function getPosition(element) {
		    var xPosition = 0;
		    var yPosition = 0;
		  
		    while(element) {
		        xPosition += (element.offsetLeft - element.scrollLeft + element.clientLeft);
		        yPosition += (element.offsetTop - element.scrollTop + element.clientTop);
		        element = element.offsetParent;
		    }
		    return { x: xPosition, y: yPosition };
		}

		
		return {
			getSlider: function() {
				return sliderElement;
			},
			getHandle: function() {
				return handleElement;
			},
			isVertical: function() {
				return vertical;
			},
			isDown: function() {
				return down;
			},
			setDown: function(val) {
				down = val;
			},
			updateXY: function(x, y) {
				var diff, val, tempValue;
				var position = getPosition(sliderElement);
				var box = [position.x, position.y, sliderElement.offsetWidth, sliderElement.offsetHeight];
				if (vertical) {
					diff = y - position.y;
					val = diff / box[3];
					tempValue = (val * (max - min)) + min;
					if (tempValue > max) {
						value = max;
						handleElement.style.top = '100%';
					} else if (tempValue < min) {
						value = min;
						handleElement.style.top = '0%';
					} else {
						handleElement.style.top = (val * 100) + '%';
						if (type === "int") {
							value = Math.round(tempValue);
						} else {
							value = tempValue;
						}
					}
				} else {
					diff = x - position.x;
					val = diff / box[2];
					tempValue = (val * (max - min)) + min;
					if (tempValue > max) {
						value = max;
						handleElement.style.left = '100%';
					} else if (tempValue < min) {
						value = min;
						handleElement.style.left = '0%';
						return;
					} else {
						handleElement.style.left = (val * 100) + '%';
						if (type === "int") {
							value = Math.round(tempValue);
						} else {
							value = tempValue;
						}
					}
				}
				for (var i in changeRegister) {
					changeRegister[i](value);
				}
			},
			setValue: function(val) {
				var newValue = 0;
				if (type === "int") {
					newValue = parseInt(val);
				} else if (type == "float") {
					newValue = parseFloat(val);
				}
				
				var percent;
				if (vertical) {
					percent = (newValue - min) / (max - min);
					handleElement.style.top = (percent * 100) + '%';
					value = newValue;
				} else {
					percent = (newValue - min) / (max - min);
					handleElement.style.left = (percent * 100) + '%';
					value = newValue;
				}
				for (var i in changeRegister) {
					changeRegister[i](value);
				}
			},
			getValue: function() {
				return value;
			},
			addChangeCallback: function(callback) {
				changeRegister.push(callback);
			}
		};
	}

	me.add = function(param) {
		var element = null;
		if (param instanceof HTMLElement) {
			element = param;
		} else {
			element = doc.getElementById(param);
		}
		if (!element || element === null) {
			return;
		}
		var sliderControl = new SliderControl(element);
		sliderControl.getSlider().addEventListener('mousedown', function(e) {
			if (e.button === 0) {
				sliderControl.setDown(true);
			}
		});
		sliderControl.getSlider().addEventListener('click', function(e) {
			var clickX = e.clientX || e.pageX;
			var clickY = e.clientY || e.pageY;
			sliderControl.updateXY(clickX, clickY);
		});
		if (element.id) {
			slidersMap[element.id] = sliderControl;
		}
		sliders.push(sliderControl);
	};

	me.init = function() {
		doc.addEventListener('mouseup', mouseUp);
		doc.addEventListener('mousemove', mouseMove);
		var elements = document.getElementsByClassName('linkbotjs-vslider');
		var i = 0;
		for (i = 0; i < elements.length; i++) {
			me.add(elements[i]);
		}
		elements = document.getElementsByClassName('linkbotjs-slider');
		for (i = 0; i < elements.length; i++) {
			me.add(elements[i]);
		}
	};
	me.get = function(id) {
		return slidersMap[id];
	};
	me.getValue = function(id) {
		var slider = slidersMap[id];
		if (slider) {
			return slider.getValue();
		}
		return null;
	};
	me.addChangeCallback = function(id, callback) {
		var slider = slidersMap[id];
		if (slider) {
			return slider.addChangeCallback(callback);
		}
	};
	return parent;
}(LinkbotControls || {}, document));
;var Linkbots = (function(exports, doc) {
    var manager = new RobotManager(doc);
    var storage = new Storage();
    var source = null;

    function move(srcId, destId) {
        storage.getAll(function(robots) {
            srcOrder = -1;
            destOrder = -1;
            for (var i = 0; i < robots.length; i++) {
                if (robots[i].name === srcId) {
                    srcOrder = robots[i].order;
                } else if (robots[i].name === destId) {
                    destOrder = robots[i].order;
                }
                if (destOrder >= 0 && srcOrder >= 0) {
                    break;
                }
            }
            if (destOrder >= 0 && srcOrder >= 0) {
                storage.changePosition(srcOrder, destOrder, function(success) {
                    if (success === true) {
                        // TODO: this should be moved to the robot manager.
                        var src = manager.robots.robots.splice(srcOrder, 1);
                        manager.robots.robots.splice(destOrder, 0, src[0]);
                    }
                });
            }
        });
    }


    function add(id) {
        storage.add(id, 0);
    }

    function remove(id) {
        storage.remove(id, function(success) {
            if (success) {
                storage.updateOrder();
            }
        });
    } 

    storage.getAll(function(robots) {
        for (var i = 0; i < robots.length; i++) {
            manager.add(robots[i].name);
        }
        manager.connect();
        manager.redraw();
    });


    manager.registerEvent('moved', move);
    manager.registerEvent('add', add);
    manager.registerEvent('remove', remove);  

    exports.scan = function() {
        return baroboBridge.scan();
    };
    exports.managerElement = function() {
        console.log('this method is deprecated');
        return '';
    };
    exports.topNavElement = function() {
        console.log('this method is deprecated');
        return '';
    };
    exports.acquire = function(n) {
        return manager.acquire(n);
    };
    exports.relinquish = function(l) {
        return manager.relinquish(l);
    };
    exports.managerAdd = function() {
        var ids = (1 <= arguments.length) ? [].slice.call(arguments, 0) : [];
        return manager.add.apply(manager, ids);
    };
    exports.managerRedraw = function() {
        return manager.redraw();
    };
    exports.managerConnect = function() {
        return manager.connect();
    };
    exports.setNavTitle = function(title) {
        var element = doc.getElementById('ljs-top-nav-title');
        if (element) {
            element.innerText = title;
        }
    };
    exports.setNavCrumbs = function(crumbs) {
        var element = doc.getElementById('ljs-top-nav-breadcrumbs');
        if (element) {
            element.innerText = crumbs.join(" // ");
        }
    };
    exports.storage = storage;

    LinkbotControls.knob.init();
    LinkbotControls.slider.init();

    // Add Robot Manager and Top Navigation.
    function addRobotManager() {
        manager.element.style.top = "75px";
        doc.body.style.marginTop = "90px";
        doc.body.appendChild(manager.topNav);
        doc.body.appendChild(manager.element);
    }

    if(window.attachEvent) {
        window.attachEvent('onload', addRobotManager);
    } else {
        if(window.onload) {
            var originalOnLoad = window.onload;
            var newOnLoad = function() {
                originalOnLoad();
                addRobotManager();
            };
            window.onload = newOnLoad;
        } else {
            window.onload = addRobotManager;
        }
    }
    
    return exports;
})(Linkbots || {}, document);
