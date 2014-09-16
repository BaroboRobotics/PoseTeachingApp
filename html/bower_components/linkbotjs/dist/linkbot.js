
/* LinkbotJS 0.1.1 */

(function() {
  var Linkbot, RobotManager, RobotStatus, baroboBridge, buttonSlot, k, methods, obj, signals, wheelSlot,
    __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
    __hasProp = {}.hasOwnProperty;

  RobotStatus = (function() {
    function RobotStatus(robots) {
      this.robots = robots != null ? robots : [];
    }

    RobotStatus.prototype.list = function() {
      return this.robots;
    };

    RobotStatus.prototype.acquire = function(n) {
      var readyBots, ret, rs;
      readyBots = this.robots.filter(function(r) {
        return r.status === "ready";
      });
      ret = {
        robots: [],
        registered: this.robots.length,
        ready: readyBots.length
      };
      if (ret.ready >= n) {
        rs = readyBots.slice(0, n);
        rs.map(function(r) {
          return r.status = "acquired";
        });
        ret.robots = rs.map(function(r) {
          return r.linkbot;
        });
        ret.ready -= n;
      }
      return ret;
    };

    RobotStatus.prototype.add = function(id) {
      if (this.robots.map(function(x) {
        return x.id;
      }).indexOf(id) < 0) {
        return this.robots.push({
          status: "new",
          id: id
        });
      } else {
        return false;
      }
    };

    RobotStatus.prototype.remove = function(id) {
      var idx;
      idx = this.robots.map(function(x) {
        return x.id;
      }).indexOf(id);
      if (idx >= 0) {
        return this.robots.splice(idx, 1);
      } else {
        return false;
      }
    };

    RobotStatus.prototype.relinquish = function(bot) {
      var idx;
      idx = this.robots.map(function(x) {
        return x.id;
      }).indexOf(bot._id);
      if (idx >= 0 && this.robots[idx].status === "acquired") {
        return this.robots[idx].status = "ready";
      } else {
        return false;
      }
    };

    RobotStatus.prototype.ready = function(idx, bot) {
      var _ref, _ref1;
      if ((_ref = this.robots[idx]) != null) {
        _ref.linkbot = bot;
      }
      return (_ref1 = this.robots[idx]) != null ? _ref1.status = "ready" : void 0;
    };

    RobotStatus.prototype.fail = function(idx) {
      var _ref;
      return (_ref = this.robots[idx]) != null ? _ref.status = "failed" : void 0;
    };

    return RobotStatus;

  })();

  RobotManager = (function() {
    function RobotManager(document) {
      this._uiRemoveFn = __bind(this._uiRemoveFn, this);
      this._uiMenuSlide = __bind(this._uiMenuSlide, this);
      this._uiAdd = __bind(this._uiAdd, this);
      this.robots = new RobotStatus();
      this.element = this._constructElement(document);
    }

    RobotManager.prototype._constructElement = function(document) {
      var addBtn, el, pulloutBtn;
      el = document.createElement('div');
      el.setAttribute('class', 'robomgr-container robomgr-container-hidden');
      el.innerHTML = '<div class="robomgr-pullout">' + '<span class="robomgr-pulloutbtn robomgr-right"></span>' + '</div>' + '<form>' + '<div id="robotFormContainer">' + '<label for="robotInput" id="robotInputLabel" class="sr-only">' + 'Linkbot ID' + '</label>' + '<input name="robotInput" id="robotInput" type="text" placeholder="Linkbot ID">' + '<button id="robomgr-add">Add</button>' + '</div>' + '</form>' + '<ol></ol>';
      addBtn = el.querySelector('button');
      pulloutBtn = el.querySelector('.robomgr-pullout');
      addBtn.addEventListener('click', this._uiAdd);
      pulloutBtn.addEventListener('click', this._uiMenuSlide);
      return el;
    };

    RobotManager.prototype._uiAdd = function(e) {
      var idInput;
      e.preventDefault();
      idInput = this.element.querySelector('input#robotInput');
      this.robots.add(idInput.value);
      idInput.value = "";
      this.drawList();
      this.connect();
      return this.drawList();
    };

    RobotManager.prototype._uiMenuSlide = function(e) {
      var container, left, spanBtn;
      e.preventDefault();
      spanBtn = this.element.querySelector('span');
      container = document.querySelector('.robomgr-container');
      left = /robomgr-left/.test(spanBtn.className);
      if (left) {
        spanBtn.className = 'robomgr-pulloutbtn robomgr-right';
        container.className = 'robomgr-container robomgr-container-hidden';
      } else {
        spanBtn.className = 'robomgr-pulloutbtn robomgr-left';
        container.className = 'robomgr-container robomgr-container-open';
      }
      return e;
    };

    RobotManager.prototype._uiRemoveFn = function(id) {
      return (function(_this) {
        return function(e) {
          e.preventDefault();
          _this.robots.remove(id);
          return _this.drawList();
        };
      })(this);
    };

    RobotManager.prototype._robotLi = function(doc, r) {
      var li, rm;
      li = doc.createElement('li');
      rm = doc.createElement('span');
      rm.innerText = '[-]';
      rm.setAttribute('class', "robomgr--rmBtn robomgr--hoverItem");
      rm.addEventListener('click', this._uiRemoveFn(r.id));
      li.setAttribute('class', "robomgr--" + r.status);
      li.innerText = r.id;
      li.appendChild(rm);
      li.addEventListener('mouseover', function(e) {
        e.stopPropagation();
        if (e.currentTarget.nodeName === "LI") {
          return e.currentTarget.classList.add("robomgr--roboHover");
        }
      });
      li.addEventListener('mouseout', function(e) {
        e.stopPropagation();
        if (e.currentTarget.nodeName === "LI") {
          return e.currentTarget.classList.remove("robomgr--roboHover");
        }
      });
      return li;
    };

    RobotManager.prototype.drawList = function() {
      var doc, ol, r, _i, _len, _ref;
      doc = this.element.ownerDocument;
      ol = doc.createElement('ol');
      _ref = this.robots.list();
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        r = _ref[_i];
        ol.appendChild(this._robotLi(doc, r));
      }
      return this.element.replaceChild(ol, this.element.querySelector('ol'));
    };

    RobotManager.prototype.connect = function() {
      var bot, idx, r, _i, _len, _ref, _results;
      _ref = this.robots.list();
      _results = [];
      for (idx = _i = 0, _len = _ref.length; _i < _len; idx = ++_i) {
        r = _ref[idx];
        if (r.status === "new") {
          bot = new Linkbot(r.id);
          if (bot._id != null) {
            _results.push(this.robots.ready(idx, bot));
          } else {
            _results.push(this.robots.fail(idx));
          }
        } else {
          _results.push(void 0);
        }
      }
      return _results;
    };

    RobotManager.prototype.relinquish = function(l) {
      l.disconnect();
      this.robots.relinquish(l);
      return this.drawList();
    };

    RobotManager.prototype.acquire = function(n) {
      var x;
      x = this.robots.acquire(n);
      this.drawList();
      return x;
    };

    return RobotManager;

  })();

  this.Linkbots = (function(doc) {
    var manager;
    manager = new RobotManager(doc);
    return {
      scan: function() {
        return baroboBridge.scan();
      },
      managerElement: function() {
        return manager.element;
      },
      acquire: function(n) {
        return manager.acquire(n);
      },
      relinquish: function(l) {
        return manager.relinquish(l);
      }
    };
  })(this.document);

  Linkbot = (function() {
    Linkbot.prototype._wheelRadius = 1.75;

    function Linkbot(_id) {
      var blessedFW, err, idAsURI, m, _i;
      this._id = _id;
      err = baroboBridge.connectRobot(this._id);
      if (err < 0) {
        this._id = null;
        return;
      }
      for (m = _i = 1; _i <= 3; m = ++_i) {
        baroboBridge.setMotorEventThreshold(this._id, m, 1e10);
      }
      this._wheelPositions = baroboBridge.getMotorAngles(this._id);
      this._firmwareVersion = baroboBridge.firmwareVersion(this._id);
      if (!baroboBridge.mock) {
        blessedFW = baroboBridge.availableFirmwareVersions();
        if (blessedFW.indexOf(this._firmwareVersion) < 0) {
          idAsURI = encodeURIComponent(this._id);
          this.disconnect();
          document.location = "../LinkbotUpdate/index.html?badRobot=" + idAsURI;
        }
      }
    }

    Linkbot.prototype.color = function(r, g, b) {
      return baroboBridge.setLEDColor(this._id, r, g, b);
    };

    Linkbot.prototype.angularSpeed = function(s1, s2, s3) {
      if (s2 == null) {
        s2 = s1;
      }
      if (s3 == null) {
        s3 = s1;
      }
      return baroboBridge.angularSpeed(this._id, s1, s2, s3);
    };

    Linkbot.prototype.move = function(r1, r2, r3) {
      return baroboBridge.move(this._id, r1, r2, r3);
    };

    Linkbot.prototype.moveTo = function(r1, r2, r3) {
      return baroboBridge.moveTo(this._id, r1, r2, r3);
    };

    Linkbot.prototype.wheelPositions = function() {
      return this._wheelPositions = baroboBridge.getMotorAngles(this._id);
    };

    Linkbot.prototype.stop = function() {
      return baroboBridge.stop(this._id);
    };

    Linkbot.prototype.buzzerFrequency = function(freq) {
      return baroboBridge.buzzerFrequency(this._id, freq);
    };

    Linkbot.prototype.disconnect = function() {
      this.stop();
      return this._id = null;
    };

    Linkbot.prototype.register = function(connections) {
      var buttonId, registerObject, slot, wheelId, _ref, _ref1, _results, _wheelId;
      if (connections.button != null) {
        _ref = connections.button;
        for (buttonId in _ref) {
          if (!__hasProp.call(_ref, buttonId)) continue;
          registerObject = _ref[buttonId];
          slot = buttonSlot(this, parseInt(buttonId), registerObject.callback, registerObject.data);
          baroboBridge.buttonChanged.connect(slot);
          baroboBridge.enableButtonSignals(this._id);
        }
      }
      if (connections.wheel != null) {
        _ref1 = connections.wheel;
        _results = [];
        for (_wheelId in _ref1) {
          if (!__hasProp.call(_ref1, _wheelId)) continue;
          registerObject = _ref1[_wheelId];
          wheelId = parseInt(_wheelId);
          slot = wheelSlot(this, wheelId, registerObject.callback, registerObject.data);
          baroboBridge.setMotorEventThreshold(this._id, wheelId, registerObject.distance);
          baroboBridge.motorChanged.connect(slot);
          _results.push(baroboBridge.enableMotorSignals(this._id));
        }
        return _results;
      }
    };

    Linkbot.prototype.unregister = function() {
      baroboBridge.motorChanged.disconnect();
      baroboBridge.disableMotorSignals(this._id);
      baroboBridge.buttonChanged.disconnect();
      return baroboBridge.disableButtonSignals(this._id);
    };

    return Linkbot;

  })();

  buttonSlot = function(robot, buttonId, callback, model) {
    if (model == null) {
      model = {};
    }
    return function(robID, btnID, press) {
      if (press === 1 && robot._id === robID && buttonId === btnID) {
        return callback(robot, model, {
          button: btnID
        });
      }
    };
  };

  wheelSlot = function(robot, wheelId, callback, model) {
    if (model == null) {
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
  };

  baroboBridge = (function() {
    var _i, _j, _len, _len1;
    if (this.baroboBridge != null) {
      return this.baroboBridge;
    } else {
      methods = ['angularSpeed', 'availableFirmwareVersions', 'buttonChanged', 'connectRobot', 'disconnectRobot', 'enableButtonSignals', 'enableMotorSignals', 'disableButtonSignals', 'disableMotorSignals', 'firmwareVersion', 'getMotorAngles', 'scan', 'setMotorEventThreshold', 'stop'];
      signals = ['motorChanged', 'buttonChanged'];
      obj = {
        mock: true
      };
      for (_i = 0, _len = methods.length; _i < _len; _i++) {
        k = methods[_i];
        obj[k] = function() {};
      }
      for (_j = 0, _len1 = signals.length; _j < _len1; _j++) {
        k = signals[_j];
        obj[k] = {
          connect: (function() {}),
          disconnect: (function() {})
        };
      }
      return obj;
    }
  }).call(this);

}).call(this);
