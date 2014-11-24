module.exports = function(grunt) {

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    concat: {
      options: {
        separator: ';'
      },
      dist: {
        src: ['src/barobo-bridge.js', 'src/robot-status.js', 'src/robot-manager.js', 'src/link.js', 'src/storage.js',
         'src/knob-control.js', 'src/slider-control.js', 'src/main.js'],
        dest: 'dist/linkbot.js'
      }
    },
    uglify: {
      options: {
        banner: '/*! <%= pkg.name %> <%= grunt.template.today("dd-mm-yyyy") %> */\n'
      },
      dist: {
        files: {
          'dist/linkbot.min.js': ['<%= concat.dist.dest %>']
        }
      }
    },
    copy: {
      main: {
        files: [
          {expand: true, cwd: 'src/', src: ['*.css'], dest: 'dist/', filter: 'isFile'},
          {expand: true, cwd: 'src/', src: ['img/**'], dest: 'dist/'}
        ]
      }
    },
    qunit: {
      files: ['test/**/*.html']
    },
    jshint: {
      files: ['Gruntfile.js', 'src/barobo-bridge.js', 'src/robot-status.js', 'src/robot-manager.js', 'src/link.js',
       'src/main.js', 'src/storage.js', 'src/knob-control.js', 'src/slider-control.js', 'test/**/*.js'],
      options: {
        // options here to override JSHint defaults
        globals: {
          jQuery: true,
          console: true,
          module: true,
          document: true
        }
      }
    },
    watch: {
      files: ['<%= jshint.files %>'],
      tasks: ['jshint', 'qunit']
    }
  });

  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-qunit');
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-copy');

  grunt.registerTask('test', ['jshint', 'qunit']);

  grunt.registerTask('default', ['jshint', 'concat', 'uglify', 'copy']);

};
