module.exports = function(grunt) {

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    concat: {
      dist: {
        src: ['src/js/barobo-bridge.js', 'tmp/linkbot.js'],
        dest: 'dist/linkbot.js'
      }
    },
    uglify: {
      options: {
        banner: '/*! <%= pkg.name %> <%= grunt.template.today("dd-mm-yyyy") %> */\n',
        sourceMap: true
      },
      dist: {
        files: {
          'dist/linkbot.min.js': ['<%= concat.dist.dest %>']
        }
      }
    },
    browserify: {
      dev: {
        files: { 'tmp/linkbot.js': 'src/jsx/**/*.jsx'},
        options: {
          debug: true,
          transform: ['reactify']
        }

      },
      prod: {
        files: { 'tmp/linkbot.js': 'src/jsx/**/*.jsx'},
        options: {
          debug: false,
          transform: ['reactify']
        }

      }
    },
    copy: {
      main: {
        files: [
          {expand: true, cwd: 'src/css', src: 'linkbot.css', dest: 'dist/', filter: 'isFile'},
          {expand: true, cwd: 'src/', src: ['img/**'], dest: 'dist/'}
        ]
      }
    },
    qunit: {
      files: ['test/**/*.html']
    },
    jshint: {
      files: ['Gruntfile.js', 'src/js/barobo-bridge.js', 'test/**/*.js'],
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
  grunt.loadNpmTasks('grunt-contrib-watch');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-copy');
  grunt.loadNpmTasks('grunt-browserify');

  grunt.registerTask('test', ['jshint', 'qunit']);

  grunt.registerTask('default', ['browserify:dev', 'jshint', 'copy', 'concat', 'uglify']);
  grunt.registerTask('build', ['browserify:prod', 'jshint', 'copy', 'concat', 'uglify']);

};
