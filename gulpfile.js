'use strict'


var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , run         = require('gulp-run')
  ;

var sources = [
    'src/**/*.purs',
    'bower_components/purescript-*/src/**/*.purs'
];

var foreigns = [
    'src/**/*.js',
    'bower_components/purescript-*/src/**/*.js'
];

var testSources = [
    'test/**/*.purs'
];

var testForeigns = [
    'test/**/*.js'
];

gulp.task('docs', function() {
    return purescript.pscDocs({
        src: sources,
        docgen: {
            "Data.Argonaut.Encode": "docs/Data/Argonaut/Encode.md",
            "Data.Argonaut.Decode": "docs/Data/Argonaut/Decode.md",
            "Data.Argonaut.Combinators": "docs/Data/Argonaut/Combinators.md"
        }
    });
});


gulp.task('make', function() {
    return purescript.psc({
        src: sources,
        ffi: foreigns
    });
});

gulp.task('test-make', function() {
    return purescript.psc({
        src: sources.concat(testSources),
        ffi: foreigns.concat(testForeigns)
    });
});

gulp.task('test', ['test-make'], function() {
    return purescript.pscBundle({
        src: "output/**/*.js",
        main: "Test.Main",
        output: "dist/test.js"
    }).pipe(run('node dist/test.js'));
});


gulp.task("default", ["make", "docs"]);
