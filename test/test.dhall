let conf = ../spago.dhall
in conf //
  { dependencies = conf.dependencies # [ "assert", "console", "quickcheck", "psci-support" ]
  , sources = conf.sources # [ "test/**/*.purs" ]
  }
