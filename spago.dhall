{ name = "datetime-iso8601"
, dependencies =
  [ "aff", "console", "datetime", "either", "effect", "spec", "strings" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
