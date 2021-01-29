{ name =
    "concur-react"
, dependencies =
    [ "aff"
    , "arrays"
    , "avar"
    , "console"
    , "concur-core"
    , "foldable-traversable"
    , "free"
    , "nonempty"
    , "profunctor-lenses"
    , "react"
    , "react-dom"
    , "tailrec"
    , "web-dom"
    , "web-html"
    ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-react"
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
