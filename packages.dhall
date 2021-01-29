let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20210118/packages.dhall sha256:a59c5c93a68d5d066f3815a89f398bcf00e130a51cb185b2da29b20e2d8ae115

let overrides = { concur-react = ./spago.dhall as Location }

let additions =
      { concur-core =
          { dependencies =
              [ "arrays"
              , "console"
              , "foldable-traversable"
              , "nonempty"
              , "profunctor-lenses"
              , "tailrec"
              , "event"
              ]
          , repo =
              "https://github.com/srghma/purescript-concur-core"
          , version =
              "master"
          }
      }

in  upstream // overrides // additions
