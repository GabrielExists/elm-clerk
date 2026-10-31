module Host exposing (..)

import HostFunctions
import Kernel
import Kernel.Html
import Rule30


functionList =
    [ ( "viewerBlock"
      , HostFunctions.oneWithViewer Kernel.int Rule30.viewerBlock identity
      )
    , ( "viewerBlockRow"
      , HostFunctions.oneWithViewer
            (Kernel.list Kernel.int)
            Rule30.viewerBlockRow
            identity
      )
    , ( "viewerBlockGrid"
      , HostFunctions.oneWithViewer
            (Kernel.list (Kernel.list Kernel.int))
            Rule30.viewerBlockGrid
            identity
      )
    , ( "black", HostFunctions.constant Kernel.int Rule30.black )
    , ( "white", HostFunctions.constant Kernel.int Rule30.white )
    , ( "three", HostFunctions.constant (Kernel.list Kernel.int) Rule30.three )
    , ( "rows"
      , HostFunctions.constant
            (Kernel.list (Kernel.list Kernel.int))
            Rule30.rows
      )
    , ( "either"
      , HostFunctions.two
            Kernel.int
            Kernel.int
            Kernel.to
            (Kernel.list Kernel.int)
            Rule30.either
      )
    , ( "rule30"
      , HostFunctions.one
            (Kernel.list Kernel.int)
            Kernel.to
            Kernel.int
            Rule30.rule30
      )
    , ( "allRules"
      , HostFunctions.constant
            (Kernel.list (Kernel.list Kernel.int))
            Rule30.allRules
      )
    , ( "firstGeneration"
      , HostFunctions.constant (Kernel.list Kernel.int) Rule30.firstGeneration
      )
    , ( "evolvePreset"
      , HostFunctions.constant
            (Kernel.list (Kernel.list Kernel.int))
            Rule30.evolvePreset
      )
    , ( "evolve"
      , HostFunctions.one
            Kernel.int
            Kernel.to
            (Kernel.list (Kernel.list Kernel.int))
            Rule30.evolve
      )
    ]
