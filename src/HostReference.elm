module Host exposing (..)

import HostFunctions
import Kernel
import Rule30


functionList =
    [ ( "viewerBlock"
      , HostFunctions.one Kernel.int Rule30.viewerBlock identity
      )
    , ( "viewerBlockRow"
      , HostFunctions.one
            (Kernel.list Kernel.int)
            Rule30.viewerBlockRow
            identity
      )
    , ( "viewerBlockGrid"
      , HostFunctions.one
            (Kernel.list (Kernel.list Kernel.int))
            Rule30.viewerBlockGrid
            identity
      )
    , ( "black", HostFunctions.constant Rule30.black Rule30.viewerBlock )
    , ( "white", HostFunctions.constant Rule30.white Rule30.viewerBlock )
    , ( "three", HostFunctions.constant Rule30.three Rule30.viewerBlockRow )
    , ( "rows"
      , HostFunctions.constant
            Rule30.rows
            Rule30.viewerBlockGrid
      )
    , ( "either"
      , HostFunctions.two
            Kernel.int
            Kernel.int
            Rule30.either
            Rule30.viewerBlockRow
      )
    , ( "rule30"
      , HostFunctions.one
            (Kernel.list Kernel.int)
            Rule30.rule30
            Rule30.viewerBlock
      )
    , ( "allRules"
      , HostFunctions.constant
            Rule30.allRules
            Rule30.viewerBlockGrid
      )
    , ( "firstGeneration"
      , HostFunctions.constant Rule30.firstGeneration Rule30.viewerBlockRow
      )
    , ( "evolvePreset"
      , HostFunctions.constant
            Rule30.evolvePreset
            Rule30.viewerBlockGrid
      )
    , ( "evolve"
      , HostFunctions.one
            Kernel.int
            Rule30.evolve
            Rule30.viewerBlockGrid
      )
    ]
