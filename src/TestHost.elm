module TestHost exposing (functionList)

import InterpreterTypes exposing (Eval, Value)
import Kernel
import Rule30


functionList : List ( String, ( Int, List Value -> Eval Value ) )
functionList =
    [ ( "black", Kernel.constant Kernel.int Rule30.black [ "Rule30", "black" ] )
    , ( "white", Kernel.constant Kernel.int Rule30.white [ "Rule30", "white" ] )
    , ( "three", Kernel.constant (Kernel.list Kernel.int) Rule30.three [ "Rule30", "three" ] )
    , ( "rows", Kernel.constant (Kernel.list (Kernel.list Kernel.int)) Rule30.rows [ "Rule30", "rows" ] )
    , ( "either", Kernel.two Kernel.int Kernel.int Kernel.to (Kernel.list Kernel.int) Rule30.either [ "Rule30", "either" ] )
    , ( "rule30", Kernel.one (Kernel.list Kernel.int) Kernel.to Kernel.int Rule30.rule30 [ "Rule30", "rule30" ] )
    , ( "allRules", Kernel.constant (Kernel.list (Kernel.list Kernel.int)) Rule30.allRules [ "Rule30", "allRules" ] )
    , ( "firstGeneration", Kernel.constant (Kernel.list Kernel.int) Rule30.firstGeneration [ "Rule30", "firstGeneration" ] )
    , ( "evolvePreset", Kernel.constant (Kernel.list (Kernel.list Kernel.int)) Rule30.evolvePreset [ "Rule30", "evolvePreset" ] )
    , ( "evolve", Kernel.one Kernel.int Kernel.to (Kernel.list (Kernel.list Kernel.int)) Rule30.evolve [ "Rule30", "evolve" ] )
    ]
