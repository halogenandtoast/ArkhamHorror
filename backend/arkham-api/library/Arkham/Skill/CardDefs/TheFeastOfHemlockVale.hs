module Arkham.Skill.CardDefs.TheFeastOfHemlockVale where

import Arkham.Skill.CardDefs.Import

purified :: CardDef
purified =
  (skill "10029" "Purified" [] Guardian)
    { cdCardTraits = setFromList [Innate, Blessed]
    , cdCommitRestrictions = [MaxOnePerTest]
    }

strongArmed1 :: CardDef
strongArmed1 =
  (skill "10031" "Strong-Armed" [#combat, #agility] Guardian)
    { cdCardTraits = setFromList [Innate]
    , cdLevel = Just 1
    }

wellFunded :: CardDef
wellFunded =
  (skill "10051" "Well-Funded" [#wild] Seeker)
    { cdCardTraits = setFromList [Fortune]
    , cdOutOfPlayEffects = [InHandEffect]
    }

esotericMethod1 :: CardDef
esotericMethod1 =
  (skill "10055" "Esoteric Method" [#wild, #wild, #wild, #wild] Seeker)
    { cdCardTraits = setFromList [Practiced, Cursed]
    , cdLevel = Just 1
    }

diabolicalLuck :: CardDef
diabolicalLuck =
  (skill "10075" "Diabolical Luck" [#wild] Rogue)
    { cdCardTraits = setFromList [Fortune, Cursed]
    , cdOutOfPlayEffects = [InHandEffect]
    }

lightfooted :: CardDef
lightfooted =
  (skill "10076" "Lightfooted" [#agility] Rogue)
    { cdCardTraits = setFromList [Practiced, Trick]
    }

accursed :: CardDef
accursed =
  (skill "10095" "Accursed" [#wild] Mystic)
    { cdCardTraits = setFromList [Innate, Cursed]
    , cdCommitTrigger = True
    }

mesmericInfluence1 :: CardDef
mesmericInfluence1 =
  (skill "10096" "Mesmeric Influence" [#willpower, #wild] Mystic)
    { cdCardTraits = setFromList [Practiced]
    , cdLevel = Just 1
    }

longShot :: CardDef
longShot =
  (skill "10116" "Long Shot" [] Survivor)
    { cdCardTraits = setFromList [Practiced]
    , cdOutOfPlayEffects = [InHandEffect]
    , cdCommitRestrictions =
        [ AnyCommitRestriction
            [ OnlyFightAgainst (EnemyAt $ oneOf [YourLocation, ConnectedLocation NotForMovement])
            , OnlyEvasionAgainst (EnemyAt $ oneOf [YourLocation, ConnectedLocation NotForMovement])
            ]
        ]
    }

persistence1 :: CardDef
persistence1 =
  (skill "10118" "Persistence" [#wild] Survivor)
    { cdCardTraits = setFromList [Practiced]
    , cdLevel = Just 1
    , cdOutOfPlayEffects = [InDiscardEffect]
    , cdCommitTrigger = True
    }

providential2 :: CardDef
providential2 =
  (skill "10125" "Providential" [#willpower, #combat, #wild] Survivor)
    { cdCardTraits = setFromList [Innate, Blessed]
    , cdLevel = Just 2
    }

wellDressed :: CardDef
wellDressed =
  (skill "10130" "Well-Dressed" [#wild] Neutral)
    { cdCardTraits = setFromList [Practiced, Fortune]
    }

theHemlockCurse :: CardDef
theHemlockCurse =
  (skill "10685" "The Hemlock Curse" [#wildMinus] Neutral)
    { cdCardTraits = setFromList [Blight, Colour]
    , cdCardSubType = Just Weakness
    , cdLevel = Nothing
    , cdEncounterSet = Just DayOfRain
    , cdEncounterSetQuantity = Just 4
    , cdOutOfPlayEffects = [InHandEffect]
    }
