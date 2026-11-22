module Arkham.Event.Cards.PowerOfTheDreamlands where

import Arkham.Event.Cards.Import
import Arkham.Keyword qualified as Keyword

{- | A dreaming card has NO printed resource cost (@cost: -5@ in the card data);
it is paid for with dreams instead. Keep the @cdCost = Nothing@ and the
'Keyword.Dreaming' together so neither can be set without the other.
-}
dreaming :: Int -> CardDef -> CardDef
dreaming n cd = cd {cdCost = Nothing, cdKeywords = insertSet (Keyword.Dreaming n) (cdKeywords cd)}

oathsworn3 :: CardDef
oathsworn3 =
  (event "14011" "Oathsworn" 0 Guardian)
    { cdSkills = [#willpower, #willpower, #wild]
    , cdCardTraits = setFromList [Upgrade, Dreamlands]
    , -- "Attach to an Ally asset controlled by an investigator at your
      -- location": with nowhere to attach there is nothing for the play to do
      cdCriteria = Just $ exists $ #ally <> at_ YourLocation <> AssetControlledBy Anyone
    , cdLevel = Just 3
    }

oneiricInsight :: CardDef
oneiricInsight =
  (event "14021" "Oneiric Insight" 1 Seeker)
    { cdSkills = [#intellect, #agility]
    , cdCardTraits = setFromList [Insight, Dreamlands]
    }

lightOfIlarnek3 :: CardDef
lightOfIlarnek3 =
  dreaming 2
    $ (event "14026" "Light of Ilarnek" 0 Seeker)
      { cdSkills = [#intellect, #intellect]
      , cdCardTraits = setFromList [Ritual, Dreamlands]
      , cdFastWindow = Just $ DuringTurn You
      , -- no cdLimits: "Limit 1 per location" caps the copies ATTACHED to a
        -- location, and the card enforces it where that target is chosen
        cdLevel = Just 3
      }

corsairsCache :: CardDef
corsairsCache =
  dreaming 3
    $ (event "14033" "Corsair's Cache" 0 Rogue)
      { cdSkills = [#agility, #wild]
      , cdCardTraits = setFromList [Supply, Illicit, Dreamlands]
      }

doubleTeam :: CardDef
doubleTeam =
  (event "14034" "Double Team" 2 Rogue)
    { cdSkills = [#combat, #agility]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdFastWindow = Just $ AttemptToFight #when (at_ YourLocation) AnyEnemy
    }

boardingParty5 :: CardDef
boardingParty5 =
  (event "14042" "Boarding Party" 3 Rogue)
    { cdSkills = [#combat, #combat, #agility, #wild]
    , cdCardTraits = setFromList [Tactic, Trick]
    , cdActions = #fight
    , cdLevel = Just 5
    }

veilOfThalarion :: CardDef
veilOfThalarion =
  (event "14048" "Veil of Thalarion" 5 Mystic)
    { cdSkills = [#willpower, #combat, #agility]
    , cdCardTraits = setFromList [Spell]
    , cdFastWindow =
        Just
          $ OrWindowMatcher
            [ InvestigatorWouldTakeDamage #when who source AnyDamageType
            , InvestigatorWouldTakeHorror #when who source
            ]
    , cdOutOfPlayEffects = [InHandEffect]
    }
 where
  who = at_ YourLocation
  source = SourceIsCancelable AnySource

dreamQuest :: CardDef
dreamQuest =
  (event "14060" "Dream Quest" 1 Survivor)
    { cdCardTraits = setFromList [Insight, Dreamlands]
    , cdActions = #investigate
    , cdPlayableFromDiscard = True
    , cdOutOfPlayEffects = [InDiscardEffect]
    }

dreamsOfPeace :: CardDef
dreamsOfPeace =
  dreaming 2
    $ (event "14061" "Dreams of Peace" 0 Survivor)
      { cdSkills = [#willpower, #agility]
      , cdCardTraits = setFromList [Spirit, Dreamlands]
      }

callOfTheDreamlands :: CardDef
callOfTheDreamlands =
  (event "14071" "Call of the Dreamlands" 0 Neutral)
    { cdSkills = [#wild]
    , cdCardTraits = setFromList [Insight]
    }
