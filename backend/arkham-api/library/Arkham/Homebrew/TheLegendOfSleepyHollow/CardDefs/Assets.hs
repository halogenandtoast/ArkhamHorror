{- | Ichabod's Books keeps the encounter back -- act 1 puts it into play at the
Derelict Schoolhouse and it never leaves. The other two are printed on player
backs: resolution 2 offers Ichabod Crane to one investigator and a copy of Van
Brunt's Reward to each of them.
-}
module Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.TheLegendOfSleepyHollow.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

ichabodCrane :: CardDef
ichabodCrane =
  ( storyAsset
      ":the-legend-of-sleepy-hollow:013"
      ("Ichabod Crane" <:> "A Man of Superstition")
      4
      Set.TheLegendOfSleepyHollow
  )
    { cdCardTraits = setFromList [Ally, Geist]
    , cdSkills = [#willpower, #intellect, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    , cdDeckRestrictions = [PerDeckLimit 1]
    }

ichabodsBooks :: CardDef
ichabodsBooks =
  (encounterAsset_ ":the-legend-of-sleepy-hollow:014" "Ichabod's Books" Set.TheLegendOfSleepyHollow)
    { cdCardTraits = setFromList [Item, Tome]
    , cdUnique = True
    }

vanBruntsReward :: CardDef
vanBruntsReward =
  permanent
    $ ( storyAssetWithMany
          ":the-legend-of-sleepy-hollow:028"
          "Van Brunt's Reward"
          0
          Set.TheLegendOfSleepyHollow
          4
      )
      { cdCardTraits = singleton Boon
      , cdKeywords = singleton Keyword.Permanent
      , cdDeckRestrictions = [PerDeckLimit 1]
      }
