module Arkham.Enemy.Cards.CthulhuAncientEvil (cthulhuAncientEvil) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher

newtype CthulhuAncientEvil = CthulhuAncientEvil EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- Keyword Massive is on the card def.
cthulhuAncientEvil :: EnemyCard CthulhuAncientEvil
cthulhuAncientEvil = enemy CthulhuAncientEvil Cards.cthulhuAncientEvil

instance HasModifiersFor CthulhuAncientEvil where
  getModifiersFor (CthulhuAncientEvil a) =
    modifySelf
      a
      [ -- Patrol - nearest location that can be flooded. The PatrolMove that
        -- the engine generates from this keyword already resolves "nearest"
        -- relative to this enemy, so the matcher only needs to constrain the
        -- candidate set to floodable locations.
        AddKeyword (Keyword.Patrol CanHaveFloodLevelIncreased)
      , -- "Cannot make attacks of opportunity."
        CannotMakeAttacksOfOpportunity
      ]

-- TODO: "Each Cthulhu enemy on the Cthulhu Board shares this enemy's Traits,
-- text box, and location." The Cthulhu Board (shared traits / shared text /
-- shared location and single-enemy interaction across the four facets) has no
-- engine support yet.
instance RunMessage CthulhuAncientEvil where
  runMessage msg (CthulhuAncientEvil attrs) = runQueueT $ CthulhuAncientEvil <$> liftRunMessage msg attrs
