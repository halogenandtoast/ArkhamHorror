module Arkham.Act.Cards.AgainstTheHouse (againstTheHouse) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher

newtype AgainstTheHouse = AgainstTheHouse ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

againstTheHouse :: ActCard AgainstTheHouse
againstTheHouse = act (2, A) AgainstTheHouse Cards.againstTheHouse Nothing

instance HasModifiersFor AgainstTheHouse where
  getModifiersFor (AgainstTheHouse a) =
    -- "Clues cannot be discovered from locations with no investigators."
    modifySelect a Anyone [CannotDiscoverCluesAt $ not_ $ LocationWithInvestigator Anyone]

instance HasAbilities AgainstTheHouse where
  getAbilities (AgainstTheHouse a) =
    [ -- "Objective - Bring this house down! If no locations are in play,
      -- immediately advance (this occurs before investigators would be
      -- defeated by not being at a location)."
      mkAbility a 1
        $ Objective
        $ forced
        $ LocationLeavesPlay #after Anywhere
    ]

instance RunMessage AgainstTheHouse where
  runMessage msg a@(AgainstTheHouse attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remaining <- selectCount Anywhere
      when (remaining == 0) $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      advanceActDeck attrs
      pure a
    _ -> AgainstTheHouse <$> liftRunMessage msg attrs
