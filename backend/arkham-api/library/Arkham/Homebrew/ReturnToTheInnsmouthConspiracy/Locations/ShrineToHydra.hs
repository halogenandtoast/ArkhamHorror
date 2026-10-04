module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.ShrineToHydra (shrineToHydra) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (deepOneInvestigator, scenarioI18n)
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as DevilReefLocations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype ShrineToHydra = ShrineToHydra LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shrineToHydra :: LocationCard ShrineToHydra
shrineToHydra = location ShrineToHydra Cards.shrineToHydra 4 (PerPlayer 1)

-- | "Salt Marshes is connected to Shrine to Hydra."
instance HasModifiersFor ShrineToHydra where
  getModifiersFor (ShrineToHydra a) =
    modifySelect
      a
      (locationIs DevilReefLocations.saltMarshes)
      [ConnectedToWhen (locationIs DevilReefLocations.saltMarshes) (be a)]

instance HasAbilities ShrineToHydra where
  getAbilities (ShrineToHydra a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 (Here <> youExist (not_ deepOneInvestigator))
          $ ActionAbility mempty Nothing
          $ ActionCost 1
      ]

instance RunMessage ShrineToHydra where
  runMessage msg l@(ShrineToHydra attrs) = runQueueT $ scenarioI18n "returnToDevilReef" $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      -- "You hear a voice calling to you, promising power and riches. Put Shrine to
      -- Hydra into play."
      placeLocation_ (toCard attrs)
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "You accept the voice's bargain. Read Scenario Interlude: A Bargain." The
      -- interlude belongs to the scenario, which owns the choice and its consequences.
      push $ Msg.ScenarioSpecific "aBargain" (toJSON iid)
      pure l
    _ -> ShrineToHydra <$> liftRunMessage msg attrs
