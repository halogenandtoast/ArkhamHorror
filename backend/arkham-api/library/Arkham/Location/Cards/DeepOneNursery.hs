module Arkham.Location.Cards.DeepOneNursery (deepOneNursery, DeepOneNursery (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ALightInTheFog.Helpers

newtype DeepOneNursery = DeepOneNursery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneNursery :: LocationCard DeepOneNursery
deepOneNursery = location DeepOneNursery Cards.deepOneNursery 0 (Static 0)

instance HasAbilities DeepOneNursery where
  getAbilities (DeepOneNursery a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 (Here <> youExist (InvestigatorWithKey PurpleKey))
          $ FastAbility Free
      , restricted a 2 (thisExists a (not_ FullyFloodedLocation))
          $ forced
          $ RevealLocation #after Anyone (be a)
      ]

instance RunMessage DeepOneNursery where
  runMessage msg l@(DeepOneNursery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flashback iid Flashback13
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      increaseThisFloodLevelTo attrs PartiallyFlooded
      pure l
    _ -> DeepOneNursery <$> liftRunMessage msg attrs
