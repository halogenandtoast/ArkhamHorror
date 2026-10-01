module Arkham.Homebrew.CircusExMortis.Locations.FoothillSlope_162 (foothillSlope_162) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FoothillSlope_162 = FoothillSlope_162 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foothillSlope_162 :: LocationCard FoothillSlope_162
foothillSlope_162 = location FoothillSlope_162 Cards.foothillSlope_162 1 (PerPlayer 1)

instance HasAbilities FoothillSlope_162 where
  getAbilities (FoothillSlope_162 a) =
    extendRevealed1 a
      $ restricted a 1 (youExist InvestigatorWithAnyActionsRemaining)
      $ forced
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage FoothillSlope_162 where
  runMessage msg l@(FoothillSlope_162 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseActions iid (attrs.ability 1) 1
      pure l
    _ -> FoothillSlope_162 <$> liftRunMessage msg attrs
