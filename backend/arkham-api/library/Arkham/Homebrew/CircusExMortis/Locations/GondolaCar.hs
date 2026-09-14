module Arkham.Homebrew.CircusExMortis.Locations.GondolaCar (gondolaCar) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GondolaCar = GondolaCar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gondolaCar :: LocationCard GondolaCar
gondolaCar = location GondolaCar Cards.gondolaCar 2 (Static 1)

instance HasAbilities GondolaCar where
  getAbilities (GondolaCar a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ freeReaction (PlayEvent #after You EventTargetsEnemy)

instance RunMessage GondolaCar where
  runMessage msg (GondolaCar attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      drawCards iid (attrs.ability 1) 1
      pure $ GondolaCar attrs
    _ -> GondolaCar <$> liftRunMessage msg attrs
