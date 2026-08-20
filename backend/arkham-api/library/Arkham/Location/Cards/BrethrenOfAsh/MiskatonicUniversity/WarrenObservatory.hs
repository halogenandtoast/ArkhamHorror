{- HLINT ignore "Use camelCase" -}
module Arkham.Location.Cards.BrethrenOfAsh.MiskatonicUniversity.WarrenObservatory (warrenObservatory) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.CardDefs.BrethrenOfAsh.MiskatonicUniversity qualified as Cards (
  warrenObservatory,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype WarrenObservatory = WarrenObservatory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

warrenObservatory :: LocationCard WarrenObservatory
warrenObservatory =
  location
    WarrenObservatory
    Cards.warrenObservatory
    3
    (PerPlayer 1)

instance HasAbilities WarrenObservatory where
  getAbilities (WarrenObservatory a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ freeReaction
      $ DiscoverClues #after You (be a) (atLeast 1)

instance RunMessage WarrenObservatory where
  runMessage msg l@(WarrenObservatory attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure l
    _ -> WarrenObservatory <$> liftRunMessage msg attrs
