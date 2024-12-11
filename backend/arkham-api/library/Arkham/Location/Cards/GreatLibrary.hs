module Arkham.Location.Cards.GreatLibrary (greatLibrary, GreatLibrary (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationClues))
import Arkham.Matcher
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype GreatLibrary = GreatLibrary LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greatLibrary :: LocationCard GreatLibrary
greatLibrary = location GreatLibrary Cards.greatLibrary 2 (Static 4)

instance HasAbilities GreatLibrary where
  getAbilities (GreatLibrary attrs) =
    extendRevealed
      attrs
      [ skillTestAbility $ restricted attrs 1 Here $ actionAbilityWithCost (ClueCost $ PerPlayer 1)
      , restricted attrs 2 (CluesOnThis $ LessThan $ Static 4) $ forced $ RoundEnds #when
      ]

instance RunMessage GreatLibrary where
  runMessage msg l@(GreatLibrary attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      current <- field LocationClues attrs.id
      placeClues (attrs.ability 2) attrs (4 - current)
      pure l
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      remember FoundTheProcess
      pure l
    _ -> GreatLibrary <$> liftRunMessage msg attrs
