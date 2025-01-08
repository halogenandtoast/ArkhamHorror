module Arkham.Location.Cards.DeepDrifts (deepDrifts) where

import Arkham.Ability
import Arkham.Card
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers

newtype DeepDrifts = DeepDrifts LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepDrifts :: LocationCard DeepDrifts
deepDrifts = locationWith DeepDrifts Cards.deepDrifts 1 (PerPlayer 3) (connectsToL .~ adjacentLocations)

instance HasAbilities DeepDrifts where
  getAbilities (DeepDrifts a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ Moves #after You AnySource (below a) (be a)

instance HasModifiersFor DeepDrifts where
  getModifiersFor (DeepDrifts l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance RunMessage DeepDrifts where
  runMessage msg l@(DeepDrifts attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfDeckAndHandle iid (attrs.ability 1) 3 attrs
      pure l
    DiscardedTopOfDeck iid cards (isAbilitySource attrs 1 -> True) (isTarget attrs -> True) -> do
      let weaknesses = filterCards WeaknessCard cards
      focusCards_ weaknesses $ chooseOneAtATimeM iid $ targets weaknesses $ drawCard iid
      pure l
    _ -> DeepDrifts <$> liftRunMessage msg attrs
