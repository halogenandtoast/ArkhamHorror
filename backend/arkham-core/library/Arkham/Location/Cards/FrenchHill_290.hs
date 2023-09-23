module Arkham.Location.Cards.FrenchHill_290 (
  frenchHill_290,
  FrenchHill_290 (..),
)
where

import Arkham.Prelude

import Arkham.Card
import Arkham.GameValue
import Arkham.Location.BreachStatus
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.SkillType ()

newtype FrenchHill_290 = FrenchHill_290 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill_290 :: LocationCard FrenchHill_290
frenchHill_290 = location FrenchHill_290 Cards.frenchHill_290 3 (Static 0)

instance HasAbilities FrenchHill_290 where
  getAbilities (FrenchHill_290 attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 (withBreaches attrs Here)
          $ ActionAbility Nothing
          $ ActionCost 1
          <> HandDiscardCost 1 AnyCard
      ]

getCardPayment :: Payment -> Card
getCardPayment = \case
  DiscardCardPayment [card] -> card
  Payments (DiscardCardPayment [card] : _) -> card
  Payments (_ : ps) -> getCardPayment (Payments ps)
  _ -> error "invalid payment"

instance RunMessage FrenchHill_290 where
  runMessage msg l@(FrenchHill_290 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ (getCardPayment -> card) -> do
      let icons = count (== #willpower) $ cdSkills $ toCardDef card
          n = 1 + min (maybe 0 countBreaches $ locationBreaches attrs) icons
      act <- selectJust AnyAct
      pushAll [RemoveBreaches (toTarget attrs) n, PlaceBreaches (toTarget act) n]
      pure l
    _ -> FrenchHill_290 <$> runMessage msg attrs
