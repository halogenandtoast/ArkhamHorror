module Arkham.Location.Cards.Rivertown_292 (
  rivertown_292,
  Rivertown_292 (..),
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

newtype Rivertown_292 = Rivertown_292 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertown_292 :: LocationCard Rivertown_292
rivertown_292 = location Rivertown_292 Cards.rivertown_292 3 (Static 0)

instance HasAbilities Rivertown_292 where
  getAbilities (Rivertown_292 attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 (withBreaches attrs Here) $
          ActionAbility Nothing $
            ActionCost 1 <> HandDiscardCost 1 AnyCard
      ]

getCardPayment :: Payment -> Card
getCardPayment = \case
  DiscardCardPayment [card] -> card
  Payments (DiscardCardPayment [card] : _) -> card
  Payments (_ : ps) -> getCardPayment (Payments ps)
  _ -> error "invalid payment"

instance RunMessage Rivertown_292 where
  runMessage msg l@(Rivertown_292 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ (getCardPayment -> card) -> do
      let icons = count (== #intellect) $ cdSkills $ toCardDef card
          n = min (maybe 0 countBreaches $ locationBreaches attrs) icons
      act <- selectJust AnyAct
      pushAll $ cycleN n [RemoveBreach (toTarget attrs), PlaceBreach (toTarget act)]
      pure l
    _ -> Rivertown_292 <$> runMessage msg attrs
