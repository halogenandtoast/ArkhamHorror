module Arkham.Location.Cards.Uptown_296 (
  uptown_296,
  Uptown_296 (..),
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

newtype Uptown_296 = Uptown_296 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

uptown_296 :: LocationCard Uptown_296
uptown_296 = location Uptown_296 Cards.uptown_296 3 (Static 0)

instance HasAbilities Uptown_296 where
  getAbilities (Uptown_296 attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 (withBreaches attrs Here)
          $ ActionAbility []
          $ ActionCost 1
          <> HandDiscardCost 1 AnyCard
      ]

getCardPayment :: Payment -> Card
getCardPayment = \case
  DiscardCardPayment [card] -> card
  Payments (DiscardCardPayment [card] : _) -> card
  Payments (_ : ps) -> getCardPayment (Payments ps)
  _ -> error "invalid payment"

instance RunMessage Uptown_296 where
  runMessage msg l@(Uptown_296 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ (getCardPayment -> card) -> do
      let icons = count (== #agility) $ cdSkills $ toCardDef card
          n = 1 + min (maybe 0 countBreaches $ locationBreaches attrs) icons
      act <- selectJust AnyAct
      pushAll [RemoveBreaches (toTarget attrs) n, PlaceBreaches (toTarget act) n]
      pure l
    _ -> Uptown_296 <$> runMessage msg attrs
