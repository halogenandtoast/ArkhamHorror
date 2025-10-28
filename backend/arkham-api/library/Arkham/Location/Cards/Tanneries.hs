module Arkham.Location.Cards.Tanneries (tanneries) where

import Arkham.Ability
import Arkham.Enemy.Creation
import Arkham.Enemy.Types (Field (EnemyHealth))
import Arkham.Helpers.Enemy
import Arkham.Helpers.Location (swapLocation)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.DeadHeat.Helpers
import Arkham.Trait (Trait (Risen))

newtype Tanneries = Tanneries LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tanneries :: LocationCard Tanneries
tanneries = symbolLabel $ location Tanneries Cards.tanneries 2 (PerPlayer 1)

instance HasAbilities Tanneries where
  getAbilities (Tanneries a) =
    if a.revealed
      then
        extendRevealed
          a
          [ restricted a 1 Here
              $ FastAbility (ChooseExtendedCardCost $ InEncounterDiscard <> basic (#enemy <> CardWithTrait Risen))
          , becomeAbandonedAbility a 2
          ]
      else extendUnrevealed1 a $ becomeAbandonedAbility a 1

instance RunMessage Tanneries where
  runMessage msg l@(Tanneries attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 | attrs.unrevealed -> do
      swapLocation attrs =<< fetchCard Cards.tanneriesAbandoned
      pure l
    UseCardAbility iid (isSource attrs -> True) 1 _ (chosenCardPayment -> Just cardId) -> do
      enemy <- createEnemyCard cardId (SpawnEngagedWith iid)
      forTarget enemy msg
      pure l
    ForTarget (EnemyTarget eid) (UseThisAbility _ (isSource attrs -> True) 1) -> do
      mn <- getEnemyField EnemyHealth eid
      for_ mn $ placeClues (attrs.ability 1) attrs
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      swapLocation attrs =<< fetchCard Cards.tanneriesAbandoned
      pure l
    _ -> Tanneries <$> liftRunMessage msg attrs
