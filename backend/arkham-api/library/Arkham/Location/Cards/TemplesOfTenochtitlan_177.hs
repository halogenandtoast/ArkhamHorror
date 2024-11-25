module Arkham.Location.Cards.TemplesOfTenochtitlan_177 (
  templesOfTenochtitlan_177,
  TemplesOfTenochtitlan_177 (..),
) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TemplesOfTenochtitlan_177 = TemplesOfTenochtitlan_177 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templesOfTenochtitlan_177 :: LocationCard TemplesOfTenochtitlan_177
templesOfTenochtitlan_177 =
  symbolLabel
    $ location TemplesOfTenochtitlan_177 Cards.templesOfTenochtitlan_177 2 (PerPlayer 2)

instance HasAbilities TemplesOfTenochtitlan_177 where
  getAbilities (TemplesOfTenochtitlan_177 a) =
    extendRevealed
      a
      [ restricted a 1 (exists $ NearestEnemyToLocation a.id AnyEnemy)
          $ forced
          $ PutLocationIntoPlay #after Anyone (be a)
      , groupLimit PerRound
          $ restricted a 2 (Here <> cluesOnThis 1 <> CanDiscoverCluesAt (be a) <> exists AnyInPlayEnemy)
          $ actionAbilityWithCost (EnemyDoomCost 1 AnyInPlayEnemy)
      ]

instance RunMessage TemplesOfTenochtitlan_177 where
  runMessage msg l@(TemplesOfTenochtitlan_177 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      enemies <- select $ NearestEnemyToLocation attrs.id AnyEnemy
      chooseOrRunOneM lead $ targets enemies \enemy -> placeDoom (attrs.ability 1) enemy 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAt NotInvestigate iid (attrs.ability 2) attrs 2
      pure l
    _ -> TemplesOfTenochtitlan_177 <$> liftRunMessage msg attrs
