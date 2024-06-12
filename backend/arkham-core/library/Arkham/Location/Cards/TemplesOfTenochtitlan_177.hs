module Arkham.Location.Cards.TemplesOfTenochtitlan_177 (
  templesOfTenochtitlan_177,
  TemplesOfTenochtitlan_177 (..),
) where

import Arkham.Ability
import Arkham.Discover
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude

newtype TemplesOfTenochtitlan_177 = TemplesOfTenochtitlan_177 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templesOfTenochtitlan_177 :: LocationCard TemplesOfTenochtitlan_177
templesOfTenochtitlan_177 =
  locationWith
    TemplesOfTenochtitlan_177
    Cards.templesOfTenochtitlan_177
    2
    (PerPlayer 2)
    (labelL .~ "square")

-- TODO: We need to place doom on an enemy as a cost
instance HasAbilities TemplesOfTenochtitlan_177 where
  getAbilities (TemplesOfTenochtitlan_177 attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (exists $ NearestEnemyToLocation attrs.id AnyEnemy)
          $ forced
          $ PutLocationIntoPlay #after Anyone (be attrs)
      , groupLimit PerRound
          $ restrictedAbility
            attrs
            2
            ( Here
                <> CluesOnThis (atLeast 1)
                <> CanDiscoverCluesAt (LocationWithId attrs.id)
                <> exists AnyEnemy
            )
            actionAbility
      ]

instance RunMessage TemplesOfTenochtitlan_177 where
  runMessage msg l@(TemplesOfTenochtitlan_177 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      targets <- selectMap EnemyTarget $ NearestEnemyToLocation attrs.id AnyEnemy
      unless (null targets)
        $ push
        $ chooseOrRunOne lead [targetLabel target [PlaceDoom (attrs.ability 1) target 1] | target <- targets]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ Msg.DiscoverClues iid $ discover attrs (attrs.ability 2) 2
      pure l
    _ -> TemplesOfTenochtitlan_177 <$> runMessage msg attrs
