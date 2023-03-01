module Arkham.Location.Cards.TemplesOfTenochtitlan_177
  ( templesOfTenochtitlan_177
  , TemplesOfTenochtitlan_177(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype TemplesOfTenochtitlan_177 = TemplesOfTenochtitlan_177 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templesOfTenochtitlan_177 :: LocationCard TemplesOfTenochtitlan_177
templesOfTenochtitlan_177 = locationWith
  TemplesOfTenochtitlan_177
  Cards.templesOfTenochtitlan_177
  2
  (PerPlayer 2)
  (labelL .~ "square")

-- TODO: We need to place doom on an enemy as a cost
instance HasAbilities TemplesOfTenochtitlan_177 where
  getAbilities (TemplesOfTenochtitlan_177 attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 (EnemyCriteria $ EnemyExists $ NearestEnemyToLocation (toId attrs) AnyEnemy)
    $ ForcedAbility
    $ PutLocationIntoPlay Timing.After Anyone
    $ LocationWithId
    $ toId attrs
    , limitedAbility (GroupLimit PerRound 1)
    $ restrictedAbility
        attrs
        2
        (Here
        <> CluesOnThis (AtLeast $ Static 1)
        <> CanDiscoverCluesAt (LocationWithId $ toId attrs)
        <> EnemyCriteria (EnemyExists AnyEnemy)
        )
    $ ActionAbility Nothing
    $ ActionCost 1
    ]

instance RunMessage TemplesOfTenochtitlan_177 where
  runMessage msg l@(TemplesOfTenochtitlan_177 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      leadInvestigatorId <- getLeadInvestigatorId
      targets <- selectListMap EnemyTarget
        $ NearestEnemyToLocation (toId attrs) AnyEnemy
      unless (null targets) $ push $ chooseOrRunOne
        leadInvestigatorId
        [ TargetLabel target [PlaceDoom target 1] | target <- targets ]
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ InvestigatorDiscoverClues iid (toId attrs) 2 Nothing
      pure l
    _ -> TemplesOfTenochtitlan_177 <$> runMessage msg attrs
