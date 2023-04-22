module Arkham.Location.Cards.MasterBedroom
  ( masterBedroom
  , MasterBedroom(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Timing qualified as Timing
import Arkham.Trait ( Trait (SilverTwilight) )

newtype MasterBedroom = MasterBedroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

masterBedroom :: LocationCard MasterBedroom
masterBedroom = location MasterBedroom Cards.masterBedroom 3 (PerPlayer 1)

instance HasAbilities MasterBedroom where
  getAbilities (MasterBedroom a) = withBaseAbilities
    a
    [ restrictedAbility
        a
        1
        (InvestigatorExists (investigatorAt $ toId a) <> EnemyCriteria
          (EnemyExists
          $ EnemyWithTrait SilverTwilight
          <> EnemyWithoutModifier CannotPlaceDoomOnThis
          )
        )
      $ ForcedAbility
      $ RoundEnds Timing.When
    ]

instance RunMessage MasterBedroom where
  runMessage msg l@(MasterBedroom attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      enemies <- selectList $ NearestEnemyToLocation
        (toId attrs)
        (EnemyWithTrait SilverTwilight
        <> EnemyWithoutModifier CannotPlaceDoomOnThis
        )
      unless (null enemies) $ do
        lead <- getLead
        push $ chooseOrRunOne
          lead
          [ targetLabel enemy [PlaceDoom (toTarget enemy) 1]
          | enemy <- enemies
          ]
      pure l
    _ -> MasterBedroom <$> runMessage msg attrs
