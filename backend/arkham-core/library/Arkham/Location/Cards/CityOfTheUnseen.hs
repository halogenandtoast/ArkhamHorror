module Arkham.Location.Cards.CityOfTheUnseen
  ( cityOfTheUnseen
  , CityOfTheUnseen(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype Metadata = Metadata { inUse :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CityOfTheUnseen = CityOfTheUnseen (LocationAttrs `With` Metadata)
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfTheUnseen :: LocationCard CityOfTheUnseen
cityOfTheUnseen = location
  (CityOfTheUnseen . (`with` Metadata False))
  Cards.cityOfTheUnseen
  4
  (Static 1)

instance HasAbilities CityOfTheUnseen where
  getAbilities (CityOfTheUnseen (attrs `With` meta)) = withBaseAbilities
    attrs
    [ limitedAbility (GroupLimit PerWindow 1)
      $ restrictedAbility attrs 1 (if inUse meta then Never else NoRestriction)
      $ ForcedAbility
      $ PlacedCounterOnEnemy
          Timing.After
          (enemyAt $ toId attrs)
          DoomCounter
          (AtLeast $ Static 1)
    ]

getEnemyId :: [Window] -> EnemyId
getEnemyId [] = error "invalid window"
getEnemyId (Window _ (PlacedDoom (EnemyTarget eid) _) : _) = eid
getEnemyId (_ : xs) = getEnemyId xs

instance RunMessage CityOfTheUnseen where
  runMessage msg (CityOfTheUnseen (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemyId -> enemyId) payment
      -> do
        pushAll
          [ PlaceDoom (EnemyTarget enemyId) 1
          , UseCardAbilityChoice
            iid
            (toSource attrs)
            1
            NoAbilityMetadata
            []
            payment
          ]
        pure . CityOfTheUnseen $ attrs `with` Metadata True
    UseCardAbilityChoice _ (isSource attrs -> True) _ _ _ _ -> do
      pure . CityOfTheUnseen $ attrs `with` Metadata False
    _ -> CityOfTheUnseen . (`with` meta) <$> runMessage msg attrs
