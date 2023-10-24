module Arkham.Enemy.Cards.HotelGuest (
  hotelGuest,
  HotelGuest (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Trait (Trait (CrimeScene, Hall))

newtype HotelGuest = HotelGuest EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelGuest :: EnemyCard HotelGuest
hotelGuest =
  enemyWith
    HotelGuest
    Cards.hotelGuest
    (1, Static 1, 2)
    (1, 0)
    (spawnAtL ?~ SpawnAt (NearestLocationToYou $ LocationWithTrait Hall))

instance HasAbilities HotelGuest where
  getAbilities (HotelGuest a) =
    withBaseAbilities
      a
      [ restrictedAbility
          a
          1
          (exists $ EnemyWithId (toId a) <> EnemyAt (LocationWithTrait CrimeScene))
          $ ForcedAbility
          $ PhaseEnds #when #enemy
      , restrictedAbility a 2 OnSameLocation parleyAction_
      ]

instance RunMessage HotelGuest where
  runMessage msg e@(HotelGuest attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel skill [beginSkillTest iid (toAbilitySource attrs 2) attrs skill 3]
          | skill <- [#willpower, #intellect]
          ]
      pure e
    PassedThisSkillTest _ (isAbilitySource attrs 2 -> True) -> do
      push $ Discard (toAbilitySource attrs 2) (toTarget attrs)
      pure e
    _ -> HotelGuest <$> runMessage msg attrs
