module Arkham.Location.Cards.Office (
  office,
  Office (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ChaosToken
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner hiding (RevealChaosToken)
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (SilverTwilight))

newtype Office = Office LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

office :: LocationCard Office
office = location Office Cards.office 4 (PerPlayer 1)

instance HasModifiersFor Office where
  getModifiersFor (EnemyTarget eid) (Office a) = do
    atOffice <- eid <=~> enemyAt (toId a)
    pure $ toModifiers a [RemoveKeyword Keyword.Aloof | atOffice]
  getModifiersFor _ _ = pure []

instance HasAbilities Office where
  getAbilities (Office a) =
    withBaseAbilities
      a
      [ restrictedAbility
          a
          1
          ( DuringSkillTest (WhileInvestigating $ LocationWithId $ toId a)
              <> EnemyCriteria
                ( EnemyExists
                    $ EnemyWithTrait SilverTwilight
                    <> EnemyWithoutModifier CannotPlaceDoomOnThis
                )
          )
          $ ForcedAbility
          $ RevealChaosToken Timing.After You
          $ ChaosTokenMatchesAny
          $ map ChaosTokenFaceIs [Skull, Cultist, Tablet, ElderThing, AutoFail]
      ]

instance RunMessage Office where
  runMessage msg l@(Office attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemies <-
        select
          $ NearestEnemyToLocation
            (toId attrs)
            ( EnemyWithTrait SilverTwilight
                <> EnemyWithoutModifier CannotPlaceDoomOnThis
            )
      unless (null enemies) $ do
        player <- getPlayer iid
        push
          $ chooseOrRunOne
            player
            [ targetLabel enemy [PlaceDoom (toAbilitySource attrs 1) (toTarget enemy) 1]
            | enemy <- enemies
            ]
      pure l
    _ -> Office <$> runMessage msg attrs
