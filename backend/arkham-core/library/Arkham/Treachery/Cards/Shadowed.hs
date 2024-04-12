module Arkham.Treachery.Cards.Shadowed (
  shadowed,
  Shadowed (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Types
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner hiding (EnemyFight)

newtype Shadowed = Shadowed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowed :: TreacheryCard Shadowed
shadowed = treachery Shadowed Cards.shadowed

instance RunMessage Shadowed where
  runMessage msg t@(Shadowed attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      cultists <- select (NearestEnemyTo iid $ EnemyWithTrait Cultist <> EnemyWithFight)
      if null cultists
        then
          pushAll
            [ InvestigatorAssignDamage iid source DamageAny 0 1
            , gainSurge attrs
            ]
        else do
          player <- getPlayer iid
          push
            $ chooseOrRunOne
              player
              [ targetLabel
                cultist
                [ PlaceDoom (toSource attrs) (toTarget cultist) 1
                , RevelationSkillTest iid source SkillWillpower (EnemyMaybeFieldDifficulty cultist EnemyFight)
                ]
              | cultist <- cultists
              ]
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ | isSource attrs source -> do
      push $ InvestigatorAssignDamage iid source DamageAny 0 2
      pure t
    _ -> Shadowed <$> runMessage msg attrs
