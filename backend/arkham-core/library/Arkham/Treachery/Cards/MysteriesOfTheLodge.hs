module Arkham.Treachery.Cards.MysteriesOfTheLodge (
  mysteriesOfTheLodge,
  mysteriesOfTheLodgeEffect,
  MysteriesOfTheLodge (..),
) where

import Arkham.Prelude

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MysteriesOfTheLodge = MysteriesOfTheLodge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

mysteriesOfTheLodge :: TreacheryCard MysteriesOfTheLodge
mysteriesOfTheLodge = treachery MysteriesOfTheLodge Cards.mysteriesOfTheLodge

instance RunMessage MysteriesOfTheLodge where
  runMessage msg t@(MysteriesOfTheLodge attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      enemies <-
        selectList
          $ NearestEnemy
          $ EnemyWithTrait Cultist
          <> EnemyWithoutModifier CannotPlaceDoomOnThis
      case enemies of
        [] -> push $ gainSurge attrs
        xs -> do
          player <- getPlayer iid
          pushAll
            [ chooseOne
                player
                [ targetLabel
                  eid
                  [ PlaceDoom (toSource attrs) (EnemyTarget eid) 1
                  , createCardEffect
                      Cards.mysteriesOfTheLodge
                      Nothing
                      source
                      (EnemyTarget eid)
                  ]
                | eid <- xs
                ]
            ]
      pure t
    _ -> MysteriesOfTheLodge <$> runMessage msg attrs

newtype MysteriesOfTheLodgeEffect = MysteriesOfTheLodgeEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

mysteriesOfTheLodgeEffect :: EffectArgs -> MysteriesOfTheLodgeEffect
mysteriesOfTheLodgeEffect =
  cardEffect MysteriesOfTheLodgeEffect Cards.mysteriesOfTheLodge

instance HasModifiersFor MysteriesOfTheLodgeEffect where
  getModifiersFor SkillTestTarget (MysteriesOfTheLodgeEffect a) = do
    mTarget <- getSkillTestTarget
    mAction <- getSkillTestAction
    pure $ toModifiers a $ case (mTarget, mAction) of
      (Just target, Just action)
        | target == effectTarget a
        , action `elem` [Action.Fight, Action.Evade, Action.Parley] ->
            [Difficulty 2]
      _ -> []
  getModifiersFor _ _ = pure []

instance RunMessage MysteriesOfTheLodgeEffect where
  runMessage msg e@(MysteriesOfTheLodgeEffect attrs@EffectAttrs {..}) =
    case msg of
      EndRound -> do
        push (DisableEffect effectId)
        pure e
      _ -> MysteriesOfTheLodgeEffect <$> runMessage msg attrs
