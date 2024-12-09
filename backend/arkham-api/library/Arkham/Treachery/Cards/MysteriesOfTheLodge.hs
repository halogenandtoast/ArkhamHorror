module Arkham.Treachery.Cards.MysteriesOfTheLodge (
  mysteriesOfTheLodge,
  mysteriesOfTheLodgeEffect,
  MysteriesOfTheLodge (..),
) where

import Arkham.Action qualified as Action
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestAction, getSkillTestTarget)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MysteriesOfTheLodge = MysteriesOfTheLodge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriesOfTheLodge :: TreacheryCard MysteriesOfTheLodge
mysteriesOfTheLodge = treachery MysteriesOfTheLodge Cards.mysteriesOfTheLodge

instance RunMessage MysteriesOfTheLodge where
  runMessage msg t@(MysteriesOfTheLodge attrs) = runQueueT $ case msg of
    Revelation iid source | isSource attrs source -> do
      enemies <-
        select $ NearestEnemyTo iid $ EnemyWithTrait Cultist <> EnemyWithoutModifier CannotPlaceDoomOnThis
      case enemies of
        [] -> gainSurge attrs
        xs -> do
          chooseTargetM iid xs \eid -> do
            placeDoom attrs eid 1
            createCardEffect Cards.mysteriesOfTheLodge Nothing source (EnemyTarget eid)
      pure t
    _ -> MysteriesOfTheLodge <$> liftRunMessage msg attrs

newtype MysteriesOfTheLodgeEffect = MysteriesOfTheLodgeEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mysteriesOfTheLodgeEffect :: EffectArgs -> MysteriesOfTheLodgeEffect
mysteriesOfTheLodgeEffect = cardEffect MysteriesOfTheLodgeEffect Cards.mysteriesOfTheLodge

instance HasModifiersFor MysteriesOfTheLodgeEffect where
  getModifiersFor (MysteriesOfTheLodgeEffect a) =
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> maybeModified_ a (SkillTestTarget st.id) do
        target <- MaybeT getSkillTestTarget
        guard $ target == a.target
        action <- MaybeT getSkillTestAction
        guard $ action `elem` [Action.Fight, Action.Evade, Action.Parley]
        pure [Difficulty 2]

instance RunMessage MysteriesOfTheLodgeEffect where
  runMessage msg e@(MysteriesOfTheLodgeEffect attrs) = runQueueT $ case msg of
    EndRound -> disableReturn e
    _ -> MysteriesOfTheLodgeEffect <$> liftRunMessage msg attrs
