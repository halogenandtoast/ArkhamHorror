module Arkham.Treachery.Cards.PulledByTheStars (
  pulledByTheStars,
  PulledByTheStars (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PulledByTheStars = PulledByTheStars TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pulledByTheStars :: TreacheryCard PulledByTheStars
pulledByTheStars = treachery PulledByTheStars Cards.pulledByTheStars

instance HasAbilities PulledByTheStars where
  getAbilities (PulledByTheStars a) =
    [ restrictedAbility a 1 (InThreatAreaOf You) $
        ForcedAbility $
          TurnEnds Timing.When (You <> NotInvestigator InvestigatorThatMovedDuringTurn)
    , restrictedAbility a 2 OnSameLocation $ ActionAbility Nothing $ ActionCost 1
    ]

instance HasModifiersFor PulledByTheStars where
  getModifiersFor SkillTestTarget (PulledByTheStars attrs) = do
    mSource <- getSkillTestSource
    mInvestigator <- getSkillTestInvestigator
    case (mSource, mInvestigator) of
      (Just source, Just iid) | isAbilitySource attrs 2 source -> do
        exhaustedWitch <-
          selectAny $ ExhaustedEnemy <> EnemyWithTrait Witch <> EnemyAt (locationWithInvestigator iid)
        pure $ toModifiers attrs [SkillTestAutomaticallySucceeds | exhaustedWitch]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage PulledByTheStars where
  runMessage msg t@(PulledByTheStars attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ AttachTreachery (toId attrs) (toTarget iid)
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 2
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ beginSkillTest iid (toAbilitySource attrs 2) iid SkillWillpower 3
      pure t
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ Discard (toSource attrs) (toTarget attrs)
      pure t
    _ -> PulledByTheStars <$> runMessage msg attrs
