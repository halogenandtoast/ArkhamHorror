module Arkham.Treachery.Cards.PulledByTheStars (
  pulledByTheStars,
  PulledByTheStars (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Source
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
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ ForcedAbility
        $ TurnEnds #when (You <> NotInvestigator InvestigatorThatMovedDuringTurn)
    , restrictedAbility a 2 OnSameLocation actionAbility
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
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ assignHorror iid (toAbilitySource attrs 1) 2
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ beginSkillTest iid (toAbilitySource attrs 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      push $ toDiscardBy iid attrs attrs
      pure t
    _ -> PulledByTheStars <$> runMessage msg attrs
