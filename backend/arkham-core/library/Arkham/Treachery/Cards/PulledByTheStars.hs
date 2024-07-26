module Arkham.Treachery.Cards.PulledByTheStars (pulledByTheStars, PulledByTheStars (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, getSkillTestSource)
import Arkham.Matcher
import Arkham.Source
import Arkham.Trait (Trait (Witch))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PulledByTheStars = PulledByTheStars TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pulledByTheStars :: TreacheryCard PulledByTheStars
pulledByTheStars = treachery PulledByTheStars Cards.pulledByTheStars

instance HasAbilities PulledByTheStars where
  getAbilities (PulledByTheStars a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ TurnEnds #when (You <> not_ InvestigatorThatMovedDuringTurn)
    , skillTestAbility $ restrictedAbility a 2 OnSameLocation actionAbility
    ]

instance HasModifiersFor PulledByTheStars where
  getModifiersFor (SkillTestTarget _) (PulledByTheStars attrs) = do
    mSource <- getSkillTestSource
    mInvestigator <- getSkillTestInvestigator
    case (mSource, mInvestigator) of
      (Just source, Just iid) | isAbilitySource attrs 2 source -> do
        exhaustedWitch <- selectAny $ ExhaustedEnemy <> EnemyWithTrait Witch <> enemyAtLocationWith iid
        pure $ toModifiers attrs [SkillTestAutomaticallySucceeds | exhaustedWitch]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage PulledByTheStars where
  runMessage msg t@(PulledByTheStars attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (toAbilitySource attrs 1) 2
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (toAbilitySource attrs 2) iid #willpower (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid attrs attrs
      pure t
    _ -> PulledByTheStars <$> liftRunMessage msg attrs
