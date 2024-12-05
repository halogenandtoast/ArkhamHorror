module Arkham.Event.Events.SpectralRazor (spectralRazor, spectralRazorEffect, SpectralRazor (..)) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Message (chooseEngageEnemy)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestTarget)
import Arkham.Matcher

newtype SpectralRazor = SpectralRazor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRazor :: EventCard SpectralRazor
spectralRazor = event SpectralRazor Cards.spectralRazor

instance RunMessage SpectralRazor where
  runMessage msg e@(SpectralRazor attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      fightableEnemies <- select $ CanFightEnemy (toSource attrs)
      engageableEnemies <- select $ CanEngageEnemy (toSource attrs)

      case (fightableEnemies, engageableEnemies) of
        ([], []) -> error "invalid call"
        ([], _ : _) -> pushAll [chooseEngageEnemy iid attrs, DoStep 1 msg]
        (_ : _, []) -> doStep 1 msg
        (_ : _, _ : _) -> do
          chooseOneM iid do
            labeled "Engage an enemy first" do
              push $ chooseEngageEnemy iid attrs
              doStep 1 msg
            labeled "Do not engage an enemy" $ doStep 1 msg
      pure e
    DoStep 1 (PlayThisEvent iid eid) | eid == toId attrs -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #willpower)
      createCardEffect Cards.spectralRazor Nothing attrs iid
      chooseFightEnemy sid iid attrs
      pure e
    _ -> SpectralRazor <$> liftRunMessage msg attrs

newtype SpectralRazorEffect = SpectralRazorEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRazorEffect :: EffectArgs -> SpectralRazorEffect
spectralRazorEffect = cardEffect SpectralRazorEffect Cards.spectralRazor

instance HasModifiersFor SpectralRazorEffect where
  getModifiersFor (SpectralRazorEffect a) = maybeModified_ a a.target do
    EnemyTarget eid <- MaybeT $ getSkillTestTarget
    elite <- lift $ eid <=~> EliteEnemy
    pure [DamageDealt $ if elite then 1 else 2]

instance RunMessage SpectralRazorEffect where
  runMessage msg e@(SpectralRazorEffect attrs) = runQueueT $ case msg of
    SkillTestEnds {} -> disableReturn e
    _ -> SpectralRazorEffect <$> liftRunMessage msg attrs
