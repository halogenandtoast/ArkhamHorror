module Arkham.Event.Events.SpectralRazor (spectralRazor, spectralRazorEffect, SpectralRazor (..)) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.CombatTarget
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
    {- The engage is offered before the attacked enemy is chosen because it is what
    makes an Aloof enemy nobody is engaged with attackable at all, so taking it aims
    the attack at that enemy: the card engages "the attacked enemy", never a second
    one. With no fightable and no engageable enemy the event resolves as nothing --
    its costs stay paid -- rather than erroring. -}
    PlayThisEvent iid eid | eid == toId attrs -> do
      canFight <- hasFightTargets (toSource attrs) iid
      engageable <- select $ CanEngageEnemy (toSource attrs)
      when (canFight || notNull engageable) do
        sid <- getRandom
        skillTestModifier sid attrs iid (AddSkillValue #willpower)
        createSkillTestCardEffect sid Cards.spectralRazor Nothing attrs iid
        fight <- capture $ chooseFightEnemy sid iid attrs
        engageThenFight <- capture $ chooseOneM iid $ targets engageable \enemy -> do
          engageEnemy iid enemy
          fightEnemy sid iid attrs enemy
        case (canFight, engageable) of
          (_, []) -> pushAll fight
          (False, _) -> pushAll engageThenFight
          (True, _) -> chooseOneM iid do
            labeledI "engageEnemyFirst" $ pushAll engageThenFight
            labeledI "doNotEngageEnemy" $ pushAll fight
      pure e
    _ -> SpectralRazor <$> liftRunMessage msg attrs

newtype SpectralRazorEffect = SpectralRazorEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRazorEffect :: EffectArgs -> SpectralRazorEffect
spectralRazorEffect = cardEffect SpectralRazorEffect Cards.spectralRazor

instance HasModifiersFor SpectralRazorEffect where
  getModifiersFor (SpectralRazorEffect a) = maybeModified_ a a.target do
    EnemyTarget eid <- MaybeT getSkillTestTarget
    elite <- lift $ eid <=~> EliteEnemy
    pure [DamageDealt $ if elite then 1 else 2]

instance RunMessage SpectralRazorEffect where
  runMessage msg (SpectralRazorEffect attrs) = runQueueT $ SpectralRazorEffect <$> liftRunMessage msg attrs
