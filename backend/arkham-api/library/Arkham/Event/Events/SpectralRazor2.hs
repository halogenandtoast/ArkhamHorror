module Arkham.Event.Events.SpectralRazor2 (spectralRazor2, spectralRazor2Effect, SpectralRazor2 (..)) where

import Arkham.Card
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.CombatTarget
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher

newtype SpectralRazor2 = SpectralRazor2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRazor2 :: EventCard SpectralRazor2
spectralRazor2 = event SpectralRazor2 Cards.spectralRazor2

instance RunMessage SpectralRazor2 where
  runMessage msg e@(SpectralRazor2 attrs) = runQueueT $ case msg of
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
        createSkillTestCardEffect sid Cards.spectralRazor2 Nothing attrs iid
        onRevealChaosTokenEffect sid IsSymbol attrs attrs do
          atEndOfTurn attrs iid $ addToHand iid (only $ toCard attrs)
        fight <- capture $ pushM $ mkChooseFight sid iid attrs
        engageThenFight <- capture $ chooseOneM iid $ targets engageable \enemy -> do
          engageEnemy iid enemy
          pushM $ mkFightEnemy sid iid attrs enemy
        case (canFight, engageable) of
          (_, []) -> pushAll fight
          (False, _) -> pushAll engageThenFight
          (True, _) -> chooseOneM iid do
            labeledI "engageEnemyFirst" $ pushAll engageThenFight
            labeledI "doNotEngageEnemy" $ pushAll fight
      pure e
    _ -> SpectralRazor2 <$> liftRunMessage msg attrs

newtype SpectralRazor2Effect = SpectralRazor2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRazor2Effect :: EffectArgs -> SpectralRazor2Effect
spectralRazor2Effect = cardEffect SpectralRazor2Effect Cards.spectralRazor2

instance HasModifiersFor SpectralRazor2Effect where
  getModifiersFor (SpectralRazor2Effect a) = maybeModified_ a a.target do
    EnemyTarget eid <- MaybeT getSkillTestTarget
    elite <- lift $ eid <=~> EliteEnemy
    pure [DamageDealt $ if elite then 1 else 2]

instance RunMessage SpectralRazor2Effect where
  runMessage msg (SpectralRazor2Effect attrs) = runQueueT $ SpectralRazor2Effect <$> liftRunMessage msg attrs
