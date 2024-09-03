module Arkham.Event.Cards.SpectralRazor2 (spectralRazor2, spectralRazor2Effect, SpectralRazor2 (..)) where

import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Strategy

newtype SpectralRazor2 = SpectralRazor2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRazor2 :: EventCard SpectralRazor2
spectralRazor2 = event SpectralRazor2 Cards.spectralRazor2

instance RunMessage SpectralRazor2 where
  runMessage msg e@(SpectralRazor2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      fightableEnemies <- select $ CanFightEnemy (toSource attrs)
      engageableEnemies <- select $ CanEngageEnemy (toSource attrs)

      case (fightableEnemies, engageableEnemies) of
        ([], []) -> error "invalid call"
        ([], _ : _) -> do
          push $ Msg.chooseEngageEnemy iid attrs
          doStep 1 msg
        (_ : _, []) -> doStep 1 msg
        (_ : _, _ : _) ->
          chooseOne
            iid
            [ Label "Engage an enemy first" [Msg.chooseEngageEnemy iid attrs, DoStep 1 msg]
            , Label "Do not engage an enemy" [DoStep 1 msg]
            ]
      pure e
    DoStep 1 (PlayThisEvent iid eid) | eid == toId attrs -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #willpower)
      createCardEffect Cards.spectralRazor2 Nothing attrs iid
      onRevealChaosTokenEffect sid IsSymbol attrs attrs do
        eventModifier attrs attrs (SetAfterPlay ReturnThisToHand)
      pushM $ mkChooseFight sid iid attrs
      pure e
    _ -> SpectralRazor2 <$> liftRunMessage msg attrs

newtype SpectralRazor2Effect = SpectralRazor2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spectralRazor2Effect :: EffectArgs -> SpectralRazor2Effect
spectralRazor2Effect = cardEffect SpectralRazor2Effect Cards.spectralRazor2

instance HasModifiersFor SpectralRazor2Effect where
  getModifiersFor target (SpectralRazor2Effect a) | a.target == target = maybeModified a do
    EnemyTarget eid <- MaybeT getSkillTestTarget
    elite <- lift $ eid <=~> EliteEnemy
    pure [DamageDealt $ if elite then 1 else 2]
  getModifiersFor _ _ = pure []

instance RunMessage SpectralRazor2Effect where
  runMessage msg e@(SpectralRazor2Effect attrs) = runQueueT $ case msg of
    SkillTestEnds {} -> disableReturn e
    _ -> SpectralRazor2Effect <$> liftRunMessage msg attrs
