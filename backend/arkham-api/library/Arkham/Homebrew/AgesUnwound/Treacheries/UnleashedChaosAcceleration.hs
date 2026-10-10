module Arkham.Homebrew.AgesUnwound.Treacheries.UnleashedChaosAcceleration (unleashedChaosAcceleration) where

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.RequestedChaosTokenStrategy
import Arkham.Treachery.Import.Lifted hiding (EnemyAttacks)

newtype UnleashedChaosAcceleration = UnleashedChaosAcceleration TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unleashedChaosAcceleration :: TreacheryCard UnleashedChaosAcceleration
unleashedChaosAcceleration = treachery UnleashedChaosAcceleration Cards.unleashedChaosIAccelerationI

{- | "Forced - After attached enemy attacks: Reveal a chaos token. If a [skull],
[cultist], [tablet], [elder_thing] or [auto_fail] is revealed, the attached enemy
attacks again. (Limit once per round.)"
-}
instance HasAbilities UnleashedChaosAcceleration where
  getAbilities (UnleashedChaosAcceleration a) = case a.attached.enemy of
    Nothing -> []
    Just eid ->
      [ limited (MaxPer Cards.unleashedChaosIAccelerationI PerRound 1)
          $ mkAbility a 1
          $ forced
          $ EnemyAttacks #after You AnyEnemyAttack (EnemyWithId eid)
      ]

{- | "Revelation - Draw the top card of the encounter deck. If it is an enemy,
attach Unleashed Chaos (Acceleration) to it. Otherwise, gain 1 action and
Unleashed Chaos (Acceleration) gains surge."
-}
instance RunMessage UnleashedChaosAcceleration where
  runMessage msg t@(UnleashedChaosAcceleration attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- peek before drawing: the attach needs the identity of the card that
      -- resolves, and only the card id survives the draw
      mcard <- headMay <$> getEncounterDeck
      case mcard of
        Nothing -> gainSurge attrs
        Just card -> do
          drawEncounterCard iid attrs
          if cardMatch card (#enemy :: CardMatcher)
            then push $ HandleTargetChoice iid (toSource attrs) (CardIdTarget $ toCardId card)
            else do
              gainActions iid attrs 1
              gainSurge attrs
      pure t
    HandleTargetChoice _ (isSource attrs -> True) (CardIdTarget cid) -> do
      selectOne (EnemyWithCardId cid) >>= \case
        Just eid -> attachTreachery attrs eid
        Nothing -> toDiscard attrs attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ RequestChaosTokens (attrs.ability 1) (Just iid) (Reveal 1) SetAside
      push $ ResetChaosTokens (attrs.ability 1)
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      when (any ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . (.face)) tokens) do
        for_ attrs.attached.enemy \eid -> initiateEnemyAttack eid (attrs.ability 1) iid
      pure t
    _ -> UnleashedChaosAcceleration <$> liftRunMessage msg attrs
