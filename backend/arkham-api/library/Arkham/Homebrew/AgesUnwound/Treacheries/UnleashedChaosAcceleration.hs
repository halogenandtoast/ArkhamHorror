module Arkham.Homebrew.AgesUnwound.Treacheries.UnleashedChaosAcceleration (unleashedChaosAcceleration) where

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted hiding (EnemyAttacks)

newtype UnleashedChaosAcceleration = UnleashedChaosAcceleration TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unleashedChaosAcceleration :: TreacheryCard UnleashedChaosAcceleration
unleashedChaosAcceleration = treachery UnleashedChaosAcceleration Cards.unleashedChaosAcceleration

instance HasAbilities UnleashedChaosAcceleration where
  getAbilities (UnleashedChaosAcceleration a) = case a.attached.enemy of
    Nothing -> []
    Just eid ->
      [ limited (MaxPer Cards.unleashedChaosAcceleration PerRound 1)
          $ mkAbility a 1
          $ forced
          $ EnemyAttacks #after You AnyEnemyAttack (EnemyWithId eid)
      ]

instance RunMessage UnleashedChaosAcceleration where
  runMessage msg t@(UnleashedChaosAcceleration attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mcard <- headMay <$> getEncounterDeck
      case mcard of
        Nothing -> gainSurge attrs
        Just card -> do
          drawEncounterCard iid attrs
          if cardMatch card (card_ #enemy)
            then handleTarget iid attrs card
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
      requestChaosTokens iid (attrs.ability 1) 1
      pure t
    RequestedChaosTokens (isAbilitySource attrs 1 -> True) (Just iid) tokens -> do
      continue_ iid
      when (any ((`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) . (.face)) tokens) do
        for_ attrs.attached.enemy \eid -> initiateEnemyAttack eid (attrs.ability 1) iid
      pure t
    _ -> UnleashedChaosAcceleration <$> liftRunMessage msg attrs
