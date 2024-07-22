module Arkham.Event.Cards.ActOfDesperation (
  actOfDesperation,
  actOfDesperationEffect,
  ActOfDesperation (..),
) where

import Arkham.Classes
import Arkham.Cost
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Modifier

newtype ActOfDesperation = ActOfDesperation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

actOfDesperation :: EventCard ActOfDesperation
actOfDesperation = event ActOfDesperation Cards.actOfDesperation

instance RunMessage ActOfDesperation where
  runMessage msg e@(ActOfDesperation attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      case attrs.payment.discards of
        [(zone, discard)] -> do
          let n = discard.printedCost
          sid <- getRandom
          skillTestModifiers sid attrs iid $ DamageDealt 1 : [SkillModifier #combat n | n > 0]
          when (zone == FromPlay && n > 0) $ createCardEffect Cards.actOfDesperation (effectInt n) attrs iid
          pushM $ mkChooseFight sid iid attrs
        _ -> error $ "Invalid choice: " <> show attrs.payment
      pure e
    _ -> ActOfDesperation <$> liftRunMessage msg attrs

newtype ActOfDesperationEffect = ActOfDesperationEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

actOfDesperationEffect :: EffectArgs -> ActOfDesperationEffect
actOfDesperationEffect = cardEffect ActOfDesperationEffect Cards.actOfDesperation

instance RunMessage ActOfDesperationEffect where
  runMessage msg e@(ActOfDesperationEffect attrs) = runQueueT $ case msg of
    PassedThisSkillTest _ (isSource attrs.source -> True) -> do
      case (attrs.meta, attrs.target) of
        (Just (EffectInt n), InvestigatorTarget iid) -> do
          gainResourcesIfCan iid attrs.source n
          disableReturn e
        _ -> error "Invalid call"
    SkillTestEnds _ _ _ -> disableReturn e
    _ -> ActOfDesperationEffect <$> liftRunMessage msg attrs
