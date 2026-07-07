module Arkham.Event.Events.ImDoneRunnin (imDoneRunnin, imDoneRunninEffect) where

import Arkham.Capability
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.I18n
import Arkham.Matcher hiding (EnemyEvaded)

newtype ImDoneRunnin = ImDoneRunnin EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

imDoneRunnin :: EventCard ImDoneRunnin
imDoneRunnin = event ImDoneRunnin Cards.imDoneRunnin

instance RunMessage ImDoneRunnin where
  runMessage msg e@(ImDoneRunnin attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ EnemyAt (locationWithInvestigator iid)
      for_ enemies readyThis
      for_ enemies (engageEnemy iid)
      createCardEffect Cards.imDoneRunnin Nothing (toSource attrs) (InvestigatorTarget iid)
      pure e
    _ -> ImDoneRunnin <$> liftRunMessage msg attrs

newtype ImDoneRunninEffect = ImDoneRunninEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

imDoneRunninEffect :: EffectArgs -> ImDoneRunninEffect
imDoneRunninEffect = cardEffect ImDoneRunninEffect Cards.imDoneRunnin

instance HasModifiersFor ImDoneRunninEffect where
  getModifiersFor (ImDoneRunninEffect a) = modified_ a a.target [DoNotExhaustEvaded, DoNotDisengageEvaded]

instance RunMessage ImDoneRunninEffect where
  runMessage msg e@(ImDoneRunninEffect attrs) = runQueueT $ case msg of
    EnemyEvaded iid enemy | InvestigatorTarget iid == attrs.target -> do
      canDamage <-
        andM
          [ enemy <=~> EnemyCanBeDamagedBySource attrs.source
          , can.deal.damage iid
          ]
      let
        normalEvade :: ReverseQueue m => m ()
        normalEvade = successfulEvasion DisengageAndExhaust enemy
      if canDamage
        then do
          chooseOneM iid $ withI18n do
            labeled' "doNotDamageEnemy" normalEvade
            labeled' "damageEnemy" $ nonAttackEnemyDamage (Just iid) attrs.source 1 enemy
        else normalEvade
      pure e
    EndTurn _ -> disableReturn e
    _ -> ImDoneRunninEffect <$> liftRunMessage msg attrs
