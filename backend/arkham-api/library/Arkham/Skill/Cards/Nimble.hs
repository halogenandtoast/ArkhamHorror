module Arkham.Skill.Cards.Nimble (nimble, nimbleEffect) where

import Arkham.Effect.Import
import Arkham.Helpers.Location
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Import.Lifted

newtype Nimble = Nimble SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nimble :: SkillCard Nimble
nimble = skill Nimble Cards.nimble

instance RunMessage Nimble where
  runMessage msg s@(Nimble attrs) = runQueueT $ case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ (min 3 -> n) | n > 0 -> do
      afterSkillTest attrs.owner "Nimble" $ createCardEffect Cards.nimble (effectInt n) attrs attrs.owner
      pure s
    _ -> Nimble <$> liftRunMessage msg attrs

newtype NimbleEffect = NimbleEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nimbleEffect :: EffectArgs -> NimbleEffect
nimbleEffect = cardEffect NimbleEffect Cards.nimble

instance RunMessage NimbleEffect where
  runMessage msg e@(NimbleEffect attrs) = runQueueT $ case msg of
    CreatedEffect eid _ _ _ | eid == attrs.id -> do
      for_ attrs.metadata \meta -> for_ meta.int (`doStep` msg)
      disable attrs
      pure e
    DoStep n msg'@(CreatedEffect eid _ _ _) | eid == attrs.id && n > 0 -> do
      for_ attrs.target.investigator \iid -> do
        connectingLocations <- getAccessibleLocations iid attrs
        chooseOrRunOneM iid $ withI18n do
          labeled' "doNotMove" nothing
          targets connectingLocations \loc -> do
            moveTo attrs iid loc
            doStep (n - 1) msg'
      pure e
    _ -> NimbleEffect <$> liftRunMessage msg attrs
