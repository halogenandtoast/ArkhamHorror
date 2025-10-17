module Arkham.Treachery.Cards.InPlainSight (inPlainSight) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InPlainSight = InPlainSight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inPlainSight :: TreacheryCard InPlainSight
inPlainSight = treachery InPlainSight Cards.inPlainSight

instance HasAbilities InPlainSight where
  getAbilities (InPlainSight a) = case a.attached.enemy of
    Just eid -> [mkAbility a 1 $ forced $ CampaignEvent #after (Just You) ("exposed[" <> tshow eid <> "]")]
    Nothing -> []

instance RunMessage InPlainSight where
  runMessage msg t@(InPlainSight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <-
        select
          $ EnemyWithPlacement InTheShadows
          <> not_ (EnemyWithAttachedTreachery $ treacheryIs Cards.inPlainSight)
      if null enemies
        then gainSurge attrs
        else chooseTargetM iid enemies $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached.enemy \eid -> do
        roundModifier (attrs.ability 1) eid (EnemyFight 2)
        initiateEnemyAttack eid (attrs.ability 1) iid
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> InPlainSight <$> liftRunMessage msg attrs
