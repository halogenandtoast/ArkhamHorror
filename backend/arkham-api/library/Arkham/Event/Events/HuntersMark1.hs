module Arkham.Event.Events.HuntersMark1 (huntersMark1) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Placement
import Arkham.Helpers.SkillTest
import Arkham.Matcher

newtype HuntersMark1 = HuntersMark1 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor HuntersMark1 where
  getModifiersFor (HuntersMark1 a) = for_ a.attachedTo.enemy \enemy -> do
    getSkillTestInvestigator >>= traverse_ \iid -> maybeModified_ a iid do
      liftGuardM $ isFightWith (EnemyWithId enemy)
      pure [SkillModifier #combat 1]

instance HasAbilities HuntersMark1 where
  getAbilities (HuntersMark1 a) =
    [ mkAbility a 1
        $ triggered
          ( EnemyDealtDamage
              #when
              AnyDamageEffect
              (EnemyWithAttachedEvent (EventWithId a.id) <> EnemyCanBeDamagedBySource (toSource a))
              AnySource
          )
          (discardCost a)
    ]

huntersMark1 :: EventCard HuntersMark1
huntersMark1 = event HuntersMark1 Cards.huntersMark1

instance RunMessage HuntersMark1 where
  runMessage msg e@(HuntersMark1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <-
        select $ enemyAtLocationWith iid <> not_ (EnemyWithAttachedEvent $ eventIs Cards.huntersMark1)
      chooseTargetM iid enemies $ place attrs . attachTo
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo.enemy (nonAttackEnemyDamage (Just iid) attrs 1)
      pure e
    _ -> HuntersMark1 <$> liftRunMessage msg attrs
