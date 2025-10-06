module Arkham.Event.Events.ManipulateDestiny2 (manipulateDestiny2) where

import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype ManipulateDestiny2 = ManipulateDestiny2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manipulateDestiny2 :: EventCard ManipulateDestiny2
manipulateDestiny2 = event ManipulateDestiny2 Cards.manipulateDestiny2

instance RunMessage ManipulateDestiny2 where
  runMessage msg e@(ManipulateDestiny2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      requestChaosTokens iid attrs 1
      resetChaosTokens (toSource attrs)
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      if any ((`elem` [#curse, #autofail, #bless, #eldersign]) . (.face)) tokens
        then do
          enemies <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (toSource attrs)
          mconcealed <- getConcealed iid
          damageInvestigators <- select $ HealableInvestigator (toSource attrs) #damage $ colocatedWith iid
          damageAssets <-
            select
              $ HealableAsset (toSource attrs) #damage
              $ at_ (locationWithInvestigator iid)
              <> AssetControlledBy (affectsOthers Anyone)

          let canDamage = any ((`elem` [#curse, #autofail]) . (.face)) tokens && (notNull enemies || isJust mconcealed)
          let canHeal =
                any ((`elem` [#bless, #eldersign]) . (.face)) tokens
                  && (notNull damageInvestigators || notNull damageAssets)

          chooseOrRunOneAtATimeM iid do
            when canDamage do
              labeled "Deal 2 damage to an enemy at your location" do
                chooseDamageEnemy iid attrs (locationWithInvestigator iid) AnyEnemy 2
            when canHeal do
              labeled "Heal 2 damage from an investigator or Ally asset at your location" do
                chooseOneM iid do
                  for_ damageInvestigators \i -> damageLabeled i $ healDamage i attrs 2
                  for_ damageAssets \a -> assetDamageLabeled a $ healDamage a attrs 2
        else requestChaosTokens iid attrs 1
      pure e
    _ -> ManipulateDestiny2 <$> liftRunMessage msg attrs
