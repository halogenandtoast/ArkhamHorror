module Arkham.Asset.Assets.Mk1Grenades4 (mk1Grenades4) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.DamageEffect
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Mk1Grenades4 = Mk1Grenades4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mk1Grenades4 :: AssetCard Mk1Grenades4
mk1Grenades4 = assetWith Mk1Grenades4 Cards.mk1Grenades4 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities Mk1Grenades4 where
  getAbilities (Mk1Grenades4 a) = [controlled_ a 1 $ fightAction (assetUseCost a Supply 1)]

instance RunMessage Mk1Grenades4 where
  runMessage msg a@(Mk1Grenades4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid source iid (SkillModifier #combat 2)
      chooseFightEnemyEdit sid iid source $ setTarget attrs
      pure a
    Successful (Action.Fight, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      let source = attrs.ability 1
      iids <- select $ colocatedWith iid <> NotInvestigator (InvestigatorWithId iid)
      eids <- select $ enemyAtLocationWith iid
      let
        handleOptions :: ReverseQueue n => ChooseT n () -> n ()
        handleOptions body = do
          chooseOneAtATimeM iid do
            targets eids \eid' ->
              push
                $ EnemyDamage eid'
                $ delayDamage
                $ if eid == eid' then attack source 2 else isDirect (attack source 2)
            targets iids \iid' -> assignDamage iid' source 2
            body
          for_ eids (checkDefeated source)

      concealed <- getConcealedIds (ForExpose $ toSource attrs) iid
      if null concealed
        then handleOptions (pure ())
        else chooseOneM iid do
          labeled "Apply damage to concealed" $ handleOptions do
            targets concealed $ exposeConcealed iid source
          labeled "Do not apply damage to concealed" $ handleOptions (pure ())
      pure a
    _ -> Mk1Grenades4 <$> liftRunMessage msg attrs
