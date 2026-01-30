module Arkham.Asset.Assets.Machete (machete) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (
  getSkillTestInvestigator,
  getSkillTestTargetedEnemy,
  isSkillTestSource,
 )
import Arkham.Matcher
import Arkham.Card.CardCode
import Arkham.Message.Lifted.Choose

newtype Machete = Machete AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machete :: AssetCard Machete
machete = asset Machete Cards.machete

instance HasModifiersFor Machete where
  getModifiersFor (Machete a) = for_ a.controller \iid -> do
    when (toCardCode a /= "12020") do
      maybeModified_ a iid do
        liftGuardM $ isSkillTestSource (a.ability 1)
        eid <- MaybeT getSkillTestTargetedEnemy
        liftGuardM $ matches eid (onlyEnemyEngagedWith iid)
        guardM $ (== iid) <$> MaybeT getSkillTestInvestigator
        pure [DamageDealt 1]

instance HasAbilities Machete where
  getAbilities (Machete a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage Machete where
  runMessage msg a@(Machete attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 1)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    ChoseEnemy sid iid (isAbilitySource attrs 1 -> True) enemy -> do
      when (toCardCode attrs == "12020" && attrs.ready) do
        whenM (matches enemy (onlyEnemyEngagedWith iid)) do
          chooseOneM iid do
            labeled "Do not exhaust Machete" $ pure ()
            labeled "Exhaust Machete" do
              exhaustThis attrs
              skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
      pure a
    _ -> Machete <$> liftRunMessage msg attrs

-- module Arkham.Asset.Assets.Machete (machete) where
--
-- import Arkham.Ability.Scripted.Builder
-- import Arkham.Asset.Cards qualified as Cards
-- import Arkham.Asset.Import.Lifted
-- import Arkham.Effect.Builder
-- import Arkham.Card
-- import Arkham.Matcher
--
-- newtype Machete = Machete AssetAttrs
--   deriving anyclass (IsAsset, HasModifiersFor)
--   deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Targetable, Sourceable, HasCardCode)
--   deriving HasAbilities via Scripted Machete
--   deriving RunMessage via Scripted Machete
--
-- machete :: AssetCard Machete
-- machete = asset Machete Cards.machete
--
-- instance ScriptedAbilities Machete where
--   scriptedAbilities = abilities do
--     fightAction $ fight $ effect you do
--       combat (Fixed 1)
--       damageDealt 1 `whenAttackedEnemy` onlyEnemyEngagedWith you
