module Arkham.Asset.Assets.IchtacaTheForgottenGuardian (ichtacaTheForgottenGuardian) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card.CardType
import Arkham.Damage
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Placement

newtype IchtacaTheForgottenGuardian = IchtacaTheForgottenGuardian AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaTheForgottenGuardian :: AssetCard IchtacaTheForgottenGuardian
ichtacaTheForgottenGuardian = ally IchtacaTheForgottenGuardian Cards.ichtacaTheForgottenGuardian (3, 2)

instance HasModifiersFor IchtacaTheForgottenGuardian where
  getModifiersFor (IchtacaTheForgottenGuardian a) = controllerGetsMaybe a \_ -> do
    enemy <- MaybeT getSkillTestTargetedEnemy
    liftGuardM $ enemy <=~> InPlayEnemy (EnemyWithId enemy)
    MaybeT getSkillTestAction >>= \case
      Action.Fight -> do
        combat <- lift $ maybe 1 (const 2) <$> getVengeancePoints enemy
        pure [SkillModifier #combat combat]
      Action.Evade -> do
        agility <- lift $ maybe 1 (const 2) <$> getVengeancePoints enemy
        pure [SkillModifier #agility agility]
      _ -> pure mempty

instance HasAbilities IchtacaTheForgottenGuardian where
  getAbilities (IchtacaTheForgottenGuardian a) = case a.placement of
    InPlayArea iid ->
      [ controlled
          a
          1
          ( oneOf
              [ exists $ HealableInvestigator (a.ability 1) #horror $ InvestigatorWithId iid
              , exists $ HealableAsset (a.ability 1) #horror (be a)
              ]
          )
          $ triggered (AddedToVictory #after $ CardWithType EnemyType) (exhaust a)
      ]
    _ -> []

instance RunMessage IchtacaTheForgottenGuardian where
  runMessage msg a@(IchtacaTheForgottenGuardian attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenM (canHaveHorrorHealed (attrs.ability 1) iid) $ healHorror iid (attrs.ability 1) 1
      ichtacaCanHealHorror <- selectAny $ HealableAsset (toSource attrs) HorrorType (be attrs)
      when ichtacaCanHealHorror $ healHorror attrs (attrs.ability 1) 1
      pure a
    _ -> IchtacaTheForgottenGuardian <$> liftRunMessage msg attrs
