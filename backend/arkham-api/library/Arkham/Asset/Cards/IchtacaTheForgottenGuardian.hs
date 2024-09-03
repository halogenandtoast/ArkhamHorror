module Arkham.Asset.Cards.IchtacaTheForgottenGuardian (
  ichtacaTheForgottenGuardian,
  IchtacaTheForgottenGuardian (..),
) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card.CardType
import Arkham.Damage
import Arkham.Helpers.Card
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype IchtacaTheForgottenGuardian = IchtacaTheForgottenGuardian AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaTheForgottenGuardian :: AssetCard IchtacaTheForgottenGuardian
ichtacaTheForgottenGuardian =
  ally IchtacaTheForgottenGuardian Cards.ichtacaTheForgottenGuardian (3, 2)

instance HasModifiersFor IchtacaTheForgottenGuardian where
  getModifiersFor (InvestigatorTarget iid) (IchtacaTheForgottenGuardian a) | controlledBy a iid = do
    maybeModified a do
      EnemyTarget eid <- MaybeT getSkillTestTarget
      action <- MaybeT getSkillTestAction
      case action of
        Action.Fight -> do
          combatValue <- lift $ maybe 1 (const 2) <$> getVictoryPoints eid
          pure [SkillModifier #combat combatValue]
        Action.Evade -> do
          agilityValue <- lift $ maybe 1 (const 2) <$> getVengeancePoints eid
          pure [SkillModifier #agility agilityValue]
        _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities IchtacaTheForgottenGuardian where
  getAbilities (IchtacaTheForgottenGuardian a) = case assetPlacement a of
    InPlayArea iid ->
      [ controlledAbility
          a
          1
          ( oneOf
              [ exists $ HealableInvestigator (a.ability 1) #horror $ InvestigatorWithId iid
              , exists $ HealableAsset (a.ability 1) #horror (be a)
              ]
          )
          $ ReactionAbility
            (AddedToVictory #after $ CardWithType EnemyType)
            (exhaust a)
      ]
    _ -> []

instance RunMessage IchtacaTheForgottenGuardian where
  runMessage msg a@(IchtacaTheForgottenGuardian attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      ichtacaCanHealHorror <-
        selectAny
          $ HealableAsset (toSource attrs) HorrorType
          $ AssetWithId
            (toId attrs)
      pushAll
        $ [HealHorror (toTarget iid) (attrs.ability 1) 1 | canHeal]
        <> [ HealHorror (toTarget attrs) (attrs.ability 1) 1
           | ichtacaCanHealHorror
           ]
      pure a
    _ -> IchtacaTheForgottenGuardian <$> runMessage msg attrs
