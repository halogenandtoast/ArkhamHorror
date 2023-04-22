module Arkham.Asset.Cards.IchtacaTheForgottenGuardian
  ( ichtacaTheForgottenGuardian
  , IchtacaTheForgottenGuardian(..)
  ) where

import Arkham.Prelude

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
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype IchtacaTheForgottenGuardian = IchtacaTheForgottenGuardian AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ichtacaTheForgottenGuardian :: AssetCard IchtacaTheForgottenGuardian
ichtacaTheForgottenGuardian =
  ally IchtacaTheForgottenGuardian Cards.ichtacaTheForgottenGuardian (3, 2)

instance HasModifiersFor IchtacaTheForgottenGuardian where
  getModifiersFor (InvestigatorTarget iid) (IchtacaTheForgottenGuardian a)
    | controlledBy a iid = do
      mSkillTestTarget <- getSkillTestTarget
      case mSkillTestTarget of
        Just (EnemyTarget eid) -> do
          mAction <- getSkillTestAction
          case mAction of
            Just Action.Fight -> do
              combatValue <- maybe 1 (const 2) <$> getVictoryPoints eid
              pure $ toModifiers a [SkillModifier SkillCombat combatValue]
            Just Action.Evade -> do
              agilityValue <- maybe 1 (const 2)
                <$> getVengeancePoints eid
              pure $ toModifiers a [SkillModifier SkillAgility agilityValue]
            _ -> pure []
        _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities IchtacaTheForgottenGuardian where
  getAbilities (IchtacaTheForgottenGuardian a) = case assetPlacement a of
    InPlayArea iid ->
      [ restrictedAbility
            a
            1
            (ControlsThis <> AnyCriterion
              [ InvestigatorExists
                (InvestigatorWithId iid <> InvestigatorWithAnyHorror)
              , AssetExists (AssetWithId (toId a) <> AssetWithHorror)
              ]
            )
          $ ReactionAbility
              (AddedToVictory Timing.After $ CardWithType EnemyType)
          $ ExhaustCost (toTarget a)
      ]
    _ -> []

instance RunMessage IchtacaTheForgottenGuardian where
  runMessage msg a@(IchtacaTheForgottenGuardian attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mHealHorror <- getHealHorrorMessage attrs 1 iid
      ichtacaCanHealHorror <-
        selectAny $ HealableAsset (toSource attrs) HorrorType $ AssetWithId
          (toId attrs)
      pushAll
        $ maybeToList mHealHorror
        <> [ HealHorror (toTarget attrs) (toSource attrs) 1
           | ichtacaCanHealHorror
           ]
      pure a
    _ -> IchtacaTheForgottenGuardian <$> runMessage msg attrs
