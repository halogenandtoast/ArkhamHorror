module Arkham.Asset.Assets.AgentAriQuinnFoundationResearcher (agentAriQuinnFoundationResearcher) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Matcher

newtype AgentAriQuinnFoundationResearcher = AgentAriQuinnFoundationResearcher AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

agentAriQuinnFoundationResearcher :: AssetCard AgentAriQuinnFoundationResearcher
agentAriQuinnFoundationResearcher = asset AgentAriQuinnFoundationResearcher Cards.agentAriQuinnFoundationResearcher

instance HasModifiersFor AgentAriQuinnFoundationResearcher where
  getModifiersFor (AgentAriQuinnFoundationResearcher a) = for_ a.controller \iid -> do
    hasConcealed <- selectAny $ locationWithInvestigator iid <> LocationWithConcealedCard
    modifiedWhen_ a hasConcealed iid $ map (`SkillModifier` 1) [#intellect, #combat, #agility]

instance HasAbilities AgentAriQuinnFoundationResearcher where
  getAbilities (AgentAriQuinnFoundationResearcher a) =
    [ controlled a 1 (youExist can.draw.cards)
        $ triggered
          (mapOneOf (CampaignEvent #when (Just You)) ["exposed[enemy]", "exposed[location]"])
          (exhaust a)
    ]

instance RunMessage AgentAriQuinnFoundationResearcher where
  runMessage msg a@(AgentAriQuinnFoundationResearcher attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> AgentAriQuinnFoundationResearcher <$> liftRunMessage msg attrs
