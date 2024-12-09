module Arkham.Asset.Assets.GildedVolto (gildedVolto, gildedVoltoEffect, GildedVolto (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTest

newtype GildedVolto = GildedVolto AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gildedVolto :: AssetCard GildedVolto
gildedVolto = asset GildedVolto Cards.gildedVolto

instance HasAbilities GildedVolto where
  getAbilities (GildedVolto a) =
    [ restrictedAbility a 1 ControlsThis $ freeReaction $ AssetEntersPlay #after (be a)
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility
          (InitiatedSkillTest #when You (NotSkillType #agility) AnySkillTestValue #any)
          (DiscardCost FromPlay $ toTarget a)
    ]

instance RunMessage GildedVolto where
  runMessage msg a@(GildedVolto attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push =<< createCardEffect Cards.gildedVolto Nothing (attrs.ability 1) iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      skillTest <- getJustSkillTest
      let
        newBase =
          case skillTestBaseValue skillTest of
            SkillBaseValue _ -> SkillBaseValue #agility
            AndSkillBaseValue _ -> SkillBaseValue #agility
            HalfResourcesOf x -> HalfResourcesOf x
            x@(FixedBaseValue _) -> x
      push $ ChangeSkillTestType (SkillSkillTest #agility) newBase
      pure a
    _ -> GildedVolto <$> runMessage msg attrs

newtype GildedVoltoEffect = GildedVoltoEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gildedVoltoEffect :: EffectArgs -> GildedVoltoEffect
gildedVoltoEffect = cardEffect GildedVoltoEffect Cards.gildedVolto

instance HasModifiersFor GildedVoltoEffect where
  getModifiersFor (GildedVoltoEffect a) = do
    case a.target of
      InvestigatorTarget iid -> do
        cards <- findAllCards (`cardMatch` (CardOwnedBy iid <> #asset))
        modifyEach a cards [BecomesFast FastPlayerWindow]
      _ -> modified_ a a.target [CanBecomeFast #asset]

instance RunMessage GildedVoltoEffect where
  runMessage msg e@(GildedVoltoEffect attrs) = case msg of
    CardEnteredPlay iid card | toTarget iid == attrs.target && cardMatch card (CardWithType AssetType) -> do
      push $ disable attrs
      pure e
    EndTurn _ -> do
      push $ disable attrs
      pure e
    _ -> GildedVoltoEffect <$> runMessage msg attrs
