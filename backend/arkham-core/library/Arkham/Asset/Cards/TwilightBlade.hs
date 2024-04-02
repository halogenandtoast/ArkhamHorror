module Arkham.Asset.Cards.TwilightBlade (twilightBlade, twilightBladeEffect, TwilightBlade (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillType

newtype TwilightBlade = TwilightBlade AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twilightBlade :: AssetCard TwilightBlade
twilightBlade = asset TwilightBlade Cards.twilightBlade

instance HasModifiersFor TwilightBlade where
  getModifiersFor (CardIdTarget cid) (TwilightBlade a) = case assetPlacement a of
    InPlayArea iid -> do
      underDiana <- field InvestigatorCardsUnderneath iid
      case find ((== cid) . toCardId) underDiana of
        Just card -> do
          let isAffected = card `cardMatch` CardWithOneOf [CardWithType EventType, CardWithType SkillType]
          pure $ toModifiers a [AdditionalCost (ExhaustCost $ toTarget a) | isAffected]
        _ -> pure []
    _ -> pure []
  getModifiersFor (InvestigatorTarget iid) (TwilightBlade a) | a `controlledBy` iid = do
    underDiana <- field InvestigatorCardsUnderneath iid
    let eventsAndSkills = filter (`cardMatch` (CardWithOneOf [CardWithType EventType, CardWithType SkillType])) underDiana
    pure $ toModifiers a $ map AsIfInHand eventsAndSkills
  getModifiersFor _ _ = pure []

instance HasAbilities TwilightBlade where
  getAbilities (TwilightBlade a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage TwilightBlade where
  runMessage msg a@(TwilightBlade attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let source = attrs.ability 1
      chooseFight <- toMessage <$> mkChooseFight iid source
      pushAll
        [ createCardEffect Cards.twilightBlade Nothing source iid
        , chooseFight
        ]
      pure a
    _ -> TwilightBlade <$> runMessage msg attrs

newtype TwilightBladeEffect = TwilightBladeEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twilightBladeEffect :: EffectArgs -> TwilightBladeEffect
twilightBladeEffect = cardEffect TwilightBladeEffect Cards.twilightBlade

instance HasModifiersFor TwilightBladeEffect where
  getModifiersFor target (TwilightBladeEffect a) | target == a.target = do
    pure $ toModifiers a [UseSkillInPlaceOf SkillCombat SkillWillpower]
  getModifiersFor _ _ = pure []

instance RunMessage TwilightBladeEffect where
  runMessage msg e@(TwilightBladeEffect attrs) = case msg of
    SkillTestEnds _ _ -> e <$ push (disable attrs)
    _ -> TwilightBladeEffect <$> runMessage msg attrs
