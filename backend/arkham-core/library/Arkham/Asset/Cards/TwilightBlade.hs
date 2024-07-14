module Arkham.Asset.Cards.TwilightBlade (twilightBlade, TwilightBlade (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
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
      chooseFight <- mkChooseFight iid source
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Use Willpower" [toMessage $ withSkillType #willpower chooseFight]
          , Label "Use Combat" [toMessage chooseFight]
          ]
      pure a
    _ -> TwilightBlade <$> runMessage msg attrs
