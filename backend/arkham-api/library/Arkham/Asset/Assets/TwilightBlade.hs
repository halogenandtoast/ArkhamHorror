module Arkham.Asset.Assets.TwilightBlade (twilightBlade, TwilightBlade (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Fight
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Projection

newtype TwilightBlade = TwilightBlade AssetAttrs
  deriving anyclass IsAsset
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
          toModifiers a [AdditionalCost (ExhaustCost $ toTarget a) | isAffected]
        _ -> pure []
    _ -> pure []
  getModifiersFor (InvestigatorTarget iid) (TwilightBlade a) | a `controlledBy` iid = do
    underDiana <- field InvestigatorCardsUnderneath iid
    let eventsAndSkills = filter (`cardMatch` (CardWithOneOf [CardWithType EventType, CardWithType SkillType])) underDiana
    toModifiers a $ map AsIfInHand eventsAndSkills
  getModifiersFor _ _ = pure []

instance HasAbilities TwilightBlade where
  getAbilities (TwilightBlade a) = [restricted a 1 ControlsThis fightAction_]

instance RunMessage TwilightBlade where
  runMessage msg a@(TwilightBlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- genId
      chooseOneM iid do
        labeled "Use {willpower}" $ chooseFightEnemyEdit sid iid source (withSkillType #willpower)
        labeled "Use {combat}" $ chooseFightEnemy sid iid source
      pure a
    InitiatePlayCard iid card _ _ _ _ | controlledBy attrs iid -> do
      underDiana <- field InvestigatorCardsUnderneath iid
      when (card `elem` underDiana) do
        exhaustThis attrs
        cardResolutionModifier card attrs iid $ CannotTriggerAbilityMatching $ AbilityIs (toSource iid) 1
        addToHand iid [card]
        push msg
      pure a
    _ -> TwilightBlade <$> liftRunMessage msg attrs
