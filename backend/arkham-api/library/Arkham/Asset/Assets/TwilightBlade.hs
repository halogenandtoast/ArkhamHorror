module Arkham.Asset.Assets.TwilightBlade (twilightBlade, TwilightBlade (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Fight
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifyEach)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype TwilightBlade = TwilightBlade AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twilightBlade :: AssetCard TwilightBlade
twilightBlade = asset TwilightBlade Cards.twilightBlade

instance HasModifiersFor TwilightBlade where
  getModifiersFor (TwilightBlade a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      underDiana <- filterCards (CardWithOneOf [#event, #skill]) <$> field InvestigatorCardsUnderneath iid
      cards <- modifyEach a underDiana [AdditionalCost (ExhaustCost $ toTarget a)]
      controller <- modified_ a iid $ map AsIfInHand underDiana
      pure $ cards <> controller

instance HasAbilities TwilightBlade where
  getAbilities (TwilightBlade a) = [restricted a 1 ControlsThis fightAction_]

instance RunMessage TwilightBlade where
  runMessage msg a@(TwilightBlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
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
