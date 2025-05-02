module Arkham.Asset.Assets.TwilightBlade (twilightBlade) where

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
  getModifiersFor (TwilightBlade a) = for_ a.controller \iid -> do
    underDiana <- filterCards (CardWithOneOf [#event, #skill]) <$> field InvestigatorCardsUnderneath iid
    modifyEach
      a
      underDiana
      [AdditionalCost (ExhaustCost $ toTarget a), AdditionalCostToCommit iid (ExhaustCost $ toTarget a)]
    modified_ a iid $ concatMap (\c -> [AsIfInHand c, CanCommitToSkillTestsAsIfInHand c]) underDiana

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
    InitiatePlayCard iid card _ _ ws _ | controlledBy attrs iid -> do
      underDiana <- field InvestigatorCardsUnderneath iid
      when (card `elem` underDiana) do
        cardResolutionModifier card attrs iid $ CannotTriggerAbilityMatching $ AbilityIs (toSource iid) 1
        playCardPayingCostWithWindows iid card ws
      pure a
    _ -> TwilightBlade <$> liftRunMessage msg attrs
