module Arkham.Asset.Assets.TwilightBlade (twilightBlade) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifyEach)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype TwilightBlade = TwilightBlade AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twilightBlade :: AssetCard TwilightBlade
twilightBlade = asset TwilightBlade Cards.twilightBlade

instance HasModifiersFor TwilightBlade where
  getModifiersFor (TwilightBlade a) = for_ a.controller \iid -> do
    underDiana <- filterCards (CardWithOneOf [#event, #skill]) <$> field InvestigatorCardsUnderneath iid
    modifyEach a underDiana [AdditionalCost (exhaust a), AdditionalCostToCommit iid (exhaust a)]
    modified_ a iid
      $ concatMap (\c -> [AsIfInHandFor ForPlay c.id, CanCommitToSkillTestsAsIfInHand c]) underDiana

instance HasAbilities TwilightBlade where
  getAbilities (TwilightBlade a) = [controlled_ a 1 $ fightActionWithAlternate_ #willpower]

instance RunMessage TwilightBlade where
  runMessage msg a@(TwilightBlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFightEnemyWithSkillChoice sid iid source [#combat, #willpower]
      pure a
    InitiatePlayCard iid card _ _ _ _ | controlledBy attrs iid -> do
      underDiana <- field InvestigatorCardsUnderneath iid
      when (card `elem` underDiana) do
        priority
          $ cardResolutionModifier card attrs iid
          $ CannotTriggerAbilityMatching
          $ AbilityIs (toSource iid) 1
      pure a
    _ -> TwilightBlade <$> liftRunMessage msg attrs
