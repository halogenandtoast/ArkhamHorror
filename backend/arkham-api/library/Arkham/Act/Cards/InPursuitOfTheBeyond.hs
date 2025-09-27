module Arkham.Act.Cards.InPursuitOfTheBeyond (inPursuitOfTheBeyond) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Matcher
import Arkham.Message.Story
import Arkham.Window (Window (..), getBatchId)
import Arkham.Window qualified as Window

newtype InPursuitOfTheBeyond = InPursuitOfTheBeyond ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities InPursuitOfTheBeyond where
  getAbilities = actAbilities \a ->
    [mkAbility a 1 $ freeReaction $ ScenarioEvent #when (Just You) "wouldBanish"]

inPursuitOfTheBeyond :: ActCard InPursuitOfTheBeyond
inPursuitOfTheBeyond = act (2, A) InPursuitOfTheBeyond Cards.inPursuitOfTheBeyond Nothing

getHeretic :: [Window] -> Card
getHeretic = \case
  [] -> error "missing heretic"
  ((windowType -> Window.ScenarioEvent "wouldBanish" _ value) : _) -> toResult value
  (_ : xs) -> getHeretic xs

instance RunMessage InPursuitOfTheBeyond where
  runMessage msg a@(InPursuitOfTheBeyond attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R3
      pure a
    UseCardAbility _iid (isSource attrs -> True) 1 (getBatchId &&& getHeretic -> (batchId, heretic)) _ -> do
      cancelBatch batchId
      obtainCard heretic
      selectEach (StoryWithCardId heretic.id) (push . StoryMessage . RemoveStory)
      erynn <- selectJust $ assetIs Assets.erynnMacAoidhDevotedEnchantress
      placeUnderneath erynn [heretic]
      pure a
    _ -> InPursuitOfTheBeyond <$> liftRunMessage msg attrs
