module Arkham.Event.Events.IllTakeThat (illTakeThat) where

import Arkham.Asset.Types (Field (..))
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.Window (getPassedBy)
import Arkham.Matcher hiding (AssetCard)
import Arkham.Projection
import Arkham.Trait (Trait (Illicit))

newtype IllTakeThat = IllTakeThat EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illTakeThat :: EventCard IllTakeThat
illTakeThat = event IllTakeThat Cards.illTakeThat

instance HasModifiersFor IllTakeThat where
  getModifiersFor (IllTakeThat attrs) = case attrs.attachedTo.asset of
    Just target -> do
      modified_ attrs target [AddTrait Illicit]
      whenJustM (fieldMay AssetCard target) \c -> modified_ attrs c [AddTrait Illicit]
    _ -> case attrs.target of
      -- Before the event attaches, treat the chosen card as Illicit so it can
      -- enter play in an Illicit-only slot (e.g. Hidden Pocket's adjustable slot)
      Just (CardIdTarget cid) -> modified_ attrs cid [AddTrait Illicit]
      _ -> pure mempty

instance RunMessage IllTakeThat where
  runMessage msg e@(IllTakeThat attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let n = getPassedBy attrs.windows
      items <-
        select $ PlayableCardWithCostReduction NoAction n $ inHandOf ForPlay iid <> basic (#item <> #asset)
      focusCards items do
        chooseTargetM iid items \item -> do
          unfocusCards
          handleTarget iid attrs item
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (CardIdTarget cid) -> do
      let n = getPassedBy attrs.windows
      c <- getCard cid
      reduceCostOf attrs c n
      playCardPayingCost iid c
      doStep 1 msg
      -- Remember the chosen card so it is treated as Illicit while entering play
      pure $ IllTakeThat $ attrs {eventTarget = Just (CardIdTarget cid)}
    DoStep 1 (HandleTargetChoice _iid (isSource attrs -> True) (CardIdTarget cid)) -> do
      selectOne (AssetWithCardId cid) >>= traverse_ (place attrs)
      pure e
    _ -> IllTakeThat <$> liftRunMessage msg attrs
