module Arkham.Event.Cards.EtherealWeaving3 (etherealWeaving3, EtherealWeaving3 (..)) where

import Arkham.Card
import Arkham.Cost.Status
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getIsPlayable)
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Modifiers (ModifierType (..), toModifier, withModifiers)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Window (defaultWindows)

newtype Meta = Meta {chosenEvents :: [CardId]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EtherealWeaving3 = EtherealWeaving3 (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealWeaving3 :: EventCard EtherealWeaving3
etherealWeaving3 = event (EtherealWeaving3 . (`with` Meta [])) Cards.etherealWeaving3

instance RunMessage EtherealWeaving3 where
  runMessage msg e@(EtherealWeaving3 (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      cards <- select $ inHandOf iid <> #event <> #spell
      focusCards cards \unfocus -> do
        chooseUpToN
          iid
          3
          "Done revealing cards"
          [targetLabel card [handleTargetChoice iid attrs card] | card <- cards]
        push unfocus
      doStep 1 msg
      pure e
    HandleTargetChoice _iid (isSource attrs -> True) (CardIdTarget cid) -> do
      push $ RevealCard cid
      pure . EtherealWeaving3 $ attrs `with` Meta (cid : chosenEvents meta)
    DoStep 1 (PlayThisEvent iid (is attrs -> True)) -> do
      cards <- traverse getCard (chosenEvents meta)
      playable <-
        withModifiers
          iid
          [toModifier GameSource $ ReduceCostOf (oneOf $ map (CardWithId . toCardId) cards) 1]
          $ filterM (getIsPlayable iid attrs (UnpaidCost NoAction) (defaultWindows iid)) cards
      when (notNull playable) do
        chooseOneM iid do
          for_ playable \card -> do
            targeting card do
              eventModifier attrs iid (AnySkillValue 2)
              costModifier attrs iid (ReduceCostOf (CardWithId card.id) 1)
              push $ PayCardCost iid card (defaultWindows iid)
      pure e
    _ -> EtherealWeaving3 . (`with` meta) <$> liftRunMessage msg attrs
