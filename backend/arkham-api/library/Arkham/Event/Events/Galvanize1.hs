module Arkham.Event.Events.Galvanize1 (galvanize1, Galvanize1 (..)) where

import Arkham.Action.Additional
import Arkham.ClassSymbol
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype Galvanize1 = Galvanize1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

galvanize1 :: EventCard Galvanize1
galvanize1 = event Galvanize1 Cards.galvanize1

instance RunMessage Galvanize1 where
  runMessage msg e@(Galvanize1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      assets <- select $ assetControlledBy iid <> AssetExhausted <> AssetWithClass Guardian
      chooseOrRunOneM iid $ targets assets ready
      turnModifier iid attrs iid
        $ GiveAdditionalAction
        $ AdditionalAction "Galvanize" (toSource attrs) #fight
      pure e
    _ -> Galvanize1 <$> liftRunMessage msg attrs
