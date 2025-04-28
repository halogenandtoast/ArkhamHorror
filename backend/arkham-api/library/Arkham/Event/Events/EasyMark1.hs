module Arkham.Event.Events.EasyMark1 (easyMark1) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype EasyMark1 = EasyMark1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easyMark1 :: EventCard EasyMark1
easyMark1 = event EasyMark1 Cards.easyMark1

instance HasAbilities EasyMark1 where
  getAbilities (EasyMark1 a) =
    [ controlled a 1 (youExist $ handWith Cards.easyMark1)
        $ freeReaction (Arkham.Matcher.PlayEventDiscarding #after You (be a))
    ]

instance RunMessage EasyMark1 where
  runMessage msg e@(EasyMark1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      gainResources iid attrs 2
      drawCards iid attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mcard <- selectOne $ inHandOf ForPlay iid <> basic (cardIs Cards.easyMark1)
      for_ mcard $ putCardIntoPlay iid
      pure e
    _ -> EasyMark1 <$> liftRunMessage msg attrs
