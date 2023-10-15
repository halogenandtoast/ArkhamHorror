module Arkham.Event.Cards.EasyMark1 (
  easyMark1,
  EasyMark1 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype EasyMark1 = EasyMark1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easyMark1 :: EventCard EasyMark1
easyMark1 =
  event EasyMark1 Cards.easyMark1

instance HasAbilities EasyMark1 where
  getAbilities (EasyMark1 a) =
    [ controlledAbility a 1 (youExist $ handWith Cards.easyMark1)
        $ freeReaction (Arkham.Matcher.PlayEventDiscarding #after You $ EventWithId $ toId a)
    ]

instance RunMessage EasyMark1 where
  runMessage msg e@(EasyMark1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      mDrawCards <- drawCardsIfCan iid attrs 1
      gainResources <- can.gain.resources iid
      pushAll
        $ [takeResources iid attrs 2 | gainResources]
        <> toList mDrawCards
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mcard <- selectOne $ inHandOf iid <> basic (cardIs Cards.easyMark1)
      for_ mcard $ push . putCardIntoPlay iid
      pure e
    _ -> EasyMark1 <$> runMessage msg attrs
