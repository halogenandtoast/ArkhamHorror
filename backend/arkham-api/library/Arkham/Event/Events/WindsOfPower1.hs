module Arkham.Event.Events.WindsOfPower1 (windsOfPower1, WindsOfPower1 (..)) where

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)

newtype WindsOfPower1 = WindsOfPower1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

windsOfPower1 :: EventCard WindsOfPower1
windsOfPower1 = event WindsOfPower1 Cards.windsOfPower1

instance HasAbilities WindsOfPower1 where
  getAbilities (WindsOfPower1 x) =
    [ restrictedAbility
        x
        1
        ( InYourHand
            <> DuringTurn You
            <> exists (AssetControlledBy You <> AssetCanHaveUses Charge)
            <> PlayableCardExists (UnpaidCost NoAction) (basic $ CardWithId $ toCardId x)
        )
        $ freeReaction
        $ DrawCard #after You (basic $ CardWithId $ toCardId x) AnyDeck
    ]

instance RunMessage WindsOfPower1 where
  runMessage msg e@(WindsOfPower1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      assets <- select $ assetControlledBy iid <> AssetCanHaveUses Charge
      chooseOne iid [targetLabel asset [AddUses (toSource attrs) asset Charge 2] | asset <- assets]
      pure e
    InHand iid' (UseCardAbility iid (isSource attrs -> True) 1 _ _) | iid' == iid -> do
      playCardPayingCost iid (toCard attrs)
      pure e
    _ -> WindsOfPower1 <$> liftRunMessage msg attrs
