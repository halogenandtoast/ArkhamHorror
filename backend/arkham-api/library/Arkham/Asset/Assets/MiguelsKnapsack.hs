module Arkham.Asset.Assets.MiguelsKnapsack (miguelsKnapsack) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card.CardType
import Arkham.ForMovement
import Arkham.Helpers.Card (passesLimits)
import Arkham.Helpers.Location (getConnectedLocations, withLocationOf)
import Arkham.Helpers.Modifiers (modified_)
import Arkham.Helpers.Window (getPlayedEvent)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype MiguelsKnapsack = MiguelsKnapsack AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miguelsKnapsack :: AssetCard MiguelsKnapsack
miguelsKnapsack = asset MiguelsKnapsack Cards.miguelsKnapsack

instance HasModifiersFor MiguelsKnapsack where
  getModifiersFor (MiguelsKnapsack a) = unless a.exhausted do
    for_ a.controller \iid ->
      modified_
        a
        iid
        [ CanModify
            ( CanPlayAtLocation
                (CardWithType EventType)
                (ConnectedFrom NotForMovement (locationWithInvestigator iid))
            )
        ]

instance HasAbilities MiguelsKnapsack where
  getAbilities (MiguelsKnapsack a) =
    [controlled_ a 1 $ triggered (PlayEvent #when You AnyEvent) (exhaust a)]

instance RunMessage MiguelsKnapsack where
  runMessage msg a@(MiguelsKnapsack attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getPlayedEvent -> eid) _ -> do
      withLocationOf iid \lid -> do
        connectingLids <- getConnectedLocations lid
        card <- fetchCard eid
        playableAtCurrentLocation <- passesLimits iid card
        let
          playAtConnectingLocation :: ReverseQueue m => m ()
          playAtConnectingLocation = chooseOrRunOneM iid do
            targets connectingLids \lid' ->
              cardResolutionModifiers card (attrs.ability 1) iid [AsIfAt lid']
        if not playableAtCurrentLocation && notNull connectingLids
          then playAtConnectingLocation
          else chooseOneM iid do
            (withI18n $ countVar 1 $ labeled' "drawCards") do
              drawCards iid (attrs.ability 1) 1
            when (notNull connectingLids) do
              (cardI18n $ labeled' "miguelsKnapsack.playThatEventAtAConnectingLocation") playAtConnectingLocation
      pure a
    _ -> MiguelsKnapsack <$> liftRunMessage msg attrs
