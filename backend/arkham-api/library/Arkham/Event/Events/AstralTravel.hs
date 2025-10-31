module Arkham.Event.Events.AstralTravel (astralTravel) where

import Arkham.ChaosToken
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher hiding (MoveAction)
import Arkham.Trait qualified as Trait
import Arkham.Window qualified as Window

newtype AstralTravel = AstralTravel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

astralTravel :: EventCard AstralTravel
astralTravel = event AstralTravel Cards.astralTravel

instance RunMessage AstralTravel where
  runMessage msg e@(AstralTravel attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      locations <- getCanMoveToMatchingLocations iid attrs RevealedLocation
      chooseOneM iid $ targets locations \lid ->  push $ MoveAction iid lid Free False
      requestChaosTokens iid attrs 1
      pure e
    RequestedChaosTokens (isSource attrs -> True) (Just iid) tokens -> do
      let faces = [Skull, Cultist, Tablet, ElderThing, AutoFail]
      when (any ((`elem` faces) . chaosTokenFace) tokens) do
        assets <- select $ assetControlledBy iid <> mapOneOf AssetWithTrait [Trait.Item, Trait.Ally]
        player <- getPlayer iid
        push
          $ If (Window.RevealChaosTokenEventEffect attrs.owner tokens attrs.id)
          $ case assets of
            [] -> [Msg.assignDamage attrs.owner attrs 1]
            xs -> [Msg.chooseOne player $ targetLabels xs $ only . Msg.toDiscardBy attrs.controller attrs]
      resetChaosTokens attrs
      pure e
    _ -> AstralTravel <$> liftRunMessage msg attrs
