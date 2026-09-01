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
    -- the destination is chosen before costs are paid, so playing this can be
    -- judged (attacks of opportunity included) knowing where you are going
    BeforePlayEvent iid eid acId | eid == toId attrs -> do
      locations <- getCanMoveToMatchingLocations iid attrs RevealedLocation
      unless (null locations) do
        chooseTargetM iid locations \lid -> push $ UpdateEventTarget eid (Just $ toTarget lid)
      push $ CreatedCost acId
      pure e
    PlayThisEvent iid eid | eid == toId attrs -> do
      for_ attrs.target \case
        LocationTarget lid -> push $ MoveAction iid lid Free False
        _ -> pure ()
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
