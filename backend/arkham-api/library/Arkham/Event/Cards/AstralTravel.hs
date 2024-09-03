module Arkham.Event.Cards.AstralTravel (astralTravel, AstralTravel (..)) where

import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher hiding (MoveAction)
import Arkham.RequestedChaosTokenStrategy
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
      chooseOne iid [targetLabel lid [MoveAction iid lid Free False] | lid <- locations]
      push $ RequestChaosTokens (toSource attrs) Nothing (Reveal 1) SetAside
      pure e
    RequestedChaosTokens (isSource attrs -> True) _ tokens -> do
      let faces = [Skull, Cultist, Tablet, ElderThing, AutoFail]
      when (any ((`elem` faces) . chaosTokenFace) tokens) do
        assets <- select $ oneOf $ AssetWithTrait <$> [Trait.Item, Trait.Ally]
        player <- getPlayer attrs.owner
        push
          $ If (Window.RevealChaosTokenEventEffect attrs.owner tokens attrs.id)
          $ case assets of
            [] -> [Msg.assignDamage attrs.owner attrs 1]
            xs -> [Msg.chooseOne player $ targetLabels xs $ only . Msg.toDiscardBy attrs.controller attrs]
      push $ ResetChaosTokens (toSource attrs)
      pure e
    _ -> AstralTravel <$> liftRunMessage msg attrs
