module Arkham.Event.Cards.EatLead (
  eatLead,
  EatLead (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window

newtype Metadata = Metadata {asset :: Maybe AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EatLead = EatLead (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eatLead :: EventCard EatLead
eatLead = event (EatLead . (`With` Metadata Nothing)) Cards.eatLead

instance RunMessage EatLead where
  runMessage msg (EatLead (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ [(windowType -> Window.ActivateAbility _ ability)] _ | eid == toId attrs -> do
      case abilitySource ability of
        AssetSource aid -> do
          ignoreWindow <-
            checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
          pushAll
            [ SpendUses (AssetTarget aid) Ammo 1
            , skillTestModifier attrs iid (ChangeRevealStrategy $ RevealAndChoose 1 1)
            , ignoreWindow
            ]
          pure . EatLead $ attrs `with` Metadata (Just aid)
        _ -> error "Invalid source"
    _ -> EatLead . (`with` metadata) <$> runMessage msg attrs
