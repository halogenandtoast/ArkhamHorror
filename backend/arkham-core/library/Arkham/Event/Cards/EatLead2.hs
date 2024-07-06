module Arkham.Event.Cards.EatLead2 (
  eatLead2,
  EatLead2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetUses))
import Arkham.Asset.Uses
import Arkham.ChaosBag.RevealStrategy
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window

newtype Metadata = Metadata {asset :: Maybe AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EatLead2 = EatLead2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eatLead2 :: EventCard EatLead2
eatLead2 = event (EatLead2 . (`With` Metadata Nothing)) Cards.eatLead2

instance RunMessage EatLead2 where
  runMessage msg e@(EatLead2 (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ [(windowType -> Window.ActivateAbility _ _ ability)] _ | eid == toId attrs -> do
      case abilitySource ability of
        AssetSource aid -> do
          uses <- fieldMap AssetUses (findWithDefault 0 Ammo) aid
          player <- getPlayer iid
          push
            $ chooseAmounts
              player
              "Additional ammo to spend"
              (MaxAmountTarget uses)
              [("Ammo", (0, uses))]
              (toTarget attrs)
          pure . EatLead2 $ attrs `with` Metadata (Just aid)
        _ -> error "Invalid source"
    ResolveAmounts iid (getChoiceAmount "Ammo" -> ammo) target | isTarget attrs target -> do
      let
        aid = fromJustNote "asset must be set" (asset metadata)
      when (ammo > 0) $ do
        ignoreWindow <-
          checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
        pushAll
          [ SpendUses (toSource attrs) (AssetTarget aid) Ammo ammo
          , skillTestModifier attrs iid (ChangeRevealStrategy $ RevealAndChoose ammo 1)
          , ignoreWindow
          ]
      pure e
    _ -> EatLead2 . (`with` metadata) <$> runMessage msg attrs
