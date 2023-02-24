module Arkham.Event.Cards.EatLead2
  ( eatLead2
  , EatLead2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types ( Field (AssetUses) )
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.ChaosBag.RevealStrategy
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id
import Arkham.EffectMetadata
import Arkham.Effect.Window
import Arkham.Message
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window
import Arkham.Projection
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype Metadata = Metadata { asset :: Maybe AssetId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EatLead2 = EatLead2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eatLead2 :: EventCard EatLead2
eatLead2 = event (EatLead2 . (`With` Metadata Nothing)) Cards.eatLead2

instance RunMessage EatLead2 where
  runMessage msg e@(EatLead2 (attrs `With` metadata)) = case msg of
    InvestigatorPlayEvent iid eid _ [Window _ (Window.ActivateAbility _ ability)] _
      | eid == toId attrs
      -> do
        case abilitySource ability of
          AssetSource aid -> do
            uses <- fieldMap AssetUses useCount aid
            pushAll
              [ chooseAmounts
                iid
                "Additional ammo to spend"
                (MaxAmountTarget uses)
                [("Ammo", (0, uses))]
                (toTarget attrs)
              ]
            pure . EatLead2 $ attrs `with` Metadata (Just aid)
          _ -> error "Invalid source"
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let
        aid = fromJustNote "asset must be set" (asset metadata)
        choicesMap = mapFromList @(HashMap Text Int) choices
        ammo = findWithDefault 0 "Ammo" choicesMap
      when (ammo > 0) $ do
        ignoreWindow <- checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
        pushAll
          [ SpendUses (AssetTarget aid) Ammo ammo
          , CreateWindowModifierEffect
            EffectSkillTestWindow
            (EffectModifiers $ toModifiers
              attrs
              [ChangeRevealStrategy $ RevealAndChoose ammo 1]
            )
            (toSource attrs)
            (InvestigatorTarget iid)
          , ignoreWindow
          ]
      pure e
    _ -> EatLead2 . (`with` metadata) <$> runMessage msg attrs
