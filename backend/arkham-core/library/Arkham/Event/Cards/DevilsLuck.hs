module Arkham.Event.Cards.DevilsLuck
  ( devilsLuck
  , DevilsLuck(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Helpers.Window
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype DevilsLuck = DevilsLuck EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsLuck :: EventCard DevilsLuck
devilsLuck = event DevilsLuck Cards.devilsLuck

instance RunMessage DevilsLuck where
  runMessage msg e@(DevilsLuck attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [Window _ (Window.WouldTakeDamageOrHorror _ _ damage horror)] _
      | eid == toId attrs
      -> do
        pushAll
          [ chooseAmounts
            iid
            "Amount of Damage/Horror to cancel"
            (MaxAmountTarget 10)
            ([ ("Damage", (0, damage)) | damage > 0 ]
            <> [ ("Horror", (0, horror)) | horror > 0 ]
            )
            (toTarget attrs)
          , discard attrs
          ]
        pure e
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let
        choicesMap = mapFromList @(HashMap Text Int) choices
        damageAmount = findWithDefault 0 "Damage" choicesMap
        horrorAmount = findWithDefault 0 "Horror" choicesMap
      ignoreWindow <- checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      pushAll
        $ [ CancelDamage iid damageAmount | damageAmount > 0 ]
        <> [ CancelHorror iid horrorAmount | horrorAmount > 0 ]
        <> [ ignoreWindow | damageAmount + horrorAmount > 0 ]
      pure e
    _ -> DevilsLuck <$> runMessage msg attrs
