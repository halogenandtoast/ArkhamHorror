module Arkham.Event.Cards.Perseverance (
  perseverance,
  Perseverance (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Window
import Arkham.Investigator.Types
import Arkham.Message
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Perseverance = Perseverance EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perseverance :: EventCard Perseverance
perseverance = event Perseverance Cards.perseverance

instance RunMessage Perseverance where
  runMessage msg e@(Perseverance attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      replaceMessageMatching
        ( \case
            InvestigatorWhenDefeated _ iid' -> iid == iid'
            _ -> False
        )
        ( \case
            InvestigatorWhenDefeated source' _ -> [CheckDefeated source']
            _ -> error "invalid match"
        )
      assignedDamage <- field InvestigatorAssignedDamage iid
      assignedHorror <- field InvestigatorAssignedHorror iid
      pushAll
        [ chooseAmounts
            iid
            "Cancel up to 4 damage and or horror"
            (MaxAmountTarget 4)
            ( [("Damage", (0, assignedDamage)) | assignedDamage > 0]
                <> [("Horror", (0, assignedHorror)) | assignedHorror > 0]
            )
            (toTarget attrs)
        ]
      pure e
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let
        damageAmount = getChoiceAmount "Damage" choices
        horrorAmount = getChoiceAmount "Horror" choices
      ignoreWindow <-
        checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      pushAll $
        CancelAssignedDamage
          (InvestigatorTarget iid)
          damageAmount
          horrorAmount
          : [ignoreWindow | damageAmount + horrorAmount > 0]
      pure e
    _ -> Perseverance <$> runMessage msg attrs
