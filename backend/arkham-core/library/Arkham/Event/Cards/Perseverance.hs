module Arkham.Event.Cards.Perseverance
  ( perseverance
  , Perseverance(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types
import Arkham.Message
import Arkham.Projection
import Arkham.Target

newtype Perseverance = Perseverance EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perseverance :: EventCard Perseverance
perseverance = event Perseverance Cards.perseverance

instance RunMessage Perseverance where
  runMessage msg e@(Perseverance attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      replaceMessageMatching
        (\case
          InvestigatorWhenDefeated _ iid' -> iid == iid'
          _ -> False
        )
        (\case
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
          ([ ("Damage", (0, assignedDamage)) | assignedDamage > 0 ]
          <> [ ("Horror", (0, assignedHorror)) | assignedHorror > 0 ]
          )
          (toTarget attrs)
        , Discard (toTarget attrs)
        ]
      pure e
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let
        choicesMap = mapFromList @(HashMap Text Int) choices
        damageAmount = findWithDefault 0 "Damage" choicesMap
        horrorAmount = findWithDefault 0 "Horror" choicesMap
      push $ CancelAssignedDamage
        (InvestigatorTarget iid)
        damageAmount
        horrorAmount
      pure e
    _ -> Perseverance <$> runMessage msg attrs
