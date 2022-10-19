module Arkham.Act.Cards.DescentIntoDark
  ( DescentIntoDark(..)
  , descentIntoDark
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Criteria
import Arkham.Helpers.Log
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution

newtype DescentIntoDark = DescentIntoDark ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descentIntoDark :: ActCard DescentIntoDark
descentIntoDark = act (2, A) DescentIntoDark Cards.descentIntoDark Nothing

instance HasAbilities DescentIntoDark where
  getAbilities (DescentIntoDark a) =
    [ restrictedAbility
          a
          1
          (Negate
              (InvestigatorExists $ InvestigatorAt $ NotLocation $ locationIs
                Locations.descentToYoth
              )
          <> LocationExists
               (locationIs Locations.descentToYoth <> LocationWithoutDoom)
          )
        $ Objective
        $ FastAbility Free
    ]

instance RunMessage DescentIntoDark where
  runMessage msg a@(DescentIntoDark attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      rescuedAlejandro <- getHasRecord TheInvestigatorsRescuedAlejandro
      push $ ScenarioResolution $ Resolution $ if rescuedAlejandro then 1 else 2
      pure a
    _ -> DescentIntoDark <$> runMessage msg attrs
