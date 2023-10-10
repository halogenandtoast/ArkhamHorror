module Arkham.Event.Cards.CrackTheCase (
  crackTheCase,
  CrackTheCase (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype CrackTheCase = CrackTheCase EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crackTheCase :: EventCard CrackTheCase
crackTheCase = event CrackTheCase Cards.crackTheCase

instance RunMessage CrackTheCase where
  runMessage msg e@(CrackTheCase attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      lid <- getJustLocation iid
      iids <- selectList $ investigatorAt lid <> InvestigatorCanGainResources
      shroud <- field LocationShroud lid
      player <- getPlayer iid
      pushAll
        $ replicate shroud
        $ chooseOrRunOne
          player
          [ ComponentLabel
            (InvestigatorComponent iid' ResourceToken)
            [TakeResources iid' 1 (toSource attrs) False]
          | iid' <- iids
          ]
      pure e
    _ -> CrackTheCase <$> runMessage msg attrs
