module Arkham.Event.Cards.CrackTheCase (crackTheCase, CrackTheCase (..)) where

import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Helpers.Query (getPlayer)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection

newtype CrackTheCase = CrackTheCase EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crackTheCase :: EventCard CrackTheCase
crackTheCase = event CrackTheCase Cards.crackTheCase

instance RunMessage CrackTheCase where
  runMessage msg e@(CrackTheCase attrs) = case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      lid <- getJustLocation iid
      iids <- select $ affectsOthers $ investigatorAt lid <> can.gain.resources
      shroud <- fieldMap LocationShroud (fromMaybe 0) lid
      player <- getPlayer iid
      pushAll
        $ replicate shroud
        $ Msg.chooseOrRunOne
          player
          [ ResourceLabel iid' [TakeResources iid' 1 (toSource attrs) False]
          | iid' <- iids
          ]
      pure e
    _ -> CrackTheCase <$> runMessage msg attrs
