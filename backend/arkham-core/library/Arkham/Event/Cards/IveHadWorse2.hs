module Arkham.Event.Cards.IveHadWorse2 (
  iveHadWorse2,
  IveHadWorse2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Window
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype IveHadWorse2 = IveHadWorse2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveHadWorse2 :: EventCard IveHadWorse2
iveHadWorse2 = event IveHadWorse2 Cards.iveHadWorse2

dropUntilDamage :: [Message] -> [Message]
dropUntilDamage = dropWhile (notElem DamageMessage . messageType)

instance RunMessage IveHadWorse2 where
  runMessage msg e@(IveHadWorse2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      (damage, horror) <- fromQueue $ \queue -> case dropUntilDamage queue of
        dmsg : _ ->
          case dmsg of
            InvestigatorDamage iid' _ damage' horror' ->
              if iid' == iid then (damage', horror') else error "mismatch"
            InvestigatorDoAssignDamage iid' _ _ _ damage' horror' _ _ ->
              if iid' == iid then (damage', horror') else error "mismatch"
            _ -> error "mismatch"
        _ -> error "unhandled"
      pushAll
        [ chooseAmounts
            iid
            "Amount of Damage/Horror to cancel"
            (MaxAmountTarget 2)
            ( [("Damage", (0, damage)) | damage > 0]
                <> [("Horror", (0, horror)) | horror > 0]
            )
            (toTarget attrs)
        ]
      pure e
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let
        damageAmount = getChoiceAmount "Damage" choices
        horrorAmount = getChoiceAmount "Horror" choices
      ignoreWindow <-
        checkWindows [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      pushAll
        $ [CancelDamage iid damageAmount | damageAmount > 0]
        <> [CancelHorror iid horrorAmount | horrorAmount > 0]
        <> [TakeResources iid (damageAmount + horrorAmount) (toSource attrs) False]
        <> [ignoreWindow | damageAmount + horrorAmount > 0]
      pure e
    _ -> IveHadWorse2 <$> runMessage msg attrs
