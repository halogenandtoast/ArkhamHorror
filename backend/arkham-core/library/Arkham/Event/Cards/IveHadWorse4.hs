module Arkham.Event.Cards.IveHadWorse4 (
  iveHadWorse4,
  IveHadWorse4 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Window
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype IveHadWorse4 = IveHadWorse4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveHadWorse4 :: EventCard IveHadWorse4
iveHadWorse4 = event IveHadWorse4 Cards.iveHadWorse4

dropUntilDamage :: [Message] -> [Message]
dropUntilDamage = dropWhile (notElem DamageMessage . messageType)

instance RunMessage IveHadWorse4 where
  runMessage msg e@(IveHadWorse4 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      player <- getPlayer iid
      (damage, horror) <- fromQueue $ \queue -> case dropUntilDamage queue of
        dmsg : _ ->
          case dmsg of
            InvestigatorDamage iid' _ damage' horror' ->
              if iid' == iid then (damage', horror') else error "mismatch"
            InvestigatorDoAssignDamage iid' _ _ _ damage' horror' _ _ ->
              if iid' == iid then (damage', horror') else error "mismatch"
            _ -> error "mismatch"
        _ -> error "unhandled"
      push
        $ chooseAmounts
          player
          "Amount of Damage/Horror to cancel"
          (MaxAmountTarget 5)
          ( [("Damage", (0, damage)) | damage > 0]
              <> [("Horror", (0, horror)) | horror > 0]
          )
          (toTarget attrs)
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
    _ -> IveHadWorse4 <$> runMessage msg attrs
