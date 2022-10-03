module Arkham.Event.Cards.IveHadWorse4
  ( iveHadWorse4
  , IveHadWorse4(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Runner
import Arkham.Message

newtype IveHadWorse4 = IveHadWorse4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveHadWorse4 :: EventCard IveHadWorse4
iveHadWorse4 = event IveHadWorse4 Cards.iveHadWorse4

dropUntilDamage :: [Message] -> [Message]
dropUntilDamage = dropWhile (notElem DamageMessage . messageType)

instance RunMessage IveHadWorse4 where
  runMessage msg e@(IveHadWorse4 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows _ | eid == toId attrs -> do
      e <$ push (UseCardAbility iid (toSource attrs) 0 windows NoPayment)
    UseCardAbility _ source 5 _ _ | isSource attrs source ->
      e <$ push (Discard $ toTarget attrs)
    UseCardAbility iid source n windows pay | isSource attrs source -> do
      (damage, horror) <- fromQueue $ \queue -> case dropUntilDamage queue of
        dmsg : _ ->
          case dmsg of
            InvestigatorDamage iid' _ damage' horror' ->
              if iid' == iid then (damage', horror') else error "mismatch"
            InvestigatorDoAssignDamage iid' _ _ _ damage' horror' _ _ ->
              if iid' == iid then (damage', horror') else error "mismatch"
            _ -> error "mismatch"
        _ -> error "unhandled"
      let
        reuse = UseCardAbility iid source (n + 1) windows pay
        damageMsg = Label
          ("Cancel 1 damage out of " <> tshow damage)
          [CancelDamage iid 1, reuse]
        horrorMsg = Label
          ("Cancel 1 horror out of " <> tshow horror)
          [CancelHorror iid 1, reuse]
        doneMsg = Label
          ("Done (Take "
          <> tshow damage
          <> " damage and "
          <> tshow horror
          <> " horror)"
          )
          [Discard (toTarget attrs)]
      case (damage, horror) of
        (0, 0) -> e <$ push (Discard $ toTarget attrs)
        (_, 0) -> e <$ push (chooseOne iid [damageMsg, doneMsg])
        (0, _) -> e <$ push (chooseOne iid [horrorMsg, doneMsg])
        (_, _) -> e <$ push (chooseOne iid [damageMsg, horrorMsg, doneMsg])
    _ -> IveHadWorse4 <$> runMessage msg attrs
