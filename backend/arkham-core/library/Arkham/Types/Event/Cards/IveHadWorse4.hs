module Arkham.Types.Event.Cards.IveHadWorse4
  ( iveHadWorse4
  , IveHadWorse4(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Event.Attrs
import Arkham.Types.Message

newtype IveHadWorse4 = IveHadWorse4 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

iveHadWorse4 :: EventCard IveHadWorse4
iveHadWorse4 = event IveHadWorse4 Cards.iveHadWorse4

instance HasActions IveHadWorse4
instance HasModifiersFor env IveHadWorse4

dropUntilDamage :: [Message] -> [Message]
dropUntilDamage = dropWhile (notElem DamageMessage . messageType)

instance RunMessage env IveHadWorse4 where
  runMessage msg e@(IveHadWorse4 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ push (UseCardAbility iid (toSource attrs) [] 0 NoPayment)
    UseCardAbility _ source _ 5 _ | isSource attrs source ->
      e <$ push (Discard $ toTarget attrs)
    UseCardAbility iid source meta n pay | isSource attrs source -> do
      (damage, horror) <- fromQueue $ \queue ->
        let dmsg : _ = dropUntilDamage queue
        in
          case dmsg of
            InvestigatorDamage iid' _ damage' horror' ->
              if iid' == iid then (damage', horror') else error "mismatch"
            InvestigatorDoAssignDamage iid' _ _ damage' horror' _ _ ->
              if iid' == iid then (damage', horror') else error "mismatch"
            _ -> error "mismatch"
      let
        reuse = UseCardAbility iid source meta (n + 1) pay
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
