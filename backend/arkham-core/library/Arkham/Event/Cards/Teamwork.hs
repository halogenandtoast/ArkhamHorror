module Arkham.Event.Cards.Teamwork (
  teamwork,
  Teamwork (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Trait

newtype Teamwork = Teamwork EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

teamwork :: EventCard Teamwork
teamwork = event Teamwork Cards.teamwork

instance RunMessage Teamwork where
  runMessage msg e@(Teamwork attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid mtarget windows' _ | eid == eventId -> do
      push $ ResolveEvent iid eid mtarget windows'
      pure e
    ResolveEvent iid eid mtarget windows' | eid == eventId -> do
      investigators <- select $ colocatedWith iid
      assetsWithInvestigator <- concatForM investigators \investigator -> do
        selectMap (investigator,)
          $ assetControlledBy investigator
          <> oneOf (map AssetWithTrait [Ally, Item])
      player <- getPlayer iid
      let beginTrade iid' x = BeginTrade iid' (toSource attrs) x (investigators \\ [iid'])
      let resolveAgain = ResolveEvent iid eid mtarget windows'

      push
        $ chooseOne player
        $ Done "Done Trading"
        : [ targetLabel aid [beginTrade iid' (toTarget aid), resolveAgain]
          | (iid', aid) <- assetsWithInvestigator
          ]
          <> [ targetLabel iid' [beginTrade iid' (ResourceTarget iid), resolveAgain]
             | iid' <- investigators
             ]
      pure e
    _ -> Teamwork <$> runMessage msg attrs
