module Arkham.Event.Cards.Teamwork
  ( teamwork
  , Teamwork(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype Teamwork = Teamwork EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor m, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

teamwork :: EventCard Teamwork
teamwork = event Teamwork Cards.teamwork

-- | Resolve Teamwork Event
--
-- This event works a little special due to how the interactions work with
-- many players. It is not enough to use ChooseSome because resources can
-- be traded many times. Because of this we introduced the ResolveEvent
-- message which is meant to be an internal message inside events after they
-- have resolved and behavior needs to be handled.

instance RunMessage Teamwork where
  runMessage msg e@(Teamwork attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid mtarget _ _ | eid == eventId ->
      e <$ push (ResolveEvent iid eid mtarget)
    ResolveEvent iid eid mtarget | eid == eventId -> do
      investigatorIds <- selectList $ colocatedWith iid
      assetsWithInvestigatorIds <- concat <$> for
        investigatorIds
        (\investigatorId -> map (investigatorId, ) <$> selectList
          (AssetControlledBy (InvestigatorWithId investigatorId)
          <> AssetOneOf (map AssetWithTrait [Ally, Item])
          )
        )
      e <$ push
        (chooseOne
          iid
          (Done "Done Trading"
          : [ TargetLabel
                (AssetTarget aid)
                [ BeginTrade
                  iid'
                  (AssetTarget aid)
                  (investigatorIds \\ [iid'])
                , ResolveEvent iid eid mtarget
                ]
            | (iid', aid) <- assetsWithInvestigatorIds
            ]
          <> [ TargetLabel
                 (InvestigatorTarget iid')
                 [ BeginTrade iid' ResourceTarget (investigatorIds \\ [iid'])
                 , ResolveEvent iid eid mtarget
                 ]
             | iid' <- investigatorIds
             ]
          )
        )
    _ -> Teamwork <$> runMessage msg attrs
