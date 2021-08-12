module Arkham.Types.Event.Cards.Teamwork
  ( teamwork
  , Teamwork(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait

newtype Teamwork = Teamwork EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

teamwork :: EventCard Teamwork
teamwork = event Teamwork Cards.teamwork

instance HasActions Teamwork
instance HasModifiersFor env Teamwork

-- | Resolve Teamwork Event
--
-- This event works a little special due to how the interactions work with
-- many players. It is not enough to use ChooseSome because resources can
-- be traded many times. Because of this we introduced the ResolveEvent
-- message which is meant to be an internal message inside events after they
-- have resolved and behavior needs to be handled.

instance
  ( HasQueue env
  , Query AssetMatcher env
  , HasSet InvestigatorId env LocationId
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env Teamwork where
  runMessage msg e@(Teamwork attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid mtarget _ | eid == eventId ->
      e <$ push (ResolveEvent iid eid mtarget)
    ResolveEvent iid eid mtarget | eid == eventId -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      assetsWithInvestigatorIds <- concat <$> for
        investigatorIds
        (\investigatorId -> map (investigatorId, ) <$> selectList
          (AssetOwnedBy (InvestigatorWithId investigatorId)
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
