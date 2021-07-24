module Arkham.Types.Event.Cards.DecipheredReality5
  ( decipheredReality5
  , DecipheredReality5(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target

newtype DecipheredReality5 = DecipheredReality5 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decipheredReality5 :: EventCard DecipheredReality5
decipheredReality5 = event DecipheredReality5 Cards.decipheredReality5

instance HasActions env DecipheredReality5 where
  getActions iid window (DecipheredReality5 attrs) =
    getActions iid window attrs

instance HasModifiersFor env DecipheredReality5 where
  getModifiersFor _ (LocationTarget _) (DecipheredReality5 attrs) =
    pure [toModifier attrs AlternateSuccessfullInvestigation]
  getModifiersFor _ _ _ = pure []

instance
  ( HasCount Shroud env LocationId
  , HasSet RevealedLocationId env ()
  )
  => RunMessage env DecipheredReality5 where
  runMessage msg e@(DecipheredReality5 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == toId attrs -> do
      locationIds <- map unRevealedLocationId <$> getSetList ()
      maxShroud <-
        maximum . ncons 0 <$> traverse (fmap unShroud . getCount) locationIds
      e <$ pushAll
        [ skillTestModifier attrs SkillTestTarget (SetDifficulty maxShroud)
        , ChooseInvestigate iid (toSource attrs) False
        ]
    SuccessfulInvestigation iid _ _ -> do
      locationIds <- map unRevealedLocationId <$> getSetList ()
      e <$ pushAll
        ([ DiscoverCluesAtLocation iid lid 1 Nothing | lid <- locationIds ]
        <> [Discard (toTarget attrs)]
        )
    SkillTestEnds _ -> e <$ push (Discard $ toTarget attrs)
    _ -> DecipheredReality5 <$> runMessage msg attrs
