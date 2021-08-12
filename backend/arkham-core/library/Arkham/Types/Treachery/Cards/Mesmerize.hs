module Arkham.Types.Treachery.Cards.Mesmerize
  ( mesmerize
  , Mesmerize(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Mesmerize = Mesmerize TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmerize :: TreacheryCard Mesmerize
mesmerize = treachery Mesmerize Cards.mesmerize

instance
  ( HasSet FarthestLocationId env (InvestigatorId, LocationMatcher)
  , TreacheryRunner env
  )
  => RunMessage env Mesmerize where
  runMessage msg t@(Mesmerize attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      maskedCarnevaleGoers <- selectList
        (AssetAtLocation lid <> AssetWithTitle "Masked Carnevale-Goer")
      case maskedCarnevaleGoers of
        [] ->
          t
            <$ pushAll
                 [ chooseOne iid [Surge iid $ toSource attrs]
                 , Discard $ toTarget attrs
                 ]
        xs -> do
          locationTargets <- map (LocationTarget . unFarthestLocationId)
            <$> getSetList (iid, LocationWithoutInvestigators)
          -- Since non-innocent revelers will no longer be assets, we can just queue
          -- the move and damage by targeting assets and the enemies will be untouched
          t <$ pushAll
            ([ Run
                 [ Flip source (AssetTarget aid)
                 , chooseOne
                   iid
                   [ TargetLabel locationTarget [AttachAsset aid locationTarget]
                   | locationTarget <- locationTargets
                   ]
                 , AssetDamage aid source 1 1
                 ]
             | aid <- xs
             ]
            <> [Discard $ toTarget attrs]
            )
    _ -> Mesmerize <$> runMessage msg attrs
