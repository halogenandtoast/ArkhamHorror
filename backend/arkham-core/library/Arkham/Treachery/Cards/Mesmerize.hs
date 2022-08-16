module Arkham.Treachery.Cards.Mesmerize
  ( mesmerize
  , Mesmerize(..)
  ) where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Classes
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Target
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Mesmerize = Mesmerize TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmerize :: TreacheryCard Mesmerize
mesmerize = treachery Mesmerize Cards.mesmerize

instance RunMessage Mesmerize where
  runMessage msg t@(Mesmerize attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mlid <- field InvestigatorLocation iid
      t <$ case mlid of
        Nothing -> push $ Surge iid (toSource attrs)
        Just lid -> do
          maskedCarnevaleGoers <- selectListMap
            AssetTarget
            (AssetAtLocation lid <> AssetWithTitle "Masked Carnevale-Goer")
          case maskedCarnevaleGoers of
            [] -> push $ chooseOne
              iid
              [TargetLabel (toTarget attrs) [Surge iid $ toSource attrs]]
            xs -> pushAll
              [ CreateEffect
                (toCardCode attrs)
                Nothing
                source
                (InvestigatorTarget iid)
              , chooseOne
                iid
                [ TargetLabel target [Flip iid source target] | target <- xs ]
              ]
    _ -> Mesmerize <$> runMessage msg attrs
