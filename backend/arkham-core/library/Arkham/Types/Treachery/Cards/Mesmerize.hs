module Arkham.Types.Treachery.Cards.Mesmerize
  ( mesmerize
  , Mesmerize(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype Mesmerize = Mesmerize TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mesmerize :: TreacheryCard Mesmerize
mesmerize = treachery Mesmerize Cards.mesmerize

instance TreacheryRunner env => RunMessage env Mesmerize where
  runMessage msg t@(Mesmerize attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      maskedCarnevaleGoers <- selectListMap
        AssetTarget
        (AssetAtLocation lid <> AssetWithTitle "Masked Carnevale-Goer")
      case maskedCarnevaleGoers of
        [] -> t <$ push (chooseOne iid [Surge iid $ toSource attrs])
        xs -> t <$ pushAll
          [ CreateEffect
            (toCardCode attrs)
            Nothing
            source
            (InvestigatorTarget iid)
          , chooseOne
            iid
            [ TargetLabel target [Flip source target] | target <- xs ]
          ]
    _ -> Mesmerize <$> runMessage msg attrs
