module Arkham.Types.Location.Cards.DeepBelowYourHouse where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (deepBelowYourHouse)
import Arkham.Types.Card.CardMatcher
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype DeepBelowYourHouse = DeepBelowYourHouse LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepBelowYourHouse :: LocationId -> DeepBelowYourHouse
deepBelowYourHouse = DeepBelowYourHouse . baseAttrs
  Cards.deepBelowYourHouse
  4
  (PerPlayer 1)
  Squiggle
  [Plus]

instance HasModifiersFor env DeepBelowYourHouse where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env DeepBelowYourHouse where
  getActions i window (DeepBelowYourHouse attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env DeepBelowYourHouse where
  runMessage msg l@(DeepBelowYourHouse attrs) = case msg of
    RevealLocation (Just iid) lid | lid == locationId attrs -> do
      unshiftMessage
        (BeginSkillTest
          iid
          (toSource attrs)
          (InvestigatorTarget iid)
          Nothing
          SkillAgility
          3
        )
      DeepBelowYourHouse <$> runMessage msg attrs
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> l <$ unshiftMessages
        (replicate
          n
          (FindAndDrawEncounterCard iid (CardMatchByCardCode "01159"))
        )
    _ -> DeepBelowYourHouse <$> runMessage msg attrs
