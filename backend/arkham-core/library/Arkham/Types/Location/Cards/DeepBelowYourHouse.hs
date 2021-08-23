module Arkham.Types.Location.Cards.DeepBelowYourHouse where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (deepBelowYourHouse)
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher hiding (RevealLocation)
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype DeepBelowYourHouse = DeepBelowYourHouse LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepBelowYourHouse :: LocationCard DeepBelowYourHouse
deepBelowYourHouse = location
  DeepBelowYourHouse
  Cards.deepBelowYourHouse
  4
  (PerPlayer 1)
  Squiggle
  [Plus]

instance HasModifiersFor env DeepBelowYourHouse

instance HasAbilities env DeepBelowYourHouse where
  getAbilities i window (DeepBelowYourHouse attrs) =
    getAbilities i window attrs

instance (LocationRunner env) => RunMessage env DeepBelowYourHouse where
  runMessage msg l@(DeepBelowYourHouse attrs) = case msg of
    RevealLocation (Just iid) lid | lid == locationId attrs -> do
      push
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
      | isSource attrs source
      -> l
        <$ pushAll
             (replicate
               n
               (FindAndDrawEncounterCard iid (CardWithCardCode "01159"))
             )
    _ -> DeepBelowYourHouse <$> runMessage msg attrs
