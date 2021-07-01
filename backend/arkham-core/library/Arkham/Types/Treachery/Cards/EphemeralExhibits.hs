module Arkham.Types.Treachery.Cards.EphemeralExhibits
  ( ephemeralExhibits
  , EphemeralExhibits(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype EphemeralExhibits = EphemeralExhibits TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ephemeralExhibits :: TreacheryCard EphemeralExhibits
ephemeralExhibits = treachery EphemeralExhibits Cards.ephemeralExhibits

instance HasModifiersFor env EphemeralExhibits where
  getModifiersFor = noModifiersFor

instance HasActions env EphemeralExhibits where
  getActions i window (EphemeralExhibits attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env EphemeralExhibits where
  runMessage msg t@(EphemeralExhibits attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ unshiftMessage
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillIntellect
        3
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> t <$ unshiftMessage (LoseActions iid source n)
    _ -> EphemeralExhibits <$> runMessage msg attrs
