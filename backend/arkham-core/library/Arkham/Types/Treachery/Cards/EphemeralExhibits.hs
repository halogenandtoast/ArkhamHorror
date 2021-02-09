module Arkham.Types.Treachery.Cards.EphemeralExhibits
  ( ephemeralExhibits
  , EphemeralExhibits(..)
  )
where


import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype EphemeralExhibits = EphemeralExhibits TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ephemeralExhibits :: TreacheryId -> a -> EphemeralExhibits
ephemeralExhibits uuid _ = EphemeralExhibits $ baseAttrs uuid "02145"

instance HasModifiersFor env EphemeralExhibits where
  getModifiersFor = noModifiersFor

instance HasActions env EphemeralExhibits where
  getActions i window (EphemeralExhibits attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env EphemeralExhibits where
  runMessage msg t@(EphemeralExhibits attrs@TreacheryAttrs {..}) = case msg of
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
