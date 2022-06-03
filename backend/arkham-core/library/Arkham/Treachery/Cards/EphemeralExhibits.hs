module Arkham.Treachery.Cards.EphemeralExhibits
  ( ephemeralExhibits
  , EphemeralExhibits(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype EphemeralExhibits = EphemeralExhibits TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ephemeralExhibits :: TreacheryCard EphemeralExhibits
ephemeralExhibits = treachery EphemeralExhibits Cards.ephemeralExhibits

instance TreacheryRunner env => RunMessage EphemeralExhibits where
  runMessage msg t@(EphemeralExhibits attrs) = case msg of
    Revelation iid source | isSource attrs source -> t <$ push
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillIntellect
        3
      )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> t <$ push (LoseActions iid source n)
    _ -> EphemeralExhibits <$> runMessage msg attrs
