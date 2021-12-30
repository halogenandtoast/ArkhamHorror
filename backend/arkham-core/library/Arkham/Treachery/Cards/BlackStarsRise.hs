module Arkham.Treachery.Cards.BlackStarsRise
  ( blackStarsRise
  , BlackStarsRise(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype BlackStarsRise = BlackStarsRise TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackStarsRise :: TreacheryCard BlackStarsRise
blackStarsRise = treachery BlackStarsRise Cards.blackStarsRise

instance TreacheryRunner env => RunMessage env BlackStarsRise where
  runMessage msg t@(BlackStarsRise attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillIntellect 4)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> t <$ push
        (chooseOne
          iid
          [ Label
            "Place 1 doom on current agenda. This effect can cause the current agenda to advance."
            [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
          , Label
            ("Take " <> tshow n <> " horror")
            [InvestigatorAssignDamage iid source DamageAny 0 n]
          ]
        )
    _ -> BlackStarsRise <$> runMessage msg attrs
