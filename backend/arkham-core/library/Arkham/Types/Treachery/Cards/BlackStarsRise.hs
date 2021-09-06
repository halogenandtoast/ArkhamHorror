module Arkham.Types.Treachery.Cards.BlackStarsRise
  ( blackStarsRise
  , BlackStarsRise(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

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
