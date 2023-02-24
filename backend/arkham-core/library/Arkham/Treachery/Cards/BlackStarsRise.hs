module Arkham.Treachery.Cards.BlackStarsRise
  ( blackStarsRise
  , BlackStarsRise(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BlackStarsRise = BlackStarsRise TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackStarsRise :: TreacheryCard BlackStarsRise
blackStarsRise = treachery BlackStarsRise Cards.blackStarsRise

instance RunMessage BlackStarsRise where
  runMessage msg t@(BlackStarsRise attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillIntellect 4)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ n
      | isSource attrs source -> do
        hasAgenda <- selectAny AnyAgenda
        push $ chooseOrRunOne iid
          $ [ Label
                "Place 1 doom on current agenda. This effect can cause the current agenda to advance."
                [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
            | hasAgenda
            ]
          <> [ Label
                 ("Take " <> tshow n <> " horror")
                 [InvestigatorAssignDamage iid source DamageAny 0 n]
             ]
        pure t
    _ -> BlackStarsRise <$> runMessage msg attrs
