module Arkham.Treachery.Cards.BlackStarsRise (blackStarsRise, BlackStarsRise (..)) where

import Arkham.Classes
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BlackStarsRise = BlackStarsRise TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackStarsRise :: TreacheryCard BlackStarsRise
blackStarsRise = treachery BlackStarsRise Cards.blackStarsRise

instance RunMessage BlackStarsRise where
  runMessage msg t@(BlackStarsRise attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #intellect (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      hasAgenda <- selectAny AnyAgenda
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Place 1 doom on current agenda. This effect can cause the current agenda to advance."
            [placeDoomOnAgendaAndCheckAdvance]
          | hasAgenda
          ]
        <> [ Label
              ("Take " <> tshow n <> " horror")
              [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 n]
           ]
      pure t
    _ -> BlackStarsRise <$> runMessage msg attrs
