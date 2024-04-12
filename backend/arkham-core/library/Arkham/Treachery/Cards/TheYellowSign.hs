module Arkham.Treachery.Cards.TheYellowSign where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Source
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheYellowSign = TheYellowSign TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theYellowSign :: TreacheryCard TheYellowSign
theYellowSign = treachery TheYellowSign Cards.theYellowSign

instance RunMessage TheYellowSign where
  runMessage msg t@(TheYellowSign attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #willpower (Fixed 4)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ _ -> do
      pushAll
        [ assignHorror iid attrs 2
        , search
            iid
            (toSource attrs)
            (toTarget iid)
            [fromDeck]
            (CardWithTrait Madness) -- TODO: We may need to specify weakness, candidate for card matcher
            (DrawFound iid 1)
        ]
      pure t
    _ -> TheYellowSign <$> runMessage msg attrs
