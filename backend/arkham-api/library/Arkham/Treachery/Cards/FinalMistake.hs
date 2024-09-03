module Arkham.Treachery.Cards.FinalMistake (finalMistake, FinalMistake (..)) where

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype FinalMistake = FinalMistake TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

finalMistake :: TreacheryCard FinalMistake
finalMistake = treachery FinalMistake Cards.finalMistake

instance RunMessage FinalMistake where
  runMessage msg t@(FinalMistake attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      doom <- getSum <$> selectAgg Sum LocationDoom (locationWithInvestigator iid)
      sid <- getRandom
      pushAll
        [ skillTestModifier sid source (SkillTestTarget sid) (Difficulty doom)
        , revelationSkillTest sid iid source #agility (Fixed 2)
        ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      push $ assignDamage iid attrs 2
      pure t
    _ -> FinalMistake <$> runMessage msg attrs
