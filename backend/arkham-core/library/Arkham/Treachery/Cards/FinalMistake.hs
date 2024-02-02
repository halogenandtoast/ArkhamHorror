module Arkham.Treachery.Cards.FinalMistake (
  finalMistake,
  FinalMistake (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Modifiers
import Arkham.Location.Types
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype FinalMistake = FinalMistake TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

finalMistake :: TreacheryCard FinalMistake
finalMistake = treachery FinalMistake Cards.finalMistake

instance RunMessage FinalMistake where
  runMessage msg t@(FinalMistake attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      doom <-
        getSum
          <$> selectAgg Sum LocationDoom (locationWithInvestigator iid)
      pushAll
        [ skillTestModifier source SkillTestTarget (Difficulty doom)
        , RevelationSkillTest iid source SkillAgility 2
        ]
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _ -> do
      push $ InvestigatorAssignDamage iid source DamageAny 2 0
      pure t
    _ -> FinalMistake <$> runMessage msg attrs
