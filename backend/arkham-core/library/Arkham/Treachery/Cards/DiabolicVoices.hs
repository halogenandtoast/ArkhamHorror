module Arkham.Treachery.Cards.DiabolicVoices (
  diabolicVoices,
  DiabolicVoices (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DiabolicVoices = DiabolicVoices TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diabolicVoices :: TreacheryCard DiabolicVoices
diabolicVoices = treachery DiabolicVoices Cards.diabolicVoices

instance RunMessage DiabolicVoices where
  runMessage msg t@(DiabolicVoices attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- length <$> findInDiscard (cardIs Cards.diabolicVoices)
      push $ revelationSkillTest iid attrs SkillWillpower (3 + n)
      pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget {} _ n | n > 0 -> do
      handCount <- fieldMap InvestigatorHand length iid
      player <- getPlayer iid
      pushAll
        $ toMessage (randomDiscardN iid attrs (min n handCount))
        : replicate
          (max 0 $ n - handCount)
          ( chooseOne
              player
              [ Label
                  "Take Damage"
                  [InvestigatorAssignDamage iid (toSource attrs) DamageAny 1 0]
              , Label
                  "Take Horror"
                  [InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1]
              ]
          )
      pure t
    _ -> DiabolicVoices <$> runMessage msg attrs
