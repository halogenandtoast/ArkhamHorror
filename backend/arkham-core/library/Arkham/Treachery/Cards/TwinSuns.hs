module Arkham.Treachery.Cards.TwinSuns (
  twinSuns,
  TwinSuns (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TwinSuns = TwinSuns TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twinSuns :: TreacheryCard TwinSuns
twinSuns = treachery TwinSuns Cards.twinSuns

instance RunMessage TwinSuns where
  runMessage msg t@(TwinSuns attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      push $ RevelationSkillTest iid source SkillIntellect 4
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n
      | isSource attrs source -> do
          agenda <- selectJust AnyAgenda
          push $
            chooseOne
              iid
              [ Label
                  "Remove 1 doom from the current agenda"
                  [RemoveDoom (toSource attrs) (toTarget agenda) 1]
              , Label
                  "Take 1 horror for each point you failed by"
                  [InvestigatorAssignDamage iid source DamageAny 0 n]
              ]
          pure t
    _ -> TwinSuns <$> runMessage msg attrs
