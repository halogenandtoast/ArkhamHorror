module Arkham.Treachery.Cards.ATearInTime (
  aTearInTime,
  ATearInTime (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (InvestigatorRemainingActions))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ATearInTime = ATearInTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aTearInTime :: TreacheryCard ATearInTime
aTearInTime = treachery ATearInTime Cards.aTearInTime

instance RunMessage ATearInTime where
  runMessage msg t@(ATearInTime attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ revelationSkillTest iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n | n > 0 -> do
      push $ DoStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hasRemainingActions <- fieldP InvestigatorRemainingActions (> 0) iid
      let source = toSource attrs
      player <- getPlayer iid
      pushAll
        [ chooseOrRunOne
            player
            $ [Label "Lose 1 Action" [LoseActions iid source 1] | hasRemainingActions]
            <> [Label "Take 1 Horror" [assignHorror iid source 1]]
        , DoStep (n - 1) msg'
        ]
      pure t
    _ -> ATearInTime <$> runMessage msg attrs
