module Arkham.Treachery.Cards.CrisisOfFaith (
  crisisOfFaith,
  CrisisOfFaith (..),
)
where

import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CrisisOfFaith = CrisisOfFaith TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crisisOfFaith :: TreacheryCard CrisisOfFaith
crisisOfFaith = treachery CrisisOfFaith Cards.crisisOfFaith

instance RunMessage CrisisOfFaith where
  runMessage msg t@(CrisisOfFaith attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      n <- selectCount $ ChaosTokenFaceIs BlessToken
      push $ DoStep n msg
      pure t
    DoStep 0 (Revelation _ (isSource attrs -> True)) -> do
      pure t
    DoStep n msg'@(Revelation iid (isSource attrs -> True)) -> do
      x <- getRemainingCurseTokens
      chooseOrRunOne
        iid
        [ Label "Replace bless token with curse" [SwapChaosToken BlessToken CurseToken | x > 0]
        , Label "Take 1 horror" [Msg.assignHorror iid attrs 1]
        ]
      push $ DoStep (n - 1) msg'
      pure t
    _ -> CrisisOfFaith <$> liftRunMessage msg attrs
