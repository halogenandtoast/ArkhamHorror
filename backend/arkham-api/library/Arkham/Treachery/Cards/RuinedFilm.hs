module Arkham.Treachery.Cards.RuinedFilm (ruinedFilm, RuinedFilm (..)) where

import Arkham.Matcher
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RuinedFilm = RuinedFilm TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruinedFilm :: TreacheryCard RuinedFilm
ruinedFilm = treachery RuinedFilm Cards.ruinedFilm

instance RunMessage RuinedFilm where
  runMessage msg t@(RuinedFilm attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      push $ DoStep 4 msg
      pure t
    DoStep n msg'@(Revelation iid (isSource attrs -> True)) | n > 0 -> do
      targets <-
        fold
          <$> sequence
            [ selectTargets $ assetControlledBy iid <> AssetWithTokens (atLeast 1) Evidence
            , selectTargets $ eventControlledBy iid <> EventWithToken Evidence
            , selectTargets $ skillControlledBy iid <> SkillWithToken Evidence
            , selectTargets $ InvestigatorWithId iid <> InvestigatorWithToken Evidence
            ]

      if null targets
        then assignHorror iid attrs n
        else
          chooseOne
            iid
            [ targetLabel target [RemoveTokens (toSource attrs) target Evidence 1, DoStep (n - 1) msg']
            | target <- targets
            ]

      pure t
    _ -> RuinedFilm <$> liftRunMessage msg attrs
