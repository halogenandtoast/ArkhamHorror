module Arkham.Act.Cards.TracesToNowhere.TheCulprit (theCulprit) where

import Arkham.Act.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Act.Import.Lifted

newtype TheCulprit = TheCulprit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCulprit :: ActCard TheCulprit
theCulprit = act (3, A) TheCulprit Cards.theCulprit Nothing

instance RunMessage TheCulprit where
  runMessage msg a@(TheCulprit attrs) = runQueueT $ case msg of
    AdvanceAct aid _ _ | aid == attrs.id && onSide B attrs -> do
      advanceActDeck attrs
      pure a
    _ -> TheCulprit <$> liftRunMessage msg attrs
