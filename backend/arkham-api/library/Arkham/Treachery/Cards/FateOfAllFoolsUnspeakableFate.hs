module Arkham.Treachery.Cards.FateOfAllFoolsUnspeakableFate (fateOfAllFoolsUnspeakableFate) where

import Arkham.Draw.Types
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FateOfAllFoolsUnspeakableFate = FateOfAllFoolsUnspeakableFate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfAllFoolsUnspeakableFate :: TreacheryCard FateOfAllFoolsUnspeakableFate
fateOfAllFoolsUnspeakableFate = treachery FateOfAllFoolsUnspeakableFate Cards.fateOfAllFoolsUnspeakableFate

instance RunMessage FateOfAllFoolsUnspeakableFate where
  runMessage msg t@(FateOfAllFoolsUnspeakableFate attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      select (HasMatchingTreachery $ TreacheryWithTitle "Fate of All Fools") >>= \case
        [] -> placeInThreatArea attrs iid
        iids -> do
          chooseTargetM iid iids \iid' -> do
            drawEncounterCardEdit iid' attrs \d ->
              d
                { cardDrawRules =
                    singleton
                      $ WithDrawnCardModifiers
                        (toSource attrs)
                        [AddKeyword Peril, EffectsCannotBeCanceled, RevelationModifier (toSource attrs) (CannotCommitCards AnyCard)]
                }
      pure t
    _ -> FateOfAllFoolsUnspeakableFate <$> liftRunMessage msg attrs
