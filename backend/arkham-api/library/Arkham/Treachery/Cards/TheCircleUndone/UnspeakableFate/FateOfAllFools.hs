module Arkham.Treachery.Cards.TheCircleUndone.UnspeakableFate.FateOfAllFools (fateOfAllFools) where

import Arkham.Draw.Types
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.CardDefs.TheCircleUndone.UnspeakableFate qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FateOfAllFools = FateOfAllFools TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfAllFools :: TreacheryCard FateOfAllFools
fateOfAllFools = treachery FateOfAllFools Cards.fateOfAllFools

instance RunMessage FateOfAllFools where
  runMessage msg t@(FateOfAllFools attrs) = runQueueT $ case msg of
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
                        [ AddKeyword Peril
                        , EffectsCannotBeCanceled
                        , RevelationModifier (toSource attrs) (CannotCommitCards AnyCard)
                        ]
                }
      pure t
    _ -> FateOfAllFools <$> liftRunMessage msg attrs
