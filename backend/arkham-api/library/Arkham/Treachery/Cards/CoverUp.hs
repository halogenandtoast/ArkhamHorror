module Arkham.Treachery.Cards.CoverUp (coverUp) where

import Arkham.Ability
import Arkham.Helpers.Window (discoveredClues)
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Matcher qualified as Matcher
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CoverUp = CoverUp TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coverUp :: TreacheryCard CoverUp
coverUp = treacheryWith CoverUp Cards.coverUp (tokensL %~ setTokens Clue 3)

instance HasAbilities CoverUp where
  getAbilities (CoverUp a) = case a.owner of
    Nothing -> []
    Just iid ->
      [ restricted
          a
          1
          (OnSameLocation <> CluesOnThis (atLeast 1))
          (freeReaction (Matcher.DiscoverClues #when You YourLocation $ atLeast 1))
      , restricted a 2 (CluesOnThis $ atLeast 1) $ forcedOnElimination iid
      ]

instance RunMessage CoverUp where
  runMessage msg t@(CoverUp attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (discoveredClues -> n) _ -> do
      matchingDon't \case
        Do (DiscoverClues iid' _) -> iid == iid'
        _ -> False
      pure $ CoverUp $ attrs & tokensL %~ subtractTokens Clue n
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withTreacheryInvestigator attrs \tormented -> sufferMentalTrauma tormented 1
      pure t
    _ -> CoverUp <$> liftRunMessage msg attrs
