module Arkham.Treachery.Cards.CoverUpAdvanced (CoverUpAdvanced (..), coverUpAdvanced) where

import Arkham.Ability
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Matcher qualified as Matcher
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype CoverUpAdvanced = CoverUpAdvanced TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coverUpAdvanced :: TreacheryCard CoverUpAdvanced
coverUpAdvanced = treacheryWith CoverUpAdvanced Cards.coverUpAdvanced (tokensL %~ setTokens Clue 4)

instance HasAbilities CoverUpAdvanced where
  getAbilities (CoverUpAdvanced a) =
    restrictedAbility
      a
      1
      (OnSameLocation <> CluesOnThis (atLeast 1))
      (freeReaction (Matcher.DiscoverClues #when You YourLocation $ atLeast 1))
      : [ restrictedAbility a 2 (CluesOnThis $ atLeast 1) $ forcedOnElimination iid
        | iid <- maybeToList a.owner
        ]

toClueCount :: [Window] -> Int
toClueCount = \case
  ((windowType -> Window.DiscoverClues _ _ _ n) : _) -> n
  _ -> error "Invalid call"

instance RunMessage CoverUpAdvanced where
  runMessage msg t@(CoverUpAdvanced attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (toClueCount -> n) _ -> do
      matchingDon't \case
        Do (DiscoverClues iid' _) -> iid == iid'
        _ -> False
      pure $ CoverUpAdvanced $ attrs & tokensL %~ subtractTokens Clue n
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withTreacheryInvestigator attrs $ \tormented -> push (SufferTrauma tormented 0 1)
      pure t
    _ -> CoverUpAdvanced <$> liftRunMessage msg attrs
