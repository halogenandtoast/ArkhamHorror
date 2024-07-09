module Arkham.Treachery.Cards.DeepDark (deepDark, DeepDark (..)) where

import Arkham.Ability
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.History
import Arkham.Matcher hiding (DiscoverClues)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Data.Map.Strict qualified as Map

newtype DeepDark = DeepDark TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepDark :: TreacheryCard DeepDark
deepDark = treachery DeepDark Cards.deepDark

instance HasModifiersFor DeepDark where
  getModifiersFor (LocationTarget _) (DeepDark a) = modified a [MaxCluesDiscovered 1]
  getModifiersFor (InvestigatorTarget iid) (DeepDark a) = do
    history <- getHistoryField RoundHistory iid HistoryCluesDiscovered
    let xs = Map.keys $ Map.filter (> 0) history
    modified a [CannotDiscoverCluesAt $ oneOf $ map LocationWithId xs | notNull xs]
  getModifiersFor _ _ = pure []

instance HasAbilities DeepDark where
  getAbilities (DeepDark a) =
    [limitedAbility (MaxPer Cards.deepDark PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage DeepDark where
  runMessage msg t@(DeepDark attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> DeepDark <$> liftRunMessage msg attrs
