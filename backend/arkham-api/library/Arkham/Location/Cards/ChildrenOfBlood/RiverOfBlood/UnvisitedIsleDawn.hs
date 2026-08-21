module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.UnvisitedIsleDawn (unvisitedIsleDawn) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelfWhen)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype UnvisitedIsleDawn = UnvisitedIsleDawn LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleDawn :: LocationCard UnvisitedIsleDawn
unvisitedIsleDawn =
  location UnvisitedIsleDawn Cards.unvisitedIsleDawn 3 (PerPlayer 3)
    & setLabel "unvisitedIsle"

instance HasModifiersFor UnvisitedIsleDawn where
  getModifiersFor (UnvisitedIsleDawn a) = do
    modifySelfWhen a a.revealed [AdditionalCostToLeave $ ActionCost 1]
    unless a.revealed do
      selectOne (locationIs Cards.riverDocksDawn) >>= traverse_ \docks -> do
        modifySelect a (enemyAt a) [HunterConnectedTo docks]
        modifySelect a (enemyAt docks) [HunterConnectedTo a.id]

instance HasAbilities UnvisitedIsleDawn where
  getAbilities (UnvisitedIsleDawn a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> NoCluesOnThis <> exists (InTokenPool #blood))
      $ FastAbility Free

instance RunMessage UnvisitedIsleDawn where
  runMessage msg l@(UnvisitedIsleDawn attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addChaosToken #blood
      remember TheInvestigatorsFoundASacrificialDagger
      pure l
    _ -> UnvisitedIsleDawn <$> liftRunMessage msg attrs
