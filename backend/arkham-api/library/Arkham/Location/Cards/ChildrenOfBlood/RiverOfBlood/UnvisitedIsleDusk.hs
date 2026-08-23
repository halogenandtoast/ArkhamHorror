module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.UnvisitedIsleDusk (unvisitedIsleDusk) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelfWhen)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype UnvisitedIsleDusk = UnvisitedIsleDusk LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unvisitedIsleDusk :: LocationCard UnvisitedIsleDusk
unvisitedIsleDusk =
  location UnvisitedIsleDusk Cards.unvisitedIsleDusk 3 (PerPlayer 3)
    & setLabel "unvisitedIsle"

instance HasModifiersFor UnvisitedIsleDusk where
  getModifiersFor (UnvisitedIsleDusk a) = do
    modifySelfWhen a a.revealed [AdditionalCostToLeave $ ActionCost 1]
    unless a.revealed do
      selectOne (locationIs Cards.riverDocksDusk) >>= traverse_ \docks -> do
        modifySelect a (enemyAt a) [HunterConnectedTo docks]
        modifySelect a (enemyAt docks) [HunterConnectedTo a.id]

instance HasAbilities UnvisitedIsleDusk where
  getAbilities (UnvisitedIsleDusk a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> NoCluesOnThis)
      $ FastAbility (AddTokenCost 2 #blood)

instance RunMessage UnvisitedIsleDusk where
  runMessage msg l@(UnvisitedIsleDusk attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember TheInvestigatorsFoundASacrificialDagger
      pure l
    _ -> UnvisitedIsleDusk <$> liftRunMessage msg attrs
