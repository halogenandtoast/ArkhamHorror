module Arkham.Homebrew.DarkMatter.Locations.EscapePodBay (escapePodBay) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype EscapePodBay = EscapePodBay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapePodBay :: LocationCard EscapePodBay
escapePodBay = symbolLabel $ location EscapePodBay Cards.escapePodBay 3 (PerPlayer 1)

{- | "[reaction] After you succeed at evading an enemy at this location: Defeat
that enemy. (Group limit 3 times per game.)"
-}
instance HasAbilities EscapePodBay where
  getAbilities (EscapePodBay a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ freeReaction
      $ EnemyEvadedSuccessfully #after You AnySource (EnemyAt $ be a)

getEvadedEnemy :: [Window] -> Maybe EnemyId
getEvadedEnemy = \case
  (windowType -> Window.EnemyEvaded _ eid) : _ -> Just eid
  _ : rest -> getEvadedEnemy rest
  [] -> Nothing

instance RunMessage EscapePodBay where
  runMessage msg l@(EscapePodBay attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEvadedEnemy -> Just eid) _ -> do
      defeatEnemy eid iid (attrs.ability 1)
      pure l
    _ -> EscapePodBay <$> liftRunMessage msg attrs
