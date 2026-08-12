module Arkham.Homebrew.DarkMatter.Locations.EscapePodBay (escapePodBay) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Window.Enemy (getEnemy)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EscapePodBay = EscapePodBay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

escapePodBay :: LocationCard EscapePodBay
escapePodBay = symbolLabel $ location EscapePodBay Cards.escapePodBay 3 (PerPlayer 1)

instance HasAbilities EscapePodBay where
  getAbilities (EscapePodBay a) =
    extendRevealed1 a
      $ limited (GroupLimit PerGame 3)
      $ restricted a 1 Here
      $ freeReaction
      $ EnemyEvadedSuccessfully #after You AnySource (EnemyAt $ be a)

instance RunMessage EscapePodBay where
  runMessage msg l@(EscapePodBay attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getEnemy -> eid) _ -> do
      defeatEnemy eid iid (attrs.ability 1)
      pure l
    _ -> EscapePodBay <$> liftRunMessage msg attrs
