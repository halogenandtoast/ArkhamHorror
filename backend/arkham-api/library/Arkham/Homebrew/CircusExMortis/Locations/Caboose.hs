module Arkham.Homebrew.CircusExMortis.Locations.Caboose (caboose) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Caboose = Caboose LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caboose :: LocationCard Caboose
caboose = location Caboose Cards.caboose 2 (Static 1)

instance HasAbilities Caboose where
  getAbilities (Caboose a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 (Here <> notExists (EnemyAt (be a))) actionAbility

instance RunMessage Caboose where
  runMessage msg l@(Caboose attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamage iid (attrs.ability 1) 1
      pure l
    _ -> Caboose <$> liftRunMessage msg attrs
