module Arkham.Homebrew.CircusExMortis.Locations.ManorCellars (manorCellars) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (modifySelectWhen, pattern CannotMoveTo)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ManorCellars = ManorCellars LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

manorCellars :: LocationCard ManorCellars
manorCellars = location ManorCellars Cards.manorCellars 2 (PerPlayer 1)

-- The ban is directional, so it rides on the investigators standing here rather
-- than as a CannotEnter on Savage Altar.
instance HasModifiersFor ManorCellars where
  getModifiersFor (ManorCellars a) =
    modifySelectWhen
      a
      (a.clues > 0)
      (InvestigatorAt (be a))
      [CannotMoveTo (locationIs Cards.savageAltar)]

instance HasAbilities ManorCellars where
  getAbilities (ManorCellars a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ be a <> LocationWithClues (atLeast 1))
      $ forced
      $ Moves #after You AnySource (locationIs Cards.savageAltar) (be a)

instance RunMessage ManorCellars where
  runMessage msg l@(ManorCellars attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      removeAllClues (attrs.ability 1) attrs
      pure l
    _ -> ManorCellars <$> liftRunMessage msg attrs
