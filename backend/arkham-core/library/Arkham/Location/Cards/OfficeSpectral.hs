module Arkham.Location.Cards.OfficeSpectral
  ( officeSpectral
  , OfficeSpectral(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Message

newtype OfficeSpectral = OfficeSpectral LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

officeSpectral :: LocationCard OfficeSpectral
officeSpectral = location OfficeSpectral Cards.officeSpectral 4 (PerPlayer 2)

instance HasAbilities OfficeSpectral where
  getAbilities (OfficeSpectral attrs) = withBaseAbilities
    attrs
    [haunted "Choose and discard a card from your hand." attrs 1]

instance RunMessage OfficeSpectral where
  runMessage msg l@(OfficeSpectral attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ ChooseAndDiscardCard iid (toSource attrs)
      pure l
    _ -> OfficeSpectral <$> runMessage msg attrs
