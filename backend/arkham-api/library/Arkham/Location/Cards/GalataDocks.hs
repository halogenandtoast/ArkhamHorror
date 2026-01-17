module Arkham.Location.Cards.GalataDocks (galataDocks) where

import Arkham.Cost
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GalataDocks = GalataDocks LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

galataDocks :: LocationCard GalataDocks
galataDocks = symbolLabel $ location GalataDocks Cards.galataDocks 5 (PerPlayer 1)

instance HasModifiersFor GalataDocks where
  getModifiersFor (GalataDocks attrs) = do
    modifySelf
      attrs
      [ AdditionalCostToEnter
          $ SameSkillIconCostMatching 3
          $ oneOf [InHandOf NotForPlay You, InPlayAreaOf You <> basic (not_ PermanentCard)]
      ]
    -- We include galata docks itself so the blocked icon does not show
    modifySelect
      attrs
      (not_ $ InvestigatorAt (oneOf [locationIs Cards.galata, locationIs Cards.galataDocks]))
      [CannotEnter attrs.id]

instance RunMessage GalataDocks where
  runMessage msg (GalataDocks attrs) = GalataDocks <$> runMessage msg attrs
