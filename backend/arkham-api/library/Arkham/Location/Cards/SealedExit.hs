module Arkham.Location.Cards.SealedExit (sealedExit, SealedExit (..)) where

import Arkham.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype SealedExit = SealedExit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sealedExit :: LocationCard SealedExit
sealedExit = locationWith SealedExit Cards.sealedExit 5 (Static 0) connectsToAdjacent

instance HasAbilities SealedExit where
  getAbilities (SealedExit attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (Here <> exists (investigatorAt attrs <> InvestigatorWithKey GreenKey))
          $ ActionAbility [#resign] (ActionCost 1)
      , mkAbility attrs 2 $ forced $ RevealLocation #after Anyone (be attrs)
      ]

instance RunMessage SealedExit where
  runMessage msg l@(SealedExit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ InvestigatorDiscardAllClues (attrs.ability 1) iid
      forField InvestigatorKeys iid $ placeKey attrs
      push $ Resign iid
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure $ SealedExit $ attrs & floodLevelL ?~ FullyFlooded
    _ -> SealedExit <$> liftRunMessage msg attrs
