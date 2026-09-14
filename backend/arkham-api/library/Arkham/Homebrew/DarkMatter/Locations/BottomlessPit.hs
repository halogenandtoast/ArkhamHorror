module Arkham.Homebrew.DarkMatter.Locations.BottomlessPit (bottomlessPit) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Investigator.Types (Field (InvestigatorRemainingActions))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Trait (Trait (Surface))

newtype BottomlessPit = BottomlessPit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bottomlessPit :: LocationCard BottomlessPit
bottomlessPit =
  symbolLabel
    $ locationWith BottomlessPit Cards.bottomlessPit 4 (Static 0) (canBeFlippedL .~ True)

{- | "Forced - After you enter Bottomless Pit: Move to any other non-[[Surface]]
location, take 3 direct damage and lose all remaining actions."
-}
instance HasAbilities BottomlessPit where
  getAbilities (BottomlessPit a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ Enters #after You (be a)

instance RunMessage BottomlessPit where
  runMessage msg l@(BottomlessPit attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      destinations <- select $ not_ (LocationWithTrait Surface) <> not_ (be attrs)
      chooseTargetM iid destinations $ moveTo (attrs.ability 1) iid
      directDamage iid (attrs.ability 1) 3
      remaining <- field InvestigatorRemainingActions iid
      when (remaining > 0) $ loseActions iid (attrs.ability 1) remaining
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      flipToOtherSide iid attrs
      pure l
    _ -> BottomlessPit <$> liftRunMessage msg attrs
