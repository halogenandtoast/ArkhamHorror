module Arkham.Location.Cards.BrackishWaters (BrackishWaters (..), brackishWaters) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype BrackishWaters = BrackishWaters LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brackishWaters :: LocationCard BrackishWaters
brackishWaters = location BrackishWaters Cards.brackishWaters 1 (Static 0)

instance HasModifiersFor BrackishWaters where
  getModifiersFor (InvestigatorTarget iid) (BrackishWaters attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs [CannotPlay #asset | here]
  getModifiersFor _ _ = pure []

instance HasAbilities BrackishWaters where
  getAbilities (BrackishWaters attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (Here <> notExists (assetIs Assets.fishingNet))
          $ actionAbilityWithCost
          $ DiscardFromCost 2 (FromHandOf You <> FromPlayAreaOf You) #asset
      ]

instance RunMessage BrackishWaters where
  runMessage msg l@(BrackishWaters attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      fishingNet <- getSetAsideCard Assets.fishingNet
      push $ TakeControlOfSetAsideAsset iid fishingNet
      pure l
    _ -> BrackishWaters <$> runMessage msg attrs
