module Arkham.Location.Cards.WhiteBluff (whiteBluff) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Scenarios.ToTheForbiddenPeaks.Helpers
import Arkham.Trait (Trait (Expedition))

newtype WhiteBluff = WhiteBluff LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whiteBluff :: LocationCard WhiteBluff
whiteBluff = locationWith WhiteBluff Cards.whiteBluff 3 (PerPlayer 1) (connectsToL .~ adjacentLocations)

instance HasModifiersFor WhiteBluff where
  getModifiersFor (WhiteBluff l) =
    whenUnrevealed l $ blockedWhenAny l $ leftOf l <> LocationWithAnyClues

instance HasAbilities WhiteBluff where
  getAbilities (WhiteBluff a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ forced
      $ Moves #after You AnySource (below a) (be a)

instance RunMessage WhiteBluff where
  runMessage msg l@(WhiteBluff attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      case attrs.row of
        Nothing -> error "should be in a row"
        Just x -> do
          sid <- getRandom
          beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed $ x + 1)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assets <- select $ assetControlledBy iid <> withTrait Expedition
      chooseTargetM iid assets (`place` attrs.id)
      pure l
    _ -> WhiteBluff <$> liftRunMessage msg attrs
