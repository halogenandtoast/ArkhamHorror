module Arkham.Homebrew.DarkMatter.Locations.EngineRoomTatterdemalion (engineRoomTatterdemalion) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype EngineRoomTatterdemalion = EngineRoomTatterdemalion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineRoomTatterdemalion :: LocationCard EngineRoomTatterdemalion
engineRoomTatterdemalion =
  location EngineRoomTatterdemalion Cards.engineRoomTatterdemalion 4 (PerPlayer 1)

{- | "Forced - At the end of your turn, if you are at the Engine Room and do not
control Radiation Tablets: Take 1 damage."
-}
instance HasAbilities EngineRoomTatterdemalion where
  getAbilities (EngineRoomTatterdemalion a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> not_ (exists $ assetIs Assets.radiationTablets <> AssetControlledBy You))
      $ forced
      $ TurnEnds #when You

instance RunMessage EngineRoomTatterdemalion where
  runMessage msg l@(EngineRoomTatterdemalion attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure l
    _ -> EngineRoomTatterdemalion <$> liftRunMessage msg attrs
