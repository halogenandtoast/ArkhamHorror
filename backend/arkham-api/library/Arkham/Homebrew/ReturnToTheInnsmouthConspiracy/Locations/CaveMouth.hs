module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.CaveMouth (caveMouth) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.DevilReef qualified as DevilReefLocations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)
import Arkham.Trait (Trait (Ocean))

newtype CaveMouth = CaveMouth LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

caveMouth :: LocationCard CaveMouth
caveMouth = location CaveMouth Cards.caveMouth 4 (Static 0)

{- | "Cave Mouth is connected to all Ocean locations and vice versa." The "and vice
versa" is the designer's hard errata: without it you could never come back.

"If an enemy would spawn at Cave Mouth, it spawns at Churning Waters instead."
-}
instance HasModifiersFor CaveMouth where
  getModifiersFor (CaveMouth a) = whenRevealed a do
    modifySelf a [ConnectedToWhen (be a) (LocationWithTrait Ocean)]
    modifySelect a (LocationWithTrait Ocean) [ConnectedToWhen (LocationWithTrait Ocean) (be a)]
    {- The spawn code reads 'ChangeSpawnLocation' off the ENEMY's modifiers, so the
    redirect has to be published to enemies rather than to this location. -}
    modifySelect a AnyEnemy [ChangeSpawnLocation (be a) (locationIs DevilReefLocations.churningWaters)]

instance HasAbilities CaveMouth where
  getAbilities (CaveMouth a) =
    extendRevealed
      a
      [ restricted
          a
          1
          ( Here
              <> HasCalculation (InvestigatorKeyCountCalculation You) (atLeast 2)
              <> exists (location_ "Bootlegger's Hideaway")
          )
          (actionAbilityWithCost $ ClueCost (Static 1))
      ]

instance RunMessage CaveMouth where
  runMessage msg l@(CaveMouth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ RevealedLocation <> LocationWithTitle "Bootlegger's Hideaway"
      chooseTargetM iid ls $ moveTo (attrs.ability 1) iid
      pure l
    _ -> CaveMouth <$> liftRunMessage msg attrs
