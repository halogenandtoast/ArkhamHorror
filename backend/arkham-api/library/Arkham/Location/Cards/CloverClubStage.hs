module Arkham.Location.Cards.CloverClubStage (cloverClubStage) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype CloverClubStage = CloverClubStage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubStage :: LocationCard CloverClubStage
cloverClubStage = symbolLabel $ location CloverClubStage Cards.cloverClubStage 1 (Static 0)

instance HasModifiersFor CloverClubStage where
  getModifiersFor (CloverClubStage attrs) = do
    modifySelect attrs (locationIs Cards.cloverClubBar) [ConnectedToWhen Anywhere (be attrs)]

instance HasAbilities CloverClubStage where
  getAbilities (CloverClubStage a) =
    extendRevealed
      a
      [ restricted a 1 (Here <> OnAct 1) $ actionAbilityWithCost (ResourceCost 1)
      , groupLimit PerGame
          $ restricted
            a
            2
            (Here <> thisExists a (LocationWithResources $ AtLeast $ PerPlayer 1) <> youExist can.gain.resources)
          $ FastAbility Free
      ]

instance RunMessage CloverClubStage where
  runMessage msg l@(CloverClubStage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs #resource 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      gainClues iid (attrs.ability 2) =<< perPlayer 1
      pure l
    _ -> CloverClubStage <$> liftRunMessage msg attrs
