module Arkham.Location.Cards.TeetawnPassage (teetawnPassage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TeetawnPassage = TeetawnPassage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

teetawnPassage :: LocationCard TeetawnPassage
teetawnPassage =
  locationWith
    TeetawnPassage
    Cards.teetawnPassage
    0
    (Static 4)
    (shroudL ?~ PerPlayer 4)

instance HasModifiersFor TeetawnPassage where
  getModifiersFor (TeetawnPassage a) = do
    let n :: Int = sum $ map printedCardCost a.underneath
    modifySelfWhen a (n > 0) [ShroudModifier (-n)]

instance HasAbilities TeetawnPassage where
  getAbilities (TeetawnPassage a) =
    extendRevealed1 a
      $ limitedAbility (MaxPer Cards.teetawnPassage PerGame 1)
      $ fastAbility a 1 Free (Here <> thisExists a LocationWithoutClues)

instance RunMessage TeetawnPassage where
  runMessage msg l@(TeetawnPassage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      heliosTelescope <- selectJust $ assetIs Assets.heliosTelescopeGateToTheCosmos
      placeTokens (attrs.ability 1) heliosTelescope Shard 1
      pure l
    _ -> TeetawnPassage <$> liftRunMessage msg attrs
